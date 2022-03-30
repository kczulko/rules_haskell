{-# LANGUAGE LambdaCase #-}
module Main where

import Compile (runSession, compile)
import Control.Exception (try)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Environment (getArgs)
import System.IO (Handle, hPrint, stderr, stdout)
import System.IO.Error (alreadyInUseErrorType, ioeGetErrorType)

main :: IO ()
main = do
    stdout_dup <- redirectStdoutToStderr
    args <- getArgs
    st <- runSession $ do
      st <- compile args 0
      liftIO $ terminateIfUsingTooMuchMemory
      return st
    hPrint stdout_dup st

-- Redirecting stdout to stderr trick is, albeit convenient, fragile under
-- heavy parallelism https://gitlab.haskell.org/ghc/ghc/issues/16819
-- it fails, e.g., when Bazel spawns multiple workers while running
-- the test suite. Therefore, we retry here a few times.
redirectStdoutToStderr :: IO Handle
redirectStdoutToStderr = do
    stdout_dup <- hDuplicate stdout
    redirectRetrying 10
    return stdout_dup
  where
    redirectRetrying :: Int -> IO ()
    redirectRetrying 0 = error "redirectStdoutToStderr: failed to redirect stdout"
    redirectRetrying i = try (hDuplicateTo stderr stdout) >>= \case
      Left e | ioeGetErrorType e == alreadyInUseErrorType ->
        redirectRetrying (i - 1)
      _ ->
        return ()
