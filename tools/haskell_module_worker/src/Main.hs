{-# LANGUAGE LambdaCase #-}
module Main where

import Compile (runSession, compile)
import Control.Exception (try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word64)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import Options (parseArgs)
import System.Environment (getArgs)
import System.IO (Handle, hPrint, stderr, stdout)
import System.IO.Error (alreadyInUseErrorType, ioeGetErrorType)

main :: IO ()
main = do
    (args, memoryAllowance) <- getArgs >>= parseArgs
    stdout_dup <- redirectStdoutToStderr
    st <- runSession $ do
      st <- compile args 0
      liftIO $ terminateIfUsingTooMuchMemory memoryAllowance
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

-- | Terminates the worker if it exceeds the memory allowance
terminateIfUsingTooMuchMemory :: Word64 -> IO ()
terminateIfUsingTooMuchMemory memoryAllowance = do
    statsEnabled <- getRTSStatsEnabled
    unless statsEnabled $
      error "terminateIfUsingTooMuchMemory: worker built without -with-rtsopts=-T"
    stats <- getRTSStats
    when (max_live_bytes stats > memoryAllowance * 1024 * 1024) $
      error $
        "terminateIfUsingTooMuchMemory: worker reached the memory threshold of "
        ++ show memoryAllowance ++ " MB"
