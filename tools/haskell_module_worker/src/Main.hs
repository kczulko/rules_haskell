{-# LANGUAGE LambdaCase #-}
module Main where

import Compile (Status(..), runSession, compile)
import Control.Exception (try)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Data.Word (Word64)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import Options (Options(..), parseArgs)
import ProtoClient (WorkRequest(..), createProtoClient, readWorkRequest, writeWorkResponse)
import System.Environment (getArgs)
import System.IO (Handle, hSetBinaryMode, stdin, stderr, stdout)
import System.IO.Error (alreadyInUseErrorType, ioeGetErrorType)

main :: IO ()
main = do
    opts <- getArgs >>= parseArgs
    stdout_dup <- redirectStdoutToStderr
    hSetBinaryMode stdin True
    hSetBinaryMode stdout_dup True
    pc <- createProtoClient stdin stdout_dup
    runSession $ (if optPersist opts then forever else id) $ do
      wr <- liftIO $ readWorkRequest pc
      st <- compile (wrArgs wr) (wrVerbosity wr)
      liftIO $ writeWorkResponse pc (statusExitCode st) (statusOutput st)
      liftIO $ when (optPersist opts) $
        terminateIfUsingTooMuchMemory (optMemoryAllowance opts)
  where
    statusExitCode = \case { Succeeded{} -> 0; _ -> 1 }
    statusOutput = \case
      Succeeded logs -> unlines $ intersperse "" logs
      CompileErrors logs errs -> unlines $ intersperse "" $ logs ++ errs
      NonHaskellInputs files ->
        unlines $ "haskell_module_worker error: non-haskell inputs:" : files

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
