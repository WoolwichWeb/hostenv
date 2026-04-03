module Hostenv.Provider.DeployAgent.Executor.Process
  ( ProcessResult(..)
  , ProcessRunner
  , ProcessSpec(..)
  , runProcessIO
  , trimStderrSummary
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Exception (SomeException, catch)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (ExitCode(..))
import System.Environment (getEnvironment)
import System.IO (Handle)
import System.Process
  ( CreateProcess(create_group, cwd, env, std_err, std_in, std_out)
  , StdStream(CreatePipe, NoStream)
  , createProcess
  , interruptProcessGroupOf
  , proc
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)

data ProcessSpec = ProcessSpec
  { description :: Text
  , executable :: FilePath
  , arguments :: [String]
  , environment :: [(String, String)]
  , workingDirectory :: Maybe FilePath
  , timeoutSeconds :: Maybe Int
  }
  deriving (Eq, Show)

data ProcessResult = ProcessResult
  { exitCode :: Int
  , stdoutText :: Text
  , stderrText :: Text
  }
  deriving (Eq, Show)

type ProcessRunner m = ProcessSpec -> m ProcessResult

runProcessIO :: ProcessRunner IO
runProcessIO spec = do
  mergedEnvironment <- buildEnvironment spec.environment
  let process =
        (proc spec.executable spec.arguments)
          { cwd = spec.workingDirectory
          , env = Just mergedEnvironment
          , std_in = NoStream
          , std_out = CreatePipe
          , std_err = CreatePipe
          , create_group = True
          }
  (_, mStdout, mStderr, processHandle) <- createProcess process
  case (mStdout, mStderr) of
    (Just stdoutHandle, Just stderrHandle) -> do
      stdoutReader <- async (readHandle stdoutHandle)
      stderrReader <- async (readHandle stderrHandle)
      mExitCode <- case spec.timeoutSeconds of
        Nothing -> Just <$> waitForProcess processHandle
        Just seconds -> timeout (seconds * 1000000) (waitForProcess processHandle)
      case mExitCode of
        Just rawExitCode -> do
          stdoutValue <- wait stdoutReader
          stderrValue <- wait stderrReader
          pure
            ProcessResult
              { exitCode = fromExitCode rawExitCode
              , stdoutText = stdoutValue
              , stderrText = stderrValue
              }
        Nothing -> do
          terminateProcess processHandle
          interruptProcessGroupOf processHandle `catch` ignoreProcessException
          _ <- waitForProcess processHandle `catch` ignoreExitException
          stdoutValue <- wait stdoutReader
          stderrValue <- wait stderrReader
          pure
            ProcessResult
              { exitCode = 124
              , stdoutText = stdoutValue
              , stderrText = if T.null stderrValue then "process timed out" else stderrValue
              }
    _ -> pure timedOutResult
  where
    timedOutResult =
      ProcessResult
        { exitCode = 124
        , stdoutText = ""
        , stderrText = "process timed out"
        }

trimStderrSummary :: Int -> Text -> Text
trimStderrSummary maxLines raw =
  let ls = T.lines raw
      kept = if length ls <= maxLines then ls else drop (length ls - maxLines) ls
   in T.intercalate "\n" kept

fromExitCode :: ExitCode -> Int
fromExitCode = \case
  ExitSuccess -> 0
  ExitFailure code -> code

buildEnvironment :: [(String, String)] -> IO [(String, String)]
buildEnvironment overrides = do
  baseEnv <- getEnvironment
  let asMap = foldl' insertVar (Map.fromList baseEnv) overrides
  pure (Map.toList asMap)
  where
    insertVar :: Map String String -> (String, String) -> Map String String
    insertVar acc (key, value) = Map.insert key value acc

readHandle :: Handle -> IO Text
readHandle = T.hGetContents

ignoreProcessException :: SomeException -> IO ()
ignoreProcessException _ = pure ()

ignoreExitException :: SomeException -> IO ExitCode
ignoreExitException _ = pure (ExitFailure 124)
