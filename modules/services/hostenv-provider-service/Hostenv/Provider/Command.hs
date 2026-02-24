{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hostenv.Provider.Command
  ( runCommand
  , runCommandWithEnv
  , CommandStream(..)
  , withCommandLineLogger
  , runCommandOrDie
  , renderCommand
  , commandErrorText
  , exitCodeToInt
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, finally, handle)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (void)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (Handle, hClose, hIsEOF, hPutStrLn, stderr)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc, waitForProcess)
import System.IO.Unsafe (unsafePerformIO)

import Hostenv.Provider.Config (AppConfig)
import Hostenv.Provider.Service (CommandError, CommandOutput(..), CommandRunner, CommandSpec(..))
import qualified Hostenv.Provider.Service as Service

data CommandStream
  = CommandStdout
  | CommandStderr
  deriving (Eq, Show)

type CommandLineLogger = CommandStream -> T.Text -> IO ()

commandLineLoggerRef :: IORef (Maybe CommandLineLogger)
{-# NOINLINE commandLineLoggerRef #-}
commandLineLoggerRef = unsafePerformIO (newIORef Nothing)

runCommand :: AppConfig -> CommandRunner
runCommand cfg = runCommandWithEnv cfg []

runCommandWithEnv :: AppConfig -> [(T.Text, T.Text)] -> CommandRunner
runCommandWithEnv _ envOverrides spec = do
  let cmd = spec.cmdName
  let args = spec.cmdArgs
  let cwd = spec.cmdCwd
  baseEnv <- getEnvironment
  let mergedEnv = applyEnvOverrides baseEnv envOverrides
  let process =
        (proc (T.unpack cmd) (map T.unpack args))
          { cwd = Just cwd
          , env = Just mergedEnv
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
  (_, maybeStdout, maybeStderr, ph) <- createProcess process
  outRef <- newIORef ([] :: [T.Text])
  errRef <- newIORef ([] :: [T.Text])
  doneStdout <- newEmptyMVar
  doneStderr <- newEmptyMVar
  case maybeStdout of
    Nothing -> putMVar doneStdout ()
    Just stdoutHandle ->
      void (forkIO (readCommandLines CommandStdout stdoutHandle (\line -> appendLine outRef line) `finally` putMVar doneStdout ()))
  case maybeStderr of
    Nothing -> putMVar doneStderr ()
    Just stderrHandle ->
      void (forkIO (readCommandLines CommandStderr stderrHandle (\line -> appendLine errRef line) `finally` putMVar doneStderr ()))
  code <- waitForProcess ph
  takeMVar doneStdout
  takeMVar doneStderr
  out <- renderLines <$> readIORef outRef
  err <- renderLines <$> readIORef errRef
  case code of
    ExitSuccess -> pure (Right (CommandOutput out err))
    _ -> pure (Left (Service.CommandError spec code out err))

withCommandLineLogger :: (CommandStream -> T.Text -> IO ()) -> IO a -> IO a
withCommandLineLogger logger action =
  bracket
    (atomicModifyIORef' commandLineLoggerRef (\current -> (Just logger, current)))
    (writeIORef commandLineLoggerRef)
    (\_ -> action)

runCommandOrDie :: AppConfig -> CommandSpec -> IO ()
runCommandOrDie cfg spec = do
  res <- runCommand cfg spec
  case res of
    Right _ -> pure ()
    Left err -> do
      hPutStrLn stderr ("hostenv-provider-service: " <> T.unpack (renderCommand err.errSpec))
      hPutStrLn stderr (T.unpack err.errStderr)
      exitFailure

renderCommand :: CommandSpec -> T.Text
renderCommand spec = T.unwords (spec.cmdName : spec.cmdArgs)

commandErrorText :: CommandError -> T.Text
commandErrorText cmdErr =
  T.unlines
    [ "Command failed: " <> renderCommand cmdErr.errSpec
    , "Exit: " <> T.pack (show cmdErr.errExit)
    , "stdout:\n" <> cmdErr.errStdout
    , "stderr:\n" <> cmdErr.errStderr
    ]

exitCodeToInt :: ExitCode -> Maybe Int
exitCodeToInt ExitSuccess = Just 0
exitCodeToInt (ExitFailure n) = Just n

applyEnvOverrides :: [(String, String)] -> [(T.Text, T.Text)] -> [(String, String)]
applyEnvOverrides base overrides =
  let baseMap = M.fromList base
      overrideMap = M.fromList (map (\(k, v) -> (T.unpack k, T.unpack v)) overrides)
      merged = M.union overrideMap baseMap
   in L.sortOn fst (M.toList merged)

appendLine :: IORef [T.Text] -> T.Text -> IO ()
appendLine ref line = atomicModifyIORef' ref (\linesSoFar -> (linesSoFar <> [line], ()))

renderLines :: [T.Text] -> T.Text
renderLines linesSoFar =
  if null linesSoFar
    then ""
    else T.unlines linesSoFar

readCommandLines :: CommandStream -> Handle -> (T.Text -> IO ()) -> IO ()
readCommandLines stream streamHandle onLine =
  handle (\(_ :: SomeException) -> hClose streamHandle) loop
  where
    loop = do
      eof <- hIsEOF streamHandle
      if eof
        then hClose streamHandle
        else do
          line <- TIO.hGetLine streamHandle
          onLine line
          emitCommandLine stream line
          loop

emitCommandLine :: CommandStream -> T.Text -> IO ()
emitCommandLine stream line = do
  logger <- readIORef commandLineLoggerRef
  case logger of
    Nothing -> pure ()
    Just callback -> callback stream line
