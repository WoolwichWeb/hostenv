{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Command
  ( runCommand
  , runCommandWithEnv
  , runCommandOrDie
  , renderCommand
  , commandErrorText
  , exitCodeToInt
  ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

import Hostenv.Provider.Config (AppConfig)
import Hostenv.Provider.Service (CommandError, CommandOutput(..), CommandRunner, CommandSpec(..))
import qualified Hostenv.Provider.Service as Service

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
          }
  (code, out, err) <- readCreateProcessWithExitCode process ""
  case code of
    ExitSuccess -> pure (Right (CommandOutput (T.pack out) (T.pack err)))
    _ -> pure (Left (Service.CommandError spec code (T.pack out) (T.pack err)))

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
