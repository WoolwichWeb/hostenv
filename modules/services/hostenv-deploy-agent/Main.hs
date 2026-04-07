module Main where

import Hostenv.Provider.DeployAgent.Config (loadConfig)
import Hostenv.Provider.DeployAgent.Supervisor (runBootstrap)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  configPath <-
    case args of
      ["--config", path] -> pure path
      _ -> dieWithUsage
  cfg <- loadConfig configPath
  runBootstrap cfg

dieWithUsage :: IO a
dieWithUsage = do
  hPutStrLn stderr "usage: hostenv-deploy-agent --config <path>"
  exitFailure
