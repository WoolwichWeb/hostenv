{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hostenv.Provider.Config (loadConfig)
import Hostenv.Provider.DB (ensureSchema, syncUsers)
import Hostenv.Provider.Project (syncFlakeFromDb)
import Hostenv.Provider.Repo (ensureProviderRepo)
import Hostenv.Provider.Server (runServer)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    configPath <- case args of
      ["--config", path] -> pure path
      _ -> dieWithUsage
    cfg <- loadConfig configPath
    ensureProviderRepo cfg
    ensureSchema cfg
    syncUsers cfg
    syncFlakeFromDb cfg
    runServer cfg

dieWithUsage :: IO a
dieWithUsage = do
  hPutStrLn stderr "usage: hostenv-provider-service --config <path>"
  exitFailure
