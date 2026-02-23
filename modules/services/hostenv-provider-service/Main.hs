{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import qualified Data.Text as T
import Hostenv.Provider.Config (loadConfig)
import Hostenv.Provider.DB (ensureSchema, syncUsers)
import Hostenv.Provider.Project (syncFlakeFromDb)
import Hostenv.Provider.Repo (RepoStatus(..), ensureProviderRepo)
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
    repoStatusResult <- ensureProviderRepo cfg
    repoStatus <- case repoStatusResult of
      Left err -> dieWith (T.unpack err)
      Right status -> pure status
    ensureSchema cfg
    syncUsers cfg
    when (repoStatus == RepoReady) $ do
      syncResult <- syncFlakeFromDb cfg
      case syncResult of
        Left err -> hPutStrLn stderr ("Provider repository startup sync skipped:\n" <> T.unpack err)
        Right _ -> pure ()
    runServer cfg repoStatus

dieWithUsage :: IO a
dieWithUsage = do
  hPutStrLn stderr "usage: hostenv-provider-service --config <path>"
  exitFailure

dieWith :: String -> IO a
dieWith msg = do
  hPutStrLn stderr msg
  exitFailure
