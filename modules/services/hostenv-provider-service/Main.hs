{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hostenv.Provider.Command (runCommandOrDie)
import Hostenv.Provider.Config (appWorkDir, loadConfig)
import Hostenv.Provider.DB (ensureSchema)
import Hostenv.Provider.Project (syncFlakeFromDb)
import Hostenv.Provider.Repo (ensureGitConfig, ensureProviderRepo)
import Hostenv.Provider.Server (runServer)
import Hostenv.Provider.Service (CommandSpec (..))

main :: IO ()
main = do
    cfg <- loadConfig
    ensureProviderRepo cfg
    ensureGitConfig cfg
    ensureSchema cfg
    syncFlakeFromDb cfg
    runCommandOrDie cfg (CommandSpec "nix" ["flake", "update"] (appWorkDir cfg))
    runServer cfg
