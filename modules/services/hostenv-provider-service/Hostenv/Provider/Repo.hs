{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Repo
  ( RepoStatus(..)
  , ensureProviderRepo
  , bootstrapProviderRepo
  , ensureGitConfig
  , openUnixSocket
  ) where

import Control.Exception (SomeException, finally, try)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Environment (setEnv)
import System.FilePath ((</>), takeDirectory)
import System.Posix.Files (setFileMode)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), SockAddr (SockAddrUnix), bind, defaultProtocol, listen, socket)

import Hostenv.Provider.Command (commandErrorText, runCommandWithEnv)
import Hostenv.Provider.Config (AppConfig(..), appWorkDir)
import Hostenv.Provider.Service (CommandSpec(..), renderGitCredentials)
import Hostenv.Provider.Util (randomToken)

data RepoStatus
  = RepoReady
  | RepoMissing
  deriving (Eq, Show)

ensureProviderRepo :: AppConfig -> IO (Either T.Text RepoStatus)
ensureProviderRepo cfg =
  let AppConfig { appDataDir = dataDir } = cfg
   in do
      dataDirExists <- doesDirectoryExist dataDir
      if not dataDirExists
        then do
          createDirectoryIfMissing True (takeDirectory dataDir)
          pure (Right RepoMissing)
        else do
          hasGitDir <- doesDirectoryExist (dataDir </> ".git")
          if not hasGitDir
            then pure (Left "dataDir exists but is not a git checkout. Delete it and retry.")
            else do
              worktreeResult <- ensureRepoWorktree cfg
              case worktreeResult of
                Left err -> pure (Left err)
                Right _ -> pure (Right RepoReady)

bootstrapProviderRepo :: AppConfig -> T.Text -> T.Text -> IO (Either T.Text RepoStatus)
bootstrapProviderRepo cfg repoUrl token = do
  stateResult <- ensureProviderRepo cfg
  case stateResult of
    Left err -> pure (Left err)
    Right RepoReady -> pure (Right RepoReady)
    Right RepoMissing -> do
      cloneResult <- cloneWithOAuth cfg repoUrl token
      case cloneResult of
        Left err -> pure (Left err)
        Right _ -> ensureProviderRepo cfg

ensureRepoWorktree :: AppConfig -> IO (Either T.Text ())
ensureRepoWorktree cfg = do
  let flakePath = appWorkDir cfg </> "flake.nix"
  hasFlake <- doesFileExist flakePath
  if not hasFlake
    then pure (Left (T.pack ("missing flake.nix at " <> flakePath)))
    else do
      createDirectoryIfMissing True (appWorkDir cfg </> "generated")
      let statePath = appWorkDir cfg </> "generated" </> "state.json"
      hasState <- doesFileExist statePath
      if hasState
        then pure ()
        else BL.writeFile statePath "{}\n"
      let AppConfig { appDataDir = dataDir } = cfg
      workDirExists <- doesDirectoryExist (appWorkDir cfg)
      if workDirExists
        then do
          chmodResult <- runCommandWithEnv cfg [] (CommandSpec "chmod" ["-R", "u+rwX", T.pack (appWorkDir cfg)] dataDir)
          case chmodResult of
            Left err -> pure (Left (commandErrorText err))
            Right _ -> pure (Right ())
        else pure (Right ())

cloneWithOAuth :: AppConfig -> T.Text -> T.Text -> IO (Either T.Text ())
cloneWithOAuth cfg repoUrl token = do
  withTempGitCredentials repoUrl token $ \envVars -> do
    let AppConfig { appDataDir = dataDir } = cfg
    let cloneCmd = CommandSpec "git" ["clone", repoUrl, T.pack dataDir] (takeDirectory dataDir)
    cloneResult <- runCommandWithEnv cfg envVars cloneCmd
    case cloneResult of
      Left err -> pure (Left (commandErrorText err))
      Right _ -> pure (Right ())

withTempGitCredentials :: T.Text -> T.Text -> ([(T.Text, T.Text)] -> IO a) -> IO a
withTempGitCredentials repoUrl token action = do
  tempRoot <- getTemporaryDirectory
  suffix <- randomToken 6
  let tempDir = tempRoot </> ("hostenv-provider-bootstrap-" <> T.unpack suffix)
  let credsPath = tempDir </> "git-credentials"
  let gitConfigPath = tempDir </> "gitconfig"
  createDirectoryIfMissing True tempDir
  writeFile credsPath (T.unpack (renderGitCredentials [(repoUrl, token)]))
  writeFile gitConfigPath (unlines ["[credential]", "\thelper = store --file " <> credsPath])
  setFileMode credsPath 0o600
  setFileMode gitConfigPath 0o600
  let envVars =
        [ ("GIT_CONFIG_GLOBAL", T.pack gitConfigPath)
        , ("GIT_TERMINAL_PROMPT", "0")
        ]
  action envVars `finally` cleanupDir tempDir

cleanupDir :: FilePath -> IO ()
cleanupDir path = do
  _ <- try (removePathForcibly path) :: IO (Either SomeException ())
  pure ()

ensureGitConfig :: AppConfig -> IO ()
ensureGitConfig cfg = do
  let AppConfig { appGitConfigPath = gitConfigPath, appGitCredentialsPath = credsPath } = cfg
  createDirectoryIfMissing True (takeDirectory gitConfigPath)
  let content = unlines
        [ "[credential]"
        , "\thelper = store --file " <> credsPath
        ]
  writeFile gitConfigPath content
  setEnv "GIT_CONFIG_GLOBAL" gitConfigPath
  setEnv "GIT_TERMINAL_PROMPT" "0"

openUnixSocket :: FilePath -> IO Socket
openUnixSocket path = do
  createDirectoryIfMissing True (takeDirectory path)
  exists <- doesFileExist path
  if exists
    then removeFile path
    else pure ()
  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock (SockAddrUnix path)
  listen sock 1024
  setFileMode path 0o660
  pure sock
