{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Repo
  ( RepoStatus(..)
  , RepoPullError(..)
  , ensureProviderRepo
  , bootstrapProviderRepo
  , pullProviderRepo
  , pullProviderRepoWithOAuth
  , withTempGitCredentials
  , ensureGitConfig
  , openUnixSocket
  , isAuthFailure
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
import Hostenv.Provider.Service (CommandOutput(..), CommandSpec(..), renderGitCredentials)
import Hostenv.Provider.Util (randomToken)

data RepoStatus
  = RepoReady
  | RepoMissing
  deriving (Eq, Show)

data RepoPullError
  = RepoPullAuthError T.Text
  | RepoPullError T.Text
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
          let bareRepoPath = providerBareRepoPath cfg
          hasBareRepo <- doesDirectoryExist bareRepoPath
          if not hasBareRepo
            then pure (Right RepoMissing)
            else do
              hasHead <- doesFileExist (bareRepoPath </> "HEAD")
              hasObjects <- doesDirectoryExist (bareRepoPath </> "objects")
              if not (hasHead && hasObjects)
                then pure (Left ("provider bare repository is invalid at " <> T.pack bareRepoPath))
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

pullProviderRepo :: AppConfig -> IO (Either RepoPullError ())
pullProviderRepo cfg = do
  pullProviderRepoWithEnv cfg []

pullProviderRepoWithOAuth :: AppConfig -> T.Text -> T.Text -> IO (Either RepoPullError ())
pullProviderRepoWithOAuth cfg host token =
  withTempGitCredentials ("https://" <> host) token $ \envVars ->
    pullProviderRepoWithEnv cfg envVars

pullProviderRepoWithEnv :: AppConfig -> [(T.Text, T.Text)] -> IO (Either RepoPullError ())
pullProviderRepoWithEnv cfg envVars = do
  let workDir = appWorkDir cfg
  pullResult <- runCommandWithEnv cfg envVars (CommandSpec "git" ["pull", "--rebase", "--autostash"] workDir)
  case pullResult of
    Left err ->
      let msg = "Failed to synchronize provider repository before operation.\n" <> commandErrorText err
       in if isAuthFailure msg
            then pure (Left (RepoPullAuthError msg))
            else pure (Left (RepoPullError msg))
    Right _ -> pure (Right ())

ensureRepoWorktree :: AppConfig -> IO (Either T.Text ())
ensureRepoWorktree cfg = do
  let bareRepoPath = providerBareRepoPath cfg
      workDir = appWorkDir cfg
  workDirExists <- doesDirectoryExist workDir
  worktreeResult <-
    if not workDirExists
      then do
        createDirectoryIfMissing True (takeDirectory workDir)
        addResult <- runCommandWithEnv cfg [] (CommandSpec "git" ["--git-dir", T.pack bareRepoPath, "worktree", "add", T.pack workDir] cfg.appDataDir)
        case addResult of
          Left err -> pure (Left ("failed to create provider worktree at " <> T.pack workDir <> "\n" <> commandErrorText err))
          Right _ -> pure (Right ())
      else pure (Right ())
  case worktreeResult of
    Left err -> pure (Left err)
    Right () -> do
      let flakePath = workDir </> "flake.nix"
      hasFlake <- doesFileExist flakePath
      if not hasFlake
        then pure (Left (T.pack ("missing flake.nix at " <> flakePath)))
        else do
          createDirectoryIfMissing True (workDir </> "generated")
          let statePath = workDir </> "generated" </> "state.json"
          hasState <- doesFileExist statePath
          if hasState
            then pure ()
            else BL.writeFile statePath "{}\n"
          chmodResult <- runCommandWithEnv cfg [] (CommandSpec "chmod" ["-R", "u+rwX", T.pack workDir] cfg.appDataDir)
          case chmodResult of
            Left err -> pure (Left (commandErrorText err))
            Right _ -> pure (Right ())

cloneWithOAuth :: AppConfig -> T.Text -> T.Text -> IO (Either T.Text ())
cloneWithOAuth cfg repoUrl token = do
  withTempGitCredentials repoUrl token $ \envVars -> do
    let AppConfig { appDataDir = dataDir } = cfg
        bareRepoPath = providerBareRepoPath cfg
    createDirectoryIfMissing True (takeDirectory bareRepoPath)
    let cloneCmd = CommandSpec "git" ["clone", "--bare", repoUrl, T.pack bareRepoPath] (takeDirectory bareRepoPath)
    cloneResult <- runCommandWithEnv cfg envVars cloneCmd
    case cloneResult of
      Left err -> pure (Left (commandErrorText err))
      Right _ -> do
        worktreeResult <- ensureRepoWorktree cfg
        case worktreeResult of
          Left err -> pure (Left err)
          Right () -> pure (Right ())

providerBareRepoPath :: AppConfig -> FilePath
providerBareRepoPath cfg = cfg.appDataDir </> "git" </> "provider.git"

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
        , "[user]"
        , "\temail = hostenv-provider@localhost"
        , "\tname = hostenv-provider"
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

isAuthFailure :: T.Text -> Bool
isAuthFailure msg =
  let lower = T.toLower msg
      patterns =
        [ "access denied"
        , "authentication failed"
        , "http basic: access denied"
        , "could not read username"
        , "invalid credentials"
        ]
   in any (`T.isInfixOf` lower) patterns
