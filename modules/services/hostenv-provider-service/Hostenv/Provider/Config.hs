{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Config
  ( AppConfig(..)
  , ProviderAccountConfig(..)
  , UserConfig(..)
  , loadConfig
  , appWorkDir
  , resolvePath
  , uiPath
  , normalizeBasePath
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>), isAbsolute)

import Hostenv.Provider.Crypto (TokenCipher, loadTokenCipher)
import Hostenv.Provider.Service (GitlabSecrets, WebhookConfig(..), readGitlabSecrets)

-- Configuration

data AppConfig = AppConfig
  { appDataDir :: FilePath
  , appFlakeRoot :: FilePath
  , appListenSocket :: FilePath
  , appWebhookSecretFile :: Maybe FilePath
  , appWebhookSecretsDir :: Maybe FilePath
  , appWebhookConfig :: WebhookConfig
  , appUiBasePath :: Text
  , appUiBaseUrl :: Text
  , appWebhookHost :: Text
  , appDbConnString :: Maybe BS.ByteString
  , appGitlabSecrets :: Maybe GitlabSecrets
  , appGitlabHosts :: [Text]
  , appGitlabTokenCipher :: Maybe TokenCipher
  , appGitlabDeployTokenTtlMinutes :: Int
  , appGitConfigPath :: FilePath
  , appGitCredentialsPath :: FilePath
  , appFlakeTemplate :: FilePath
  , appSeedUsers :: [UserConfig]
  , appJobsRetentionDays :: Int
  , appJobsCleanupIntervalMinutes :: Int
  , appHttpManager :: Maybe Manager
  }

data ProviderConfigFile = ProviderConfigFile
  { dataDir :: FilePath
  , flakeRoot :: FilePath
  , listenSocket :: FilePath
  , webhookSecretFile :: Maybe FilePath
  , webhookSecretsDir :: Maybe FilePath
  , webhookHost :: Text
  , uiBasePath :: Text
  , uiBaseUrl :: Text
  , dbUri :: String
  , gitlab :: GitlabConfigFile
  , seedUsers :: [UserConfig]
  , gitConfigFile :: FilePath
  , gitCredentialsFile :: FilePath
  , flakeTemplate :: FilePath
  , jobs :: JobsConfigFile
  } deriving (Eq, Show, Generic)

data GitlabConfigFile = GitlabConfigFile
  { enable :: Bool
  , oAuthSecretsFile :: Maybe FilePath
  , hosts :: [Text]
  , tokenEncryptionKeyFile :: Maybe FilePath
  , deployTokenTtlMinutes :: Maybe Int
  } deriving (Eq, Show, Generic)

data JobsConfigFile = JobsConfigFile
  { retentionDays :: Int
  , cleanupIntervalMinutes :: Int
  } deriving (Eq, Show, Generic)

data ProviderAccountConfig = ProviderAccountConfig
  { provider :: Text
  , host :: Text
  , username :: Text
  , userId :: Maybe Int64
  } deriving (Eq, Show, Generic)

data UserConfig = UserConfig
  { configUsername :: Text
  , email :: Maybe Text
  , role :: Text
  , providerAccounts :: [ProviderAccountConfig]
  } deriving (Eq, Show, Generic)

instance A.FromJSON GitlabConfigFile where
  parseJSON = A.withObject "GitlabConfigFile" $ \o ->
    GitlabConfigFile
      <$> o A..:? "enable" A..!= False
      <*> o A..:? "oAuthSecretsFile"
      <*> o A..:? "hosts" A..!= []
      <*> o A..:? "tokenEncryptionKeyFile"
      <*> o A..:? "deployTokenTtlMinutes"

instance A.FromJSON ProviderAccountConfig

instance A.FromJSON UserConfig

instance A.FromJSON JobsConfigFile where
  parseJSON = A.withObject "JobsConfigFile" $ \o ->
    JobsConfigFile
      <$> o A..:? "retentionDays" A..!= 30
      <*> o A..:? "cleanupIntervalMinutes" A..!= 1440

instance A.FromJSON ProviderConfigFile where
  parseJSON = A.withObject "ProviderConfigFile" $ \o -> do
    gitlabCfg <-
      o A..:? "gitlab" >>= \case
        Just cfg -> pure cfg
        Nothing -> do
          legacySecrets <- o A..:? "gitlabOAuthSecretsFile"
          legacyHosts <- o A..:? "gitlabHosts" A..!= []
          legacyTokenKey <- o A..:? "gitlabTokenEncryptionKeyFile"
          legacyDeployTokenTtl <- o A..:? "gitlabDeployTokenTtlMinutes"
          pure
            GitlabConfigFile
              { enable = isJust legacySecrets
              , oAuthSecretsFile = legacySecrets
              , hosts = legacyHosts
              , tokenEncryptionKeyFile = legacyTokenKey
              , deployTokenTtlMinutes = legacyDeployTokenTtl
              }
    ProviderConfigFile
      <$> o A..: "dataDir"
      <*> o A..: "flakeRoot"
      <*> o A..: "listenSocket"
      <*> o A..:? "webhookSecretFile"
      <*> o A..:? "webhookSecretsDir"
      <*> o A..: "webhookHost"
      <*> o A..: "uiBasePath"
      <*> o A..: "uiBaseUrl"
      <*> o A..: "dbUri"
      <*> pure gitlabCfg
      <*> o A..:? "seedUsers" A..!= []
      <*> o A..: "gitConfigFile"
      <*> o A..: "gitCredentialsFile"
      <*> o A..: "flakeTemplate"
      <*> o A..:? "jobs" A..!= JobsConfigFile { retentionDays = 30, cleanupIntervalMinutes = 1440 }

appWorkDir :: AppConfig -> FilePath
appWorkDir cfg = cfg.appDataDir </> cfg.appFlakeRoot

resolvePath :: AppConfig -> FilePath -> FilePath
resolvePath cfg path =
  if isAbsolute path then path else appWorkDir cfg </> path

uiPath :: AppConfig -> Text -> Text
uiPath cfg path =
  let base = normalizeBasePath cfg.appUiBasePath
   in case (base, path) of
        ("/", "/") -> "/"
        ("/", _) -> path
        (_, "/") -> base
        _ -> base <> path

normalizeBasePath :: Text -> Text
normalizeBasePath t =
  let withSlash = if T.isPrefixOf "/" t then t else "/" <> t
      trimmed = T.dropWhileEnd (== '/') withSlash
   in if trimmed == "" then "/" else trimmed

loadConfig :: FilePath -> IO AppConfig
loadConfig configPath = do
  providerCfg <- readProviderConfig configPath
  let gitlabCfg = providerCfg.gitlab
  secrets <-
    if gitlabCfg.enable
      then case gitlabCfg.oAuthSecretsFile of
        Nothing ->
          dieWith "gitlab.oAuthSecretsFile must be configured when gitlab.enable is true"
        Just path -> Just <$> readGitlabSecrets path
      else pure Nothing

  tokenCipher <-
    if gitlabCfg.enable
      then case gitlabCfg.tokenEncryptionKeyFile of
        Nothing ->
          dieWith "gitlab.tokenEncryptionKeyFile must be configured when gitlab.enable is true"
        Just keyPath -> do
          cipherResult <- loadTokenCipher keyPath
          case cipherResult of
            Left err -> dieWith ("failed to load token encryption key " <> keyPath <> ": " <> T.unpack err)
            Right cipher -> pure (Just cipher)
      else pure Nothing

  manager <- Just <$> newManager tlsManagerSettings

  let workDir = providerCfg.dataDir </> providerCfg.flakeRoot
  let planPath = workDir </> "generated" </> "plan.json"
  let webhookCfg = WebhookConfig workDir planPath

  pure
    AppConfig
      { appDataDir = providerCfg.dataDir
      , appFlakeRoot = providerCfg.flakeRoot
      , appListenSocket = providerCfg.listenSocket
      , appWebhookSecretFile = providerCfg.webhookSecretFile
      , appWebhookSecretsDir = providerCfg.webhookSecretsDir
      , appWebhookConfig = webhookCfg
      , appUiBasePath = normalizeBasePath providerCfg.uiBasePath
      , appUiBaseUrl = T.dropWhileEnd (== '/') providerCfg.uiBaseUrl
      , appWebhookHost = providerCfg.webhookHost
      , appDbConnString = Just (BSC.pack providerCfg.dbUri)
      , appGitlabSecrets = secrets
      , appGitlabHosts = if null gitlabCfg.hosts then ["gitlab.com"] else gitlabCfg.hosts
      , appGitlabTokenCipher = tokenCipher
      , appGitlabDeployTokenTtlMinutes = max 1 (maybe 15 id gitlabCfg.deployTokenTtlMinutes)
      , appGitConfigPath = providerCfg.gitConfigFile
      , appGitCredentialsPath = providerCfg.gitCredentialsFile
      , appFlakeTemplate = providerCfg.flakeTemplate
      , appSeedUsers = providerCfg.seedUsers
      , appJobsRetentionDays = max 1 providerCfg.jobs.retentionDays
      , appJobsCleanupIntervalMinutes = max 1 providerCfg.jobs.cleanupIntervalMinutes
      , appHttpManager = manager
      }

readProviderConfig :: FilePath -> IO ProviderConfigFile
readProviderConfig path = do
  exists <- doesFileExist path
  if not exists
    then dieWith ("provider config file not found: " <> path)
    else do
      bytes <- BL.readFile path
      case A.eitherDecode' bytes of
        Left err -> dieWith ("failed to parse provider config file " <> path <> ": " <> err)
        Right cfg -> pure cfg


dieWith :: String -> IO a
dieWith msg = do
  hPutStrLn stderr msg
  exitFailure
