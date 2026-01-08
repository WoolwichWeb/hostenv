{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Config
  ( AppConfig(..)
  , loadConfig
  , appWorkDir
  , resolvePath
  , uiPath
  , normalizeBasePath
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>), isAbsolute)

import Hostenv.Provider.Service (GitlabSecrets, WebhookConfig(..), readGitlabSecrets)

-- Configuration

data AppConfig = AppConfig
  { appDataDir :: FilePath
  , appRepoSource :: FilePath
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
  , appGitConfigPath :: FilePath
  , appGitCredentialsPath :: FilePath
  , appFlakeTemplate :: FilePath
  , appHttpManager :: Maybe Manager
  }

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

loadConfig :: IO AppConfig
loadConfig = do
  dataDir <- do
    override <- lookupEnv "HOSTENV_PROVIDER_DATA_DIR"
    case override of
      Just v -> pure v
      Nothing -> getXdgDirectory XdgData "hostenv-provider"

  repoSource <- requireEnv "HOSTENV_PROVIDER_REPO_SOURCE"
  flakeRoot <- fromMaybe "." <$> lookupEnv "HOSTENV_PROVIDER_FLAKE_ROOT"
  listenSocket <- requireEnv "HOSTENV_PROVIDER_LISTEN_SOCKET"
  secretFile <- lookupEnv "HOSTENV_PROVIDER_WEBHOOK_SECRET_FILE"
  secretsDir <- lookupEnv "HOSTENV_PROVIDER_WEBHOOK_SECRETS_DIR"
  webhookHost <- T.pack <$> requireEnv "HOSTENV_PROVIDER_WEBHOOK_HOST"

  uiBasePath <- T.pack <$> (fromMaybe "/ui" <$> lookupEnv "HOSTENV_PROVIDER_UI_BASE_PATH")
  uiBaseUrl <- T.pack <$> (fromMaybe "https://example.invalid" <$> lookupEnv "HOSTENV_PROVIDER_UI_BASE_URL")

  dbUri <- Just <$> requireEnv "HOSTENV_PROVIDER_DB_URI"
  gitlabHosts <- fmap parseList (lookupEnv "HOSTENV_PROVIDER_GITLAB_HOSTS")
  gitConfigPath <- fromMaybe (dataDir </> "gitconfig") <$> lookupEnv "HOSTENV_PROVIDER_GIT_CONFIG_FILE"
  gitCredentialsPath <- fromMaybe (dataDir </> "git-credentials") <$> lookupEnv "HOSTENV_PROVIDER_GIT_CREDENTIALS_FILE"
  flakeTemplate <- fromMaybe "flake.template.nix" <$> lookupEnv "HOSTENV_PROVIDER_FLAKE_TEMPLATE"

  mSecretsPath <- lookupEnv "HOSTENV_PROVIDER_GITLAB_SECRETS_FILE"
  secrets <- case mSecretsPath of
    Nothing -> dieWith "GitLab OAuth secrets file missing"
    Just path -> Just <$> readGitlabSecrets path

  manager <- Just <$> newManager tlsManagerSettings

  let workDir = dataDir </> flakeRoot
  let planPath = workDir </> "generated" </> "plan.json"
  let webhookCfg = WebhookConfig workDir planPath

  pure
    AppConfig
      { appDataDir = dataDir
      , appRepoSource = repoSource
      , appFlakeRoot = flakeRoot
      , appListenSocket = listenSocket
      , appWebhookSecretFile = secretFile
      , appWebhookSecretsDir = secretsDir
      , appWebhookConfig = webhookCfg
      , appUiBasePath = normalizeBasePath uiBasePath
      , appUiBaseUrl = T.dropWhileEnd (== '/') uiBaseUrl
      , appWebhookHost = webhookHost
      , appDbConnString = BSC.pack <$> dbUri
      , appGitlabSecrets = secrets
      , appGitlabHosts = if null gitlabHosts then ["gitlab.com"] else gitlabHosts
      , appGitConfigPath = gitConfigPath
      , appGitCredentialsPath = gitCredentialsPath
      , appFlakeTemplate = flakeTemplate
      , appHttpManager = manager
      }

parseList :: Maybe String -> [Text]
parseList Nothing = []
parseList (Just raw) =
  filter (/= "") (map T.strip (T.splitOn "," (T.pack raw)))

requireEnv :: String -> IO String
requireEnv key = do
  val <- lookupEnv key
  case val of
    Just v -> pure v
    Nothing -> dieWith ("missing required env var: " <> key)


dieWith :: String -> IO a
dieWith msg = do
  hPutStrLn stderr msg
  exitFailure
