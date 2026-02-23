{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Project
  ( addProjectFlow
  , ProjectFlowError(..)
  , projectFlowErrorText
  , bootstrapRepoFlow
  , ensureWebhook
  , regenerateFlake
  , syncFlakeFromDb
  ) where

import Control.Exception (IOException, catch)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BSC
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)

import Hostenv.Provider.Command (commandErrorText, runCommandWithEnv)
import Hostenv.Provider.Config (AppConfig(..), appWorkDir, resolvePath)
import Hostenv.Provider.DB
  ( OAuthCredential(..)
  , ProjectRow(..)
  , SessionInfo(..)
  , User(..)
  , loadProjects
  , upsertProjectOAuthCredential
  , withDb
  )
import Hostenv.Provider.Gitlab
  ( GitlabError
  , GitlabHook(..)
  , GitlabProject(..)
  , appendNixAccessTokenConfig
  , createGitlabWebhook
  , fetchGitlabProject
  , isReauthError
  , loadUserOAuthCredential
  , renderGitlabError
  , updateGitlabWebhook
  )
import Hostenv.Provider.Repo (RepoPullError(..), bootstrapProviderRepo, pullProviderRepo)
import Hostenv.Provider.Service (CommandSpec(..), projectHashFor, renderFlakeTemplate, renderProjectInputs)
import Hostenv.Provider.Util (randomToken, sanitizeName, splitNamespace)
import Hostenv.Provider.Webhook (loadPlan)

data ProjectFlowError
  = ProjectFlowError Text
  | ProjectFlowAuthError Text
  deriving (Eq, Show)

projectFlowErrorText :: ProjectFlowError -> Text
projectFlowErrorText flowErr =
  case flowErr of
    ProjectFlowError msg -> msg
    ProjectFlowAuthError msg -> msg

syncFlakeFromDb :: AppConfig -> IO (Either Text ())
syncFlakeFromDb cfg = do
  pullResult <- pullProviderRepo cfg
  case pullResult of
    Left pullErr -> pure (Left (repoPullErrorText pullErr))
    Right _ -> do
      projects <- withDb cfg loadProjects
      regenerateFlake cfg projects

addProjectFlow :: AppConfig -> SessionInfo -> Int64 -> Maybe Text -> Maybe Text -> IO (Either ProjectFlowError Text)
addProjectFlow cfg sess repoId orgInput projectInput = do
  pullResult <- pullProviderRepo cfg
  case pullResult of
    Left pullErr -> pure (Left (projectFlowErrorFromPullError pullErr))
    Right _ -> do
      credentialResult <- loadUserOAuthCredential cfg sess
      case credentialResult of
        Left err -> pure (Left (projectFlowErrorFromGitlab err))
        Right credential -> withDb cfg $ \conn -> do
          let OAuthCredential { host = host, accessToken = token } = credential
          projectInfo <- fetchGitlabProject cfg host token repoId
          case projectInfo of
            Left err -> pure (Left (projectFlowErrorFromGitlab err))
            Right repo -> do
              let (defaultOrg, defaultProject) = splitNamespace repo.path
              let orgCandidate = fromMaybe defaultOrg orgInput
              let projCandidate = fromMaybe defaultProject projectInput
              let org = if T.strip orgCandidate == "" then sanitizeName defaultOrg else sanitizeName orgCandidate
              let proj = if T.strip projCandidate == "" then sanitizeName defaultProject else sanitizeName projCandidate
              if org == "" || proj == ""
                then pure (Left (ProjectFlowError "Organisation and project names must be non-empty"))
                else do
                  let flakeInput = org <> "__" <> proj
                  [projectRow] <- query conn
                    "INSERT INTO projects (org, project, git_host, repo_id, repo_url, repo_path, flake_input) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT (git_host, repo_id) DO UPDATE SET org = EXCLUDED.org, project = EXCLUDED.project, repo_url = EXCLUDED.repo_url, repo_path = EXCLUDED.repo_path, flake_input = EXCLUDED.flake_input, updated_at = now() RETURNING id, org, project, git_host, repo_id, repo_url, repo_path, flake_input, default_env_hash"
                    (org, proj, host, repo.id, repo.httpUrl, repo.path, flakeInput)
                  let projectIdVal = projectRow.id
                  _ <- execute conn "DELETE FROM project_oauth_credentials WHERE project_id = ?" (Only projectIdVal)
                  saveResult <- upsertProjectOAuthCredential cfg conn projectIdVal credential
                  case saveResult of
                    Left err -> pure (Left (ProjectFlowError err))
                    Right _ -> do
                      regenResult <- regenerateFlake cfg =<< loadProjects conn
                      case regenResult of
                        Left msg -> pure (Left (ProjectFlowError msg))
                        Right _ -> do
                          existingNixConfig <- fmap (fmap T.pack) (lookupEnv "NIX_CONFIG")
                          let scopedNixConfig = appendNixAccessTokenConfig existingNixConfig host token
                              runScoped = runCommandWithEnv cfg [("NIX_CONFIG", scopedNixConfig)]
                          cmdRes <- runScoped (CommandSpec "nix" ["flake", "update", flakeInput] (appWorkDir cfg))
                          case cmdRes of
                            Left err -> pure (Left (ProjectFlowError (commandErrorText err)))
                            Right _ -> do
                              planRes <- runScoped (CommandSpec "nix" ["run", ".#hostenv-provider", "--", "plan"] (appWorkDir cfg))
                              case planRes of
                                Left err -> pure (Left (ProjectFlowError (commandErrorText err)))
                                Right _ -> do
                                  planRaw <- loadPlan cfg
                                  case projectHashFor org proj planRaw of
                                    Left err -> pure (Left (ProjectFlowError err))
                                    Right projHash -> do
                                      _ <- execute conn "UPDATE projects SET default_env_hash = ?, updated_at = now() WHERE id = ?" (projHash, projectIdVal)
                                      webhookResult <- ensureWebhook cfg conn host token repoId projectRow projHash
                                      case webhookResult of
                                        Left err -> pure (Left (projectFlowErrorFromGitlab err))
                                        Right (secret, _) -> do
                                          maybeWriteSecret cfg projHash org proj secret
                                          let AppConfig { appWebhookHost = webhookHost } = cfg
                                          let hookUrl = "https://" <> webhookHost <> "/webhook/" <> projHash
                                          pure (Right ("Webhook configured at " <> hookUrl))

bootstrapRepoFlow :: AppConfig -> SessionInfo -> Int64 -> IO (Either Text Text)
bootstrapRepoFlow cfg sess repoId = do
  credentialResult <- loadUserOAuthCredential cfg sess
  case credentialResult of
    Left err -> pure (Left (renderGitlabError err))
    Right credential -> do
      let OAuthCredential { host = host, accessToken = token } = credential
      projectInfo <- fetchGitlabProject cfg host token repoId
      case projectInfo of
        Left err -> pure (Left (renderGitlabError err))
        Right repo -> do
          bootstrapResult <- bootstrapProviderRepo cfg repo.httpUrl token
          case bootstrapResult of
            Left msg -> pure (Left msg)
            Right _ -> do
              syncResult <- syncFlakeFromDb cfg
              case syncResult of
                Left msg -> pure (Left msg)
                Right _ -> pure (Right ("Provider repository bootstrapped from " <> repo.path))

ensureWebhook :: AppConfig -> Connection -> Text -> Text -> Int64 -> ProjectRow -> Text -> IO (Either GitlabError (Text, Maybe Int64))
ensureWebhook cfg conn host token repoId projectRow projHash = do
  let projectIdVal = projectRow.id
  existing <- query conn "SELECT secret, webhook_id FROM webhooks WHERE project_id = ?" (Only projectIdVal)
  (secret, hookId) <- case existing of
    ((s, mId):_) -> pure (s, mId)
    [] -> do
      s <- randomToken 24
      pure (s, Nothing)
  let AppConfig { appWebhookHost = webhookHost } = cfg
  let url = "https://" <> webhookHost <> "/webhook/" <> projHash
  hookResult <- case hookId of
    Just hid -> do
      updated <- updateGitlabWebhook cfg host token repoId hid url secret
      case updated of
        Left _ -> createGitlabWebhook cfg host token repoId url secret
        Right hook -> pure (Right hook)
    Nothing -> createGitlabWebhook cfg host token repoId url secret
  case hookResult of
    Left err -> pure (Left err)
    Right hook -> do
      _ <- execute conn "INSERT INTO webhooks (project_id, secret, webhook_id, webhook_url) VALUES (?, ?, ?, ?) ON CONFLICT (project_id) DO UPDATE SET secret = EXCLUDED.secret, webhook_id = EXCLUDED.webhook_id, webhook_url = EXCLUDED.webhook_url, updated_at = now()" (projectIdVal, secret, hook.id, hook.url)
      pure (Right (secret, Just hook.id))

regenerateFlake :: AppConfig -> [ProjectRow] -> IO (Either Text ())
regenerateFlake cfg projects = do
  let AppConfig { appFlakeTemplate = flakeTemplate } = cfg
  let templatePath = resolvePath cfg flakeTemplate
  exists <- doesFileExist templatePath
  if not exists
    then pure (Left ("flake template not found: " <> T.pack templatePath))
    else do
      templateText <- T.pack <$> readFile templatePath
      let inputs = [ (input, projectInputUrl p) | p@ProjectRow { flakeInput = input } <- projects ]
      let inputBlock = renderProjectInputs inputs
      case renderFlakeTemplate templateText inputBlock of
        Left err -> pure (Left err)
        Right flakeText -> do
          let flakePath = appWorkDir cfg </> "flake.nix"
          let tempFlakePath = flakePath <> ".tmp"
          writeFile tempFlakePath (T.unpack flakeText)
          validationResult <- validateFlake cfg templatePath tempFlakePath
          case validationResult of
            Left err -> do
              removeFileIfExists tempFlakePath
              pure (Left err)
            Right _ -> do
              renameFile tempFlakePath flakePath
              pure (Right ())

projectInputUrl :: ProjectRow -> Text
projectInputUrl p =
  let ProjectRow { gitHost = gitHost, repoPath = repoPath } = p
      base = if gitHost == "gitlab.com" then "gitlab:" else "gitlab:" <> gitHost <> "/"
   in base <> repoPath <> "?dir=.hostenv"

maybeWriteSecret :: AppConfig -> Text -> Text -> Text -> Text -> IO ()
maybeWriteSecret cfg hash org proj secret =
  let AppConfig { appWebhookSecretsDir = secretsDir } = cfg
   in case secretsDir of
    Nothing -> pure ()
    Just dir -> do
      createDirectoryIfMissing True dir
      let byHash = dir </> T.unpack hash
      let byProject = dir </> T.unpack (org <> "__" <> proj)
      writeSecretFile byHash secret
      writeSecretFile byProject secret

writeSecretFile :: FilePath -> Text -> IO ()
writeSecretFile path secret = do
  BSC.writeFile path (TE.encodeUtf8 secret)
  setFileMode path 0o640

validateFlake :: AppConfig -> FilePath -> FilePath -> IO (Either Text ())
validateFlake cfg templatePath flakePath = do
  result <- runCommandWithEnv cfg [] (CommandSpec "nix-instantiate" ["--parse", T.pack flakePath] (appWorkDir cfg))
  case result of
    Left err ->
      pure
        ( Left
            ( T.unlines
                [ "Generated flake.nix failed Nix syntax validation."
                , "Check template: " <> T.pack templatePath
                , "Rendered file: " <> T.pack flakePath
                , commandErrorText err
                ]
            )
        )
    Right _ -> pure (Right ())

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = removeFile path `catch` ignoreMissing
  where
    ignoreMissing :: IOException -> IO ()
    ignoreMissing _ = pure ()

repoPullErrorText :: RepoPullError -> Text
repoPullErrorText pullErr =
  case pullErr of
    RepoPullAuthError msg -> msg
    RepoPullError msg -> msg

projectFlowErrorFromPullError :: RepoPullError -> ProjectFlowError
projectFlowErrorFromPullError pullErr =
  case pullErr of
    RepoPullAuthError msg -> ProjectFlowAuthError msg
    RepoPullError msg -> ProjectFlowError msg

projectFlowErrorFromGitlab :: GitlabError -> ProjectFlowError
projectFlowErrorFromGitlab gitlabErr =
  let msg = renderGitlabError gitlabErr
   in if isReauthError gitlabErr
        then ProjectFlowAuthError msg
        else ProjectFlowError msg
