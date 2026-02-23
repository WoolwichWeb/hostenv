{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Project
  ( addProjectFlow
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
  ( GitlabAccessToken(..)
  , ProjectRow(..)
  , SessionInfo(..)
  , User(..)
  , loadLatestUserGitlabToken
  , loadProjects
  , saveGitlabToken
  , withDb
  )
import Hostenv.Provider.Gitlab (GitlabHook(..), GitlabProject(..), appendNixAccessTokenConfig, createGitlabWebhook, fetchGitlabProject, updateGitlabWebhook)
import Hostenv.Provider.Repo (bootstrapProviderRepo)
import Hostenv.Provider.Service (CommandSpec(..), projectHashFor, renderFlakeTemplate, renderProjectInputs)
import Hostenv.Provider.Util (randomToken, sanitizeName, splitNamespace)
import Hostenv.Provider.Webhook (loadPlan)


syncFlakeFromDb :: AppConfig -> IO ()
syncFlakeFromDb cfg = do
  projects <- withDb cfg loadProjects
  _ <- regenerateFlake cfg projects
  pure ()

addProjectFlow :: AppConfig -> SessionInfo -> Int64 -> Maybe Text -> Maybe Text -> IO (Either Text Text)
addProjectFlow cfg sess repoId orgInput projectInput = withDb cfg $ \conn -> do
  let userIdVal = sess.user.id
  tokenResult <- loadLatestUserGitlabToken cfg conn userIdVal
  case tokenResult of
    Left err -> pure (Left err)
    Right Nothing -> pure (Left "Missing GitLab token for user")
    Right (Just tokenInfo) -> do
      let host = tokenInfo.host
          token = tokenInfo.value
      projectInfo <- fetchGitlabProject cfg host token repoId
      case projectInfo of
        Left msg -> pure (Left msg)
        Right repo -> do
          let (defaultOrg, defaultProject) = splitNamespace repo.glProjectPath
          let orgCandidate = fromMaybe defaultOrg orgInput
          let projCandidate = fromMaybe defaultProject projectInput
          let org = if T.strip orgCandidate == "" then sanitizeName defaultOrg else sanitizeName orgCandidate
          let proj = if T.strip projCandidate == "" then sanitizeName defaultProject else sanitizeName projCandidate
          if org == "" || proj == ""
            then pure (Left "Organisation and project names must be non-empty")
            else do
              let flakeInput = org <> "__" <> proj
              [projectRow] <- query conn
                "INSERT INTO projects (org, project, git_host, repo_id, repo_url, repo_path, flake_input) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT (git_host, repo_id) DO UPDATE SET org = EXCLUDED.org, project = EXCLUDED.project, repo_url = EXCLUDED.repo_url, repo_path = EXCLUDED.repo_path, flake_input = EXCLUDED.flake_input, updated_at = now() RETURNING id, org, project, git_host, repo_id, repo_url, repo_path, flake_input, default_env_hash"
                (org, proj, host, repo.glProjectId, repo.glProjectHttpUrl, repo.glProjectPath, flakeInput)
              let projectIdVal = projectRow.id
              _ <- execute conn "DELETE FROM gitlab_tokens WHERE project_id = ?" (Only projectIdVal)
              saveResult <- saveGitlabToken cfg conn (Just userIdVal) (Just projectIdVal) host token ("api read_repository" :: Text)
              case saveResult of
                Left err -> pure (Left err)
                Right _ -> do
                  regenResult <- regenerateFlake cfg =<< loadProjects conn
                  case regenResult of
                    Left msg -> pure (Left msg)
                    Right _ -> do
                      existingNixConfig <- fmap (fmap T.pack) (lookupEnv "NIX_CONFIG")
                      let scopedNixConfig = appendNixAccessTokenConfig existingNixConfig host token
                          runScoped = runCommandWithEnv cfg [("NIX_CONFIG", scopedNixConfig)]
                      cmdRes <- runScoped (CommandSpec "nix" ["flake", "update", flakeInput] (appWorkDir cfg))
                      case cmdRes of
                        Left err -> pure (Left (commandErrorText err))
                        Right _ -> do
                          planRes <- runScoped (CommandSpec "nix" ["run", ".#hostenv-provider", "--", "plan"] (appWorkDir cfg))
                          case planRes of
                            Left err -> pure (Left (commandErrorText err))
                            Right _ -> do
                              planRaw <- loadPlan cfg
                              case projectHashFor org proj planRaw of
                                Left err -> pure (Left err)
                                Right projHash -> do
                                  _ <- execute conn "UPDATE projects SET default_env_hash = ?, updated_at = now() WHERE id = ?" (projHash, projectIdVal)
                                  webhookResult <- ensureWebhook cfg conn host token repoId projectRow projHash
                                  case webhookResult of
                                    Left msg -> pure (Left msg)
                                    Right (secret, _) -> do
                                      maybeWriteSecret cfg projHash org proj secret
                                      let AppConfig { appWebhookHost = webhookHost } = cfg
                                      let hookUrl = "https://" <> webhookHost <> "/webhook/" <> projHash
                                      pure (Right ("Webhook configured at " <> hookUrl))

bootstrapRepoFlow :: AppConfig -> SessionInfo -> Int64 -> IO (Either Text Text)
bootstrapRepoFlow cfg sess repoId = do
  tokenResult <- withDb cfg $ \conn -> do
    let userIdVal = sess.user.id
    loadLatestUserGitlabToken cfg conn userIdVal
  case tokenResult of
    Left err -> pure (Left err)
    Right Nothing -> pure (Left "Missing GitLab token for user")
    Right (Just tokenInfo) -> do
      let host = tokenInfo.host
          token = tokenInfo.value
      projectInfo <- fetchGitlabProject cfg host token repoId
      case projectInfo of
        Left msg -> pure (Left msg)
        Right repo -> do
          bootstrapResult <- bootstrapProviderRepo cfg repo.glProjectHttpUrl token
          case bootstrapResult of
            Left msg -> pure (Left msg)
            Right _ -> do
              syncFlakeFromDb cfg
              pure (Right ("Provider repository bootstrapped from " <> repo.glProjectPath))

ensureWebhook :: AppConfig -> Connection -> Text -> Text -> Int64 -> ProjectRow -> Text -> IO (Either Text (Text, Maybe Int64))
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
    Left msg -> pure (Left msg)
    Right hook -> do
      _ <- execute conn "INSERT INTO webhooks (project_id, secret, webhook_id, webhook_url) VALUES (?, ?, ?, ?) ON CONFLICT (project_id) DO UPDATE SET secret = EXCLUDED.secret, webhook_id = EXCLUDED.webhook_id, webhook_url = EXCLUDED.webhook_url, updated_at = now()" (projectIdVal, secret, hook.glHookId, hook.glHookUrl)
      pure (Right (secret, Just hook.glHookId))

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
