{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Webhook
  ( webhookHandler
  , loadPlan
  , WebhookAccepted(..)
  , runWebhookDeployJob
  , chooseFinalResult
  ) where

import Control.Exception (finally)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Only (..), fromOnly, query)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Servant

import Hostenv.Provider.Command (commandErrorText, exitCodeToInt, renderCommand, runCommandWithEnv)
import Hostenv.Provider.Config (AppConfig(..))
import Hostenv.Provider.DB (DeployCredential(..), loadDeployCredentialByHash, withDb)
import Hostenv.Provider.Gitlab (GitlabDeployToken(..), NixGitlabTokenType(..), appendNixAccessTokenConfig, createProjectDeployToken, ensureProjectCredential, renderGitlabError, revokeProjectToken)
import Hostenv.Provider.Http (ErrorResponse(..), errorWithBody)
import Hostenv.Provider.Jobs (JobLogger(..), JobRuntime, NewJob(..), enqueueJob)
import Hostenv.Provider.Repo (RepoStatus(..))
import Hostenv.Provider.Service
  ( CommandError(..)
  , CommandOutput(..)
  , ProjectRef(..)
  , WebhookConfig(..)
  , WebhookError(..)
  , WebhookResult(..)
  , projectForHash
  , runWebhookWith
  , verifyGitHubSignature
  , verifyGitLabToken
  )
import Hostenv.Provider.Util (pickFirstExisting, readSecret)

data WebhookAccepted = WebhookAccepted
  { accepted :: Bool
  , jobId :: Text
  } deriving (Eq, Show)

instance A.ToJSON WebhookAccepted where
  toJSON response =
    A.object
      [ "accepted" A..= response.accepted
      , "jobId" A..= response.jobId
      ]

loadPlan :: AppConfig -> IO BL.ByteString
loadPlan cfg =
  let AppConfig { appWebhookConfig = WebhookConfig { whPlanPath = planPath } } = cfg
   in BL.readFile planPath

webhookHandler :: JobRuntime -> IORef RepoStatus -> AppConfig -> Text -> Maybe Text -> Maybe Text -> BL.ByteString -> Handler WebhookAccepted
webhookHandler runtime repoStatusRef cfg hash mHubSig mGitlabToken rawBody = do
  repoStatus <- liftIO (readIORef repoStatusRef)
  case repoStatus of
    RepoMissing ->
      throwError
        ( errorWithBody
            err503
            (ErrorResponse "provider repository is not initialized; bootstrap it via the dashboard first" Nothing Nothing Nothing Nothing)
        )
    RepoReady -> pure ()
  planRaw <- liftIO (loadPlan cfg)
  projectRef <-
    case projectForHash hash planRaw of
      Left msg ->
        if msg == "webhook hash not found in plan.json"
          then throwError err404
          else throwError (errorWithBody err500 (ErrorResponse msg Nothing Nothing Nothing Nothing))
      Right ref -> pure ref
  secretInfo <- liftIO (resolveSecret cfg hash projectRef)
  verifyWebhook secretInfo mHubSig mGitlabToken rawBody
  queuedJobId <- liftIO $ enqueueJob runtime $
    NewJob
      { kind = "webhook_deploy"
      , requestedByUserId = Nothing
      , projectId = Nothing
      , payload =
          A.object
            [ "hash" A..= hash
            , "org" A..= projectRef.prOrg
            , "project" A..= projectRef.prProject
            ]
      , run = runWebhookDeployJob cfg hash projectRef
      }
  pure (WebhookAccepted True queuedJobId)

runWebhookDeployJob :: AppConfig -> Text -> ProjectRef -> JobLogger -> IO (Either Text Text)
runWebhookDeployJob cfg hash projectRef logger = do
  logger.logInfo ("Running webhook deployment for hash " <> hash)
  deployCredResult <- withDb cfg (\conn -> loadDeployCredentialByHash cfg conn hash)
  deployCred <-
    case deployCredResult of
      Left err -> pure (Left err)
      Right Nothing -> pure (Left "Missing project OAuth token for deploy")
      Right (Just cred) -> pure (Right cred)
  case deployCred of
    Left err -> pure (Left err)
    Right cred -> do
      refreshedCredResult <- ensureProjectCredential cfg cred
      refreshedDeployCred <-
        case refreshedCredResult of
          Left err -> pure (Left (renderGitlabError err))
          Right refreshed -> pure (Right refreshed)
      case refreshedDeployCred of
        Left err -> pure (Left err)
        Right refreshedDeployCred' -> do
          let DeployCredential { host = deployHost, accessToken = deployAccessToken, repoId = deployRepoId } = refreshedDeployCred'
          deployTokenResult <- createProjectDeployToken cfg deployHost deployAccessToken deployRepoId
          case deployTokenResult of
            Left err -> pure (Left (renderGitlabError err))
            Right deployToken -> do
              existingNixConfig <- fmap (fmap T.pack) (lookupEnv "NIX_CONFIG")
              let scopedNixConfig = appendNixAccessTokenConfig existingNixConfig deployHost NixGitlabPAT deployToken.value
              let AppConfig { appWebhookConfig = webhookCfg } = cfg
              revokeErrRef <- newIORef Nothing
              result <-
                runWebhookWith
                  (runCommandWithEnv cfg [("NIX_CONFIG", scopedNixConfig)])
                  (loadPlan cfg)
                  webhookCfg
                  projectRef
                  `finally` do
                    revokeResult <- revokeProjectToken cfg deployHost deployAccessToken deployRepoId deployToken.id
                    case revokeResult of
                      Left msg -> writeIORef revokeErrRef (Just msg)
                      Right _ -> pure ()
              revokeErr <- readIORef revokeErrRef
              case chooseFinalResult result (fmap renderGitlabError revokeErr) of
                Left (Left err) ->
                  pure (Left (renderWebhookError err))
                Left (Right revokeMsg) ->
                  pure (Left revokeMsg)
                Right okResult ->
                  let WebhookResult { webhookOk = ok } = okResult
                   in if ok
                        then pure (Right "Webhook deployment completed")
                        else pure (Left "Webhook deployment reported one or more failed deploys")


-- Webhook signature resolution

data SecretInfo = SecretInfo
  { secretConfigured :: Bool
  , secretValue :: Maybe BS.ByteString
  }

verifyWebhook :: SecretInfo -> Maybe Text -> Maybe Text -> BL.ByteString -> Handler ()
verifyWebhook secretInfo mHubSig mGitlabToken rawBody = do
  let hasGitHub = mHubSig /= Nothing
  let hasGitLab = mGitlabToken /= Nothing
  case secretInfo.secretValue of
    Nothing ->
      if secretInfo.secretConfigured && (hasGitHub || hasGitLab)
        then throwError (errorWithBody err401 (ErrorResponse "webhook secret not configured" Nothing Nothing Nothing Nothing))
        else if secretInfo.secretConfigured
          then throwError (errorWithBody err401 (ErrorResponse "missing webhook signature" Nothing Nothing Nothing Nothing))
          else pure ()
    Just secret ->
      case (mHubSig, mGitlabToken) of
        (Nothing, Nothing) ->
          throwError (errorWithBody err401 (ErrorResponse "missing webhook signature" Nothing Nothing Nothing Nothing))
        _ -> do
          let githubOk = maybe False (\sigText -> verifyGitHubSignature secret rawBody (TE.encodeUtf8 sigText)) mHubSig
          let gitlabOk = maybe False (\tokText -> verifyGitLabToken secret (TE.encodeUtf8 tokText)) mGitlabToken
          unless (githubOk || gitlabOk) $
            throwError (errorWithBody err401 (ErrorResponse "invalid webhook signature" Nothing Nothing Nothing Nothing))

resolveSecret :: AppConfig -> Text -> ProjectRef -> IO SecretInfo
resolveSecret cfg hash ref = do
  let AppConfig
        { appDbConnString = dbConn
        , appWebhookSecretsDir = secretsDir
        , appWebhookSecretFile = secretFile
        } = cfg
  let configured = True
  dbSecret <- case dbConn of
    Nothing -> pure Nothing
    Just _ -> withDb cfg $ \conn -> do
      rows <- query conn "SELECT secret FROM webhooks JOIN projects ON webhooks.project_id = projects.id WHERE projects.default_env_hash = ?" (Only hash)
      pure (listToMaybe (map fromOnly rows))
  case dbSecret of
    Just secret -> pure (SecretInfo configured (Just (TE.encodeUtf8 secret)))
    Nothing -> do
      fromDir <- case secretsDir of
        Nothing -> pure Nothing
        Just dir -> do
          let ProjectRef { prOrg = org, prProject = project } = ref
          let byHash = dir </> T.unpack hash
          let byProject = dir </> T.unpack (org <> "__" <> project)
          pickFirstExisting [byHash, byProject]
      case fromDir of
        Just secret -> pure (SecretInfo configured (Just secret))
        Nothing ->
          case secretFile of
            Nothing -> pure (SecretInfo configured Nothing)
            Just path -> do
              secret <- readSecret path
              pure (SecretInfo configured (Just secret))


-- Webhook error -> HTTP

serverError :: WebhookError -> ServerError
serverError err =
  case err of
    WebhookPlanError msg -> errorWithBody err500 (ErrorResponse msg Nothing Nothing Nothing Nothing)
    WebhookCommandError cmdErr ->
      let CommandError spec exitCodeRaw stdoutText stderrText = cmdErr
          cmd = renderCommand spec
          exitCode = exitCodeToInt exitCodeRaw
          response = ErrorResponse "command failed" (Just cmd) exitCode (Just stdoutText) (Just stderrText)
       in errorWithBody err500 response

chooseFinalResult :: Either WebhookError WebhookResult -> Maybe Text -> Either (Either WebhookError Text) WebhookResult
chooseFinalResult primaryResult mRevokeError =
  case primaryResult of
    Left primaryErr -> Left (Left primaryErr)
    Right successResult ->
      case mRevokeError of
        Just revokeErr -> Left (Right revokeErr)
        Nothing -> Right successResult

renderWebhookError :: WebhookError -> Text
renderWebhookError err =
  case err of
    WebhookPlanError msg -> msg
    WebhookCommandError cmdErr -> commandErrorText cmdErr
