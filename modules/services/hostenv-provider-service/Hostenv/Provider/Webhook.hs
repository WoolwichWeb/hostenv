{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Webhook
  ( webhookHandler
  , loadPlan
  ) where

import Control.Concurrent.MVar (MVar, withMVar)
import Control.Exception (finally)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Only (..), fromOnly, query)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Servant

import Hostenv.Provider.Command (exitCodeToInt, renderCommand, runCommandWithEnv)
import Hostenv.Provider.Config (AppConfig(..))
import Hostenv.Provider.DB (ProjectDeployCredential(..), loadProjectDeployCredentialByHash, withDb)
import Hostenv.Provider.Gitlab (GitlabDeployToken(..), appendNixAccessTokenConfig, createProjectDeployToken, revokeProjectDeployToken)
import Hostenv.Provider.Http (ErrorResponse(..), errorWithBody)
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


loadPlan :: AppConfig -> IO BL.ByteString
loadPlan cfg =
  let AppConfig { appWebhookConfig = WebhookConfig { whPlanPath = planPath } } = cfg
   in BL.readFile planPath

webhookHandler :: MVar () -> AppConfig -> Text -> Maybe Text -> Maybe Text -> BL.ByteString -> Handler WebhookResult
webhookHandler webhookLock cfg hash mHubSig mGitlabToken rawBody = do
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
  deployCredResult <- liftIO $ withDb cfg (\conn -> loadProjectDeployCredentialByHash cfg conn hash)
  deployCred <-
    case deployCredResult of
      Left err -> throwError (errorWithBody err500 (ErrorResponse err Nothing Nothing Nothing Nothing))
      Right Nothing -> throwError (errorWithBody err500 (ErrorResponse "Missing project OAuth token for deploy" Nothing Nothing Nothing Nothing))
      Right (Just cred) -> pure cred
  deployTokenResult <- liftIO (createProjectDeployToken cfg deployCred.host deployCred.token deployCred.repoId)
  deployToken <-
    case deployTokenResult of
      Left err -> throwError (errorWithBody err500 (ErrorResponse err Nothing Nothing Nothing Nothing))
      Right token -> pure token
  existingNixConfig <- liftIO (fmap (fmap T.pack) (lookupEnv "NIX_CONFIG"))
  let scopedNixConfig = appendNixAccessTokenConfig existingNixConfig deployCred.host deployToken.deployTokenValue
  let AppConfig { appWebhookConfig = webhookCfg } = cfg
  revokeErrRef <- liftIO (newIORef Nothing)
  result <-
    liftIO $
      withMVar webhookLock $ \_ ->
        runWebhookWith
          (runCommandWithEnv cfg [("NIX_CONFIG", scopedNixConfig)])
          (loadPlan cfg)
          webhookCfg
          projectRef
          `finally` do
            revokeResult <- revokeProjectDeployToken cfg deployCred.host deployCred.token deployCred.repoId deployToken.deployTokenId
            case revokeResult of
              Left msg -> writeIORef revokeErrRef (Just msg)
              Right _ -> pure ()
  revokeErr <- liftIO (readIORef revokeErrRef)
  case revokeErr of
    Just err -> throwError (errorWithBody err500 (ErrorResponse err Nothing Nothing Nothing Nothing))
    Nothing -> pure ()
  case result of
    Left err -> throwError (serverError err)
    Right okResult ->
      let WebhookResult { webhookOk = ok } = okResult
       in if ok
            then pure okResult
            else throwError (errorWithBody err500 okResult)


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
