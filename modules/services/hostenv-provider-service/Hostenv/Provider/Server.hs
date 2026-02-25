{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Hostenv.Provider.Server
  ( runServer
  , app
  ) where

import Data.IORef (IORef, newIORef)
import qualified Data.ByteString.Lazy as BL
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import qualified Data.Aeson as A
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setBeforeMainLoop)
import Servant

import Hostenv.Provider.Config (AppConfig(..))
import Hostenv.Provider.DeployApi (NodeEvent, backupSnapshotHandler, eventHandler, intentByJobHandler, intentByShaHandler, jobActionsHandler, jobStatusHandler, jobStatusesHandler)
import Hostenv.Provider.Jobs (JobRuntime, markJobFailedFromDeploy, markJobSucceededFromDeploy, publishJobUpdate, startJobRuntime)
import Hostenv.Provider.Repo (RepoStatus, openUnixSocket)
import Hostenv.Provider.UI.Router (uiApp)
import Hostenv.Provider.Webhook (WebhookAccepted, webhookHandler)


type API =
  "webhook"
    :> Capture "hash" Text
    :> Header "X-Hub-Signature-256" Text
    :> Header "X-Gitlab-Token" Text
    :> ReqBody '[OctetStream] BL.ByteString
    :> PostAccepted '[JSON] WebhookAccepted
    :<|> "api"
      :> "deploy-jobs"
      :> Capture "jobId" Text
      :> "intents"
      :> Capture "node" Text
      :> Header "Authorization" Text
      :> Get '[JSON] A.Value
    :<|> "api"
      :> "deploy-jobs"
      :> Capture "jobId" Text
      :> "status"
      :> QueryParam' '[Required] "node" Text
      :> Header "Authorization" Text
      :> Get '[JSON] A.Value
    :<|> "api"
      :> "deploy-jobs"
      :> Capture "jobId" Text
      :> "statuses"
      :> QueryParam' '[Required] "node" Text
      :> Header "Authorization" Text
      :> Get '[JSON] A.Value
    :<|> "api"
      :> "deploy-jobs"
      :> Capture "jobId" Text
      :> "actions"
      :> QueryParam' '[Required] "node" Text
      :> Header "Authorization" Text
      :> Get '[JSON] A.Value
    :<|> "api"
      :> "deploy-jobs"
      :> Capture "jobId" Text
      :> "backup-snapshot"
      :> QueryParam' '[Required] "node" Text
      :> QueryParam' '[Required] "source" Text
      :> QueryParam' '[Required] "user" Text
      :> Header "Authorization" Text
      :> Get '[JSON] A.Value
    :<|> "api"
      :> "deploy-jobs"
      :> Capture "jobId" Text
      :> "events"
      :> Header "Authorization" Text
      :> ReqBody '[JSON] NodeEvent
      :> Post '[JSON] NoContent
    :<|> "api"
      :> "deploy-intents"
      :> "by-sha"
      :> QueryParam' '[Required] "sha" Text
      :> QueryParam' '[Required] "node" Text
      :> Header "Authorization" Text
      :> Get '[JSON] A.Value
    :<|> Raw

api :: Proxy API
api = Proxy

runServer :: AppConfig -> RepoStatus -> IO ()
runServer cfg initialRepoStatus = do
  let AppConfig { appListenSocket = listenSocket } = cfg
  repoStatusRef <- newIORef initialRepoStatus
  jobRuntime <- startJobRuntime cfg
  sock <- openUnixSocket listenSocket
  let settings = setBeforeMainLoop (putStrLn "hostenv-provider-service: listening") defaultSettings
  runSettingsSocket settings sock (app jobRuntime repoStatusRef cfg)

app :: JobRuntime -> IORef RepoStatus -> AppConfig -> Application
app jobRuntime repoStatusRef cfg = serve api (server jobRuntime repoStatusRef cfg)

server :: JobRuntime -> IORef RepoStatus -> AppConfig -> Server API
server jobRuntime repoStatusRef cfg =
  webhookHandler jobRuntime repoStatusRef cfg
    :<|> intentByJobHandler cfg
    :<|> jobStatusHandler cfg
    :<|> jobStatusesHandler cfg
    :<|> jobActionsHandler cfg
    :<|> backupSnapshotHandler cfg
    :<|> eventHandler cfg (publishJobUpdate jobRuntime) (markJobFailedFromDeploy jobRuntime) (markJobSucceededFromDeploy jobRuntime)
    :<|> intentByShaHandler cfg
    :<|> Tagged (uiApp jobRuntime repoStatusRef cfg)
