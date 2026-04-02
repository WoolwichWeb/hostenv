{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Hostenv.Provider.Server (
    runServer,
    app,
) where

import Control.Concurrent.STM (atomically, readTChan)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef)
import qualified Data.Map.Strict as Map
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status403, status404)
import Network.Wai (Application)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setBeforeMainLoop)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Servant

import Hostenv.Provider.Config (AppConfig (..), DeployConfig (..))
import Hostenv.Provider.DB (loadDeployActions, loadDeployActionsByNode, loadLatestDeployIntentForNode)
import Hostenv.Provider.DeployApi (NodeEvent, acceptsNodeEvents, backupSnapshotHandler, dispatchFingerprint, dispatchForNode, eventHandler, intentByJobHandler, intentByShaHandler, jobActionsHandler, jobStatusHandler, jobStatusesHandler, shouldDispatchJob, validateIntent)
import Hostenv.Provider.Jobs (JobRuntime, duplicateBroadcastChannel, jobSummaryStatus, loadJobById, markJobFailedFromDeploy, markJobSucceededFromDeploy, publishJobUpdate, startJobRuntime)
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
    let AppConfig{appListenSocket = listenSocket} = cfg
    repoStatusRef <- newIORef initialRepoStatus
    jobRuntime <- startJobRuntime cfg
    sock <- openUnixSocket listenSocket
    let settings = setBeforeMainLoop (putStrLn "hostenv-provider-service: listening") defaultSettings
    runSettingsSocket settings sock (app jobRuntime repoStatusRef cfg)

app :: JobRuntime -> IORef RepoStatus -> AppConfig -> Application
app jobRuntime repoStatusRef cfg req respond = do
    -- Debug logging for request tracing
    let path = Wai.pathInfo req
        rawPath = Wai.rawPathInfo req
        method = Wai.requestMethod req
        headers = Wai.requestHeaders req
    putStrLn $ "DEBUG: Request " ++ show method ++ " " ++ show rawPath ++ " pathInfo=" ++ show path
    putStrLn $ "DEBUG: Headers " ++ show headers
    -- Upgrade requests to /api/deploy-jobs/ws to use a websocket.
    if path == ["api", "deploy-jobs", "ws"]
        then deployWsApp jobRuntime cfg req respond
        else serve api (server jobRuntime repoStatusRef cfg) req respond

deployWsApp :: JobRuntime -> AppConfig -> Application
deployWsApp runtime cfg req respond = do
    putStrLn $ "DEBUG: deployWsApp called"
    case requestedNode req of
        Nothing -> do
            putStrLn $ "DEBUG: No node parameter found, returning 403"
            respond (Wai.responseLBS status403 [("Content-Type", "text/plain")] "forbidden")
        Just nodeName -> do
            putStrLn $ "DEBUG: Node parameter found: " ++ show nodeName
            let fallbackResponse = Wai.responseLBS status404 [("Content-Type", "text/plain")] "websocket required"
             in websocketsOr
                    WS.defaultConnectionOptions
                    (deploySocketServer runtime cfg nodeName)
                    ( \_ respondFallback -> do
                        putStrLn "DEBUG: WebSocket fallback triggered (not a WS upgrade)"
                        respondFallback fallbackResponse
                    )
                    req
                    respond

deploySocketServer :: JobRuntime -> AppConfig -> Text -> WS.ServerApp
deploySocketServer runtime cfg nodeName pending = do
    connection <- WS.acceptRequest pending
    firstMessage <- WS.receiveData connection
    if not (isValidWsAuth cfg nodeName firstMessage)
        then WS.sendClose connection ("forbidden" :: Text)
        else do
            initialDispatchId <- sendLatestDeployJob cfg nodeName connection Nothing
            WS.sendTextData connection (A.encode (A.object ["type" A..= ("deploy_hint" :: Text), "node" A..= nodeName]))
            channel <- duplicateBroadcastChannel runtime
            let loop mLastDispatchId = do
                    _ <- atomically (readTChan channel)
                    nextDispatchId <- sendLatestDeployJob cfg nodeName connection mLastDispatchId
                    WS.sendTextData connection (A.encode (A.object ["type" A..= ("deploy_hint" :: Text), "node" A..= nodeName]))
                    loop nextDispatchId
            loop initialDispatchId

sendLatestDeployJob :: AppConfig -> Text -> WS.Connection -> Maybe Text -> IO (Maybe Text)
sendLatestDeployJob cfg nodeName connection mLastDispatchId = do
    mLatest <- loadLatestDeployIntentForNode cfg nodeName
    case mLatest of
        Nothing -> pure mLastDispatchId
        Just (jobId, commitSha, payload) ->
            do
                mJob <- loadJobById cfg jobId
                case mJob of
                    Nothing -> pure mLastDispatchId
                    Just job | not (acceptsNodeEvents (jobSummaryStatus job)) -> pure mLastDispatchId
                    Just _ ->
                        case validateIntent payload of
                            Nothing -> pure mLastDispatchId
                            Just validated -> do
                                allJobActions <- loadDeployActions cfg jobId
                                nodeActions <- loadDeployActionsByNode cfg jobId nodeName
                                case dispatchForNode validated allJobActions nodeName of
                                    Nothing -> pure mLastDispatchId
                                    Just (filteredIntent, filteredActions) -> do
                                        let dispatchId = dispatchFingerprint jobId filteredIntent filteredActions
                                        if not (shouldDispatchJob validated nodeActions filteredActions mLastDispatchId dispatchId)
                                            then pure mLastDispatchId
                                            else do
                                                WS.sendTextData
                                                    connection
                                                    ( A.encode
                                                        ( A.object
                                                            [ "type" A..= ("deploy_job" :: Text)
                                                            , "jobId" A..= jobId
                                                            , "commitSha" A..= commitSha
                                                            , "node" A..= nodeName
                                                            , "intent" A..= filteredIntent
                                                            , "actions" A..= filteredActions
                                                            ]
                                                        )
                                                    )
                                                pure (Just dispatchId)

requestedNode :: Wai.Request -> Maybe Text
requestedNode req =
    case lookup "node" (Wai.queryString req) of
        Just (Just value) ->
            case TE.decodeUtf8' value of
                Left _ -> Nothing
                Right decoded ->
                    let txt = T.strip decoded
                     in if txt == "" then Nothing else Just txt
        _ -> Nothing

isValidWsAuth :: AppConfig -> Text -> BL.ByteString -> Bool
isValidWsAuth cfg nodeName rawMessage =
    case A.decode rawMessage of
        Just (A.Object obj) ->
            case (KM.lookup "token" obj, KM.lookup "node" obj) of
                (Just (A.String supplied), Just (A.String suppliedNode))
                    | suppliedNode == nodeName ->
                        case Map.lookup nodeName cfg.appDeploy.nodeAuthTokens of
                            Just expected -> expected == T.strip supplied
                            Nothing -> False
                _ -> False
        _ -> False

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
