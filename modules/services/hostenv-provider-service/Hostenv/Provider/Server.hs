{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Hostenv.Provider.Server (
    runServer,
    app,
) where

import Control.Concurrent.Async (waitAnyCatchCancel, withAsync)
import Control.Exception (finally)
import Control.Concurrent.STM (TChan, TQueue, atomically, newTQueueIO, orElse, readTChan, readTQueue, writeTQueue)
import qualified Data.Aeson as A
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef)
import Data.Maybe (catMaybes)
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Unique (hashUnique, newUnique)
import Network.HTTP.Types (status403, status404)
import Network.Wai (Application)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setBeforeMainLoop)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Servant

import Hostenv.Provider.Config (AppConfig (..))
import Hostenv.Provider.DB (deployActionId, loadDeployActions, loadDeployActionsByNode, loadLatestDeployIntentForNode)
import Hostenv.Provider.DeployApi (NodeEvent(..), acceptsNodeEvents, backupSnapshotHandler, buildDeployJobEnvelope, dispatchFingerprint, dispatchForNode, dispatchStableId, eventHandler, intentByJobHandler, intentByShaHandler, isValidDeployWsAuth, jobActionsHandler, jobStatusHandler, jobStatusesHandler, processNodeEvent, shouldDispatchJob, validateIntent)
import Hostenv.Provider.Jobs (JobRuntime, duplicateBroadcastChannel, jobSummaryStatus, loadJobById, markJobFailedFromDeploy, markJobSucceededFromDeploy, publishJobUpdate, startJobRuntime)
import Hostenv.Provider.Logging (ProviderLogFields(..), ProviderSeverity (..), logProviderEvent, providerLogFields)
import Hostenv.Provider.Repo (RepoStatus, openUnixSocket)
import Hostenv.Provider.UI.Router (uiApp)
import Hostenv.Provider.Webhook (WebhookAccepted, webhookHandler)

data DeployWsInbound
    = DeployWsResume Text A.Value
    | DeployWsNodeEvent NodeEvent
    | DeployWsClientMessage Text (Maybe Text)

data DeployWsDispatchTrigger
    = DispatchTriggerBroadcast

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
    let settings =
            setBeforeMainLoop
                (logProviderEvent (providerLogFields "provider_service_listening" ProviderSeverityInfo) ["listen_socket" A..= listenSocket])
                defaultSettings
    runSettingsSocket settings sock (app jobRuntime repoStatusRef cfg)

app :: JobRuntime -> IORef RepoStatus -> AppConfig -> Application
app jobRuntime repoStatusRef cfg req respond = do
    requestId <- nextRequestId
    let path = Wai.pathInfo req
        routeDecision = if path == ["api", "deploy-jobs", "ws"] then "websocket" else "http"
    if isDeployRequest path
        then
            logProviderEvent
                ( (providerLogFields "deploy_request_received" ProviderSeverityInfo)
                    { entryRequestId = Just requestId
                    , entryNode = deployRequestNode req
                    , entryProtocolVersion = deployRequestProtocolVersion path
                    , entryDecision = Just routeDecision
                    , entryReason = Just "incoming_request"
                    }
                )
                [ "http_method" A..= decodeRequestMethod (Wai.requestMethod req)
                , "path" A..= T.intercalate "/" path
                ]
        else pure ()
    if path == ["api", "deploy-jobs", "ws"]
        then deployWsApp jobRuntime cfg requestId req respond
        else serve api (server jobRuntime repoStatusRef cfg) req respond

deployWsApp :: JobRuntime -> AppConfig -> Text -> Application
deployWsApp runtime cfg requestId req respond = do
    case requestedNode req of
        Nothing -> do
            logProviderEvent
                ( (providerLogFields "deploy_ws_connection_rejected" ProviderSeverityWarn)
                    { entryRequestId = Just requestId
                    , entryProtocolVersion = Just 1
                    , entryDecision = Just "reject"
                    , entryReason = Just "missing_node"
                    }
                )
                []
            respond (Wai.responseLBS status403 [("Content-Type", "text/plain")] "forbidden")
        Just nodeName -> do
            let fallbackResponse = Wai.responseLBS status404 [("Content-Type", "text/plain")] "websocket required"
             in websocketsOr
                    WS.defaultConnectionOptions
                    (deploySocketServer runtime cfg requestId nodeName)
                    ( \_ respondFallback -> do
                        logProviderEvent
                            ( (providerLogFields "deploy_ws_connection_rejected" ProviderSeverityWarn)
                                { entryRequestId = Just requestId
                                , entryNode = Just nodeName
                                , entryProtocolVersion = Just 1
                                , entryDecision = Just "reject"
                                , entryReason = Just "websocket_required"
                                }
                            )
                            []
                        respondFallback fallbackResponse
                    )
                    req
                    respond

deploySocketServer :: JobRuntime -> AppConfig -> Text -> Text -> WS.ServerApp
deploySocketServer runtime cfg requestId nodeName pending = do
    connection <- WS.acceptRequest pending
    logProviderEvent
        ( (providerLogFields "deploy_ws_connection_opened" ProviderSeverityInfo)
            { entryRequestId = Just requestId
            , entryNode = Just nodeName
            , entryProtocolVersion = Just 1
            , entryDecision = Just "accept"
            , entryReason = Just "connection_opened"
            }
        )
        []
    flip finally
        ( logProviderEvent
            ( (providerLogFields "deploy_ws_connection_closed" ProviderSeverityInfo)
                { entryRequestId = Just requestId
                , entryNode = Just nodeName
                , entryProtocolVersion = Just 1
                , entryDecision = Just "close"
                , entryReason = Just "connection_closed"
                }
            )
            []
        ) $ do
            firstMessage <- WS.receiveData connection
            if not (isValidDeployWsAuth cfg nodeName firstMessage)
                then do
                    logProviderEvent
                        ( (providerLogFields "deploy_ws_auth" ProviderSeverityWarn)
                            { entryRequestId = Just requestId
                            , entryNode = Just nodeName
                            , entryProtocolVersion = Just 1
                            , entryDecision = Just "reject"
                            , entryReason = Just "forbidden"
                            }
                        )
                        []
                    sendProtocolMessage connection "auth_error" nodeName Nothing Nothing Nothing (A.object ["reason" A..= ("forbidden" :: Text)])
                    WS.sendClose connection ("forbidden" :: Text)
                else do
                    logProviderEvent
                        ( (providerLogFields "deploy_ws_auth" ProviderSeverityInfo)
                            { entryRequestId = Just requestId
                            , entryNode = Just nodeName
                            , entryProtocolVersion = Just 1
                            , entryDecision = Just "accept"
                            , entryReason = Just "authenticated"
                            }
                        )
                        []
                    inboundQueue <- newTQueueIO
                    outboundQueue <- newTQueueIO
                    dispatchQueue <- newTQueueIO
                    channel <- duplicateBroadcastChannel runtime
                    enqueueProtocolMessage outboundQueue "auth_ok" nodeName Nothing Nothing Nothing (A.object [])
                    withAsync (runDeployWsReceiver requestId nodeName connection inboundQueue) $ \receiverAsync ->
                        withAsync (runDeployWsSender requestId nodeName connection outboundQueue) $ \senderAsync ->
                            withAsync (runDeployWsBroadcastListener channel dispatchQueue) $ \broadcastAsync ->
                                withAsync (runDeployWsDispatcher runtime cfg requestId nodeName outboundQueue inboundQueue dispatchQueue) $ \dispatcherAsync -> do
                                    outcome <- waitAnyCatchCancel [receiverAsync, senderAsync, broadcastAsync, dispatcherAsync]
                                    case snd outcome of
                                        Left err -> fail (show err)
                                        Right () -> pure ()

runDeployWsReceiver :: Text -> Text -> WS.Connection -> TQueue DeployWsInbound -> IO ()
runDeployWsReceiver requestId nodeName connection inboundQueue =
    let loop = do
            raw <- WS.receiveData connection
            case decodeDeployWsInbound raw of
                Nothing -> do
                    logProviderEvent
                        ( (providerLogFields "deploy_ws_client_message" ProviderSeverityWarn)
                            { entryRequestId = Just requestId
                            , entryNode = Just nodeName
                            , entryProtocolVersion = Just 1
                            , entryDecision = Just "ignore"
                            , entryReason = Just "malformed_payload"
                            }
                        )
                        (inboundLogFields raw)
                    loop
                Just inbound -> do
                    logProviderEvent
                        ( (providerLogFields "deploy_ws_client_message" ProviderSeverityInfo)
                            { entryRequestId = Just requestId
                            , entryNode = Just nodeName
                            , entryProtocolVersion = Just 1
                            , entryDecision = Just "queue"
                            , entryReason = Just (inboundReason inbound)
                            }
                        )
                        (inboundLogFields raw)
                    atomically (writeTQueue inboundQueue inbound)
                    loop
     in loop

runDeployWsSender :: Text -> Text -> WS.Connection -> TQueue BL.ByteString -> IO ()
runDeployWsSender requestId nodeName connection outboundQueue =
    let loop = do
            payload <- atomically (readTQueue outboundQueue)
            logProviderEvent
                ( (providerLogFields "deploy_ws_server_message" ProviderSeverityInfo)
                    { entryRequestId = Just requestId
                    , entryNode = Just nodeName
                    , entryProtocolVersion = Just 1
                    , entryDecision = Just "send"
                    , entryReason = Just "server_message"
                    }
                )
                (outboundLogFields payload)
            WS.sendTextData connection payload
            loop
     in loop

runDeployWsBroadcastListener :: TChan A.Value -> TQueue DeployWsDispatchTrigger -> IO ()
runDeployWsBroadcastListener channel dispatchQueue =
    let loop = do
            _ <- atomically (readTChan channel)
            atomically (writeTQueue dispatchQueue DispatchTriggerBroadcast)
            loop
     in loop

runDeployWsDispatcher :: JobRuntime -> AppConfig -> Text -> Text -> TQueue BL.ByteString -> TQueue DeployWsInbound -> TQueue DeployWsDispatchTrigger -> IO ()
runDeployWsDispatcher runtime cfg requestId nodeName outboundQueue inboundQueue dispatchQueue =
    let loop mLastDispatchId isResumed = do
            event <- atomically $ (Left <$> readTQueue inboundQueue) `orElse` (Right <$> readTQueue dispatchQueue)
            case event of
                Left inbound ->
                    case inbound of
                        DeployWsResume messageId payload -> do
                            logProviderEvent
                                ( (providerLogFields "deploy_ws_client_message" ProviderSeverityInfo)
                                    { entryRequestId = Just requestId
                                    , entryNode = Just nodeName
                                    , entryProtocolVersion = Just 1
                                    , entryDecision = Just "accept"
                                    , entryReason = Just "resume_received"
                                    }
                                )
                                ["message_id" A..= messageId, "payload" A..= payload]
                            nextDispatchId <- sendLatestDeployJob cfg requestId nodeName (enqueueJson outboundQueue) Nothing
                            loop nextDispatchId True
                        DeployWsNodeEvent event -> do
                            let NodeEvent { jobId = eventJobId } = event
                            _ <- processNodeEvent cfg (publishJobUpdate runtime) (markJobFailedFromDeploy runtime) (markJobSucceededFromDeploy runtime) eventJobId event
                            loop mLastDispatchId isResumed
                        DeployWsClientMessage kindText mMessageId -> do
                            logProviderEvent
                                ( (providerLogFields "deploy_ws_client_message" ProviderSeverityInfo)
                                    { entryRequestId = Just requestId
                                    , entryNode = Just nodeName
                                    , entryProtocolVersion = Just 1
                                    , entryDecision = Just "ignore"
                                    , entryReason = Just "unsupported_client_message"
                                    }
                                )
                                (["kind" A..= kindText] <> catMaybes [fmap ("message_id" A..=) mMessageId])
                            loop mLastDispatchId isResumed
                Right DispatchTriggerBroadcast ->
                    if isResumed
                        then do
                            logProviderEvent
                                ( (providerLogFields "deploy_dispatch" ProviderSeverityInfo)
                                    { entryRequestId = Just requestId
                                    , entryNode = Just nodeName
                                    , entryProtocolVersion = Just 1
                                    , entryDecision = Just "evaluate"
                                    , entryReason = Just "broadcast_received"
                                    }
                                )
                                []
                            nextDispatchId <- sendLatestDeployJob cfg requestId nodeName (enqueueJson outboundQueue) mLastDispatchId
                            loop nextDispatchId isResumed
                        else do
                            logProviderEvent
                                ( (providerLogFields "deploy_dispatch" ProviderSeverityInfo)
                                    { entryRequestId = Just requestId
                                    , entryNode = Just nodeName
                                    , entryProtocolVersion = Just 1
                                    , entryDecision = Just "skip"
                                    , entryReason = Just "waiting_for_resume"
                                    }
                                )
                                []
                            loop mLastDispatchId isResumed
     in loop Nothing False

enqueueJson :: TQueue BL.ByteString -> A.Value -> IO ()
enqueueJson outboundQueue value =
    atomically (writeTQueue outboundQueue (A.encode value))

enqueueProtocolMessage :: TQueue BL.ByteString -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> A.Value -> IO ()
enqueueProtocolMessage outboundQueue kindText nodeName mJobId mDispatchId mActionId payload = do
    now <- getCurrentTime
    enqueueJson outboundQueue (protocolEnvelope now kindText nodeName mJobId mDispatchId mActionId payload)

decodeDeployWsInbound :: BL.ByteString -> Maybe DeployWsInbound
decodeDeployWsInbound raw =
    case A.decode raw of
        Just (A.Object obj) -> do
            version <- lookupInt "version" obj
            kindText <- lookupText "kind" obj
            nodeValue <- lookupText "node" obj
            payload <- KM.lookup "payload" obj
            if version /= 1 || nodeValue == ""
                then Nothing
                else case kindText of
                    "resume" -> DeployWsResume <$> lookupText "messageId" obj <*> pure payload
                    _ ->
                        case A.fromJSON (A.Object obj) of
                            A.Success nodeEvent -> Just (DeployWsNodeEvent nodeEvent)
                            A.Error _ -> Just (DeployWsClientMessage kindText (lookupMaybeText "messageId" obj))
        _ -> Nothing

lookupText :: Text -> KM.KeyMap A.Value -> Maybe Text
lookupText key obj =
    case KM.lookup (K.fromText key) obj of
        Just (A.String value) -> Just (T.strip value)
        _ -> Nothing

lookupMaybeText :: Text -> KM.KeyMap A.Value -> Maybe Text
lookupMaybeText = lookupText

lookupInt :: Text -> KM.KeyMap A.Value -> Maybe Int
lookupInt key obj =
    case KM.lookup (K.fromText key) obj of
        Just value ->
            case A.fromJSON value of
                A.Success intValue -> Just intValue
                A.Error _ -> Nothing
        _ -> Nothing

inboundReason :: DeployWsInbound -> Text
inboundReason inbound =
    case inbound of
        DeployWsResume{} -> "resume_received"
        DeployWsNodeEvent{} -> "node_event_received"
        DeployWsClientMessage{} -> "client_message_received"

inboundLogFields :: BL.ByteString -> [Pair]
inboundLogFields raw =
    ["raw_size" A..= BL.length raw] <> envelopeMetaFields raw <> rawPreviewField raw

outboundLogFields :: BL.ByteString -> [Pair]
outboundLogFields raw =
    ["raw_size" A..= BL.length raw] <> envelopeMetaFields raw <> rawPreviewField raw

envelopeMetaFields :: BL.ByteString -> [Pair]
envelopeMetaFields raw =
    case A.decode raw of
        Just (A.Object obj) ->
            catMaybes
                [ fmap ("kind" A..=) (lookupMaybeText "kind" obj)
                , fmap ("message_id" A..=) (lookupMaybeText "messageId" obj)
                , fmap ("job_id" A..=) (lookupMaybeText "jobId" obj)
                , fmap ("dispatch_id" A..=) (lookupMaybeText "dispatchId" obj)
                , fmap ("action_id" A..=) (lookupMaybeText "actionId" obj)
                ]
        _ -> []

rawPreviewField :: BL.ByteString -> [Pair]
rawPreviewField raw =
    let preview = T.pack (take 256 (BLC.unpack raw))
     in if preview == "" then [] else ["raw_preview" A..= preview]


sendLatestDeployJob :: AppConfig -> Text -> Text -> (A.Value -> IO ()) -> Maybe Text -> IO (Maybe Text)
sendLatestDeployJob cfg requestId nodeName sendValue mLastDispatchId = do
    mLatest <- loadLatestDeployIntentForNode cfg nodeName
    case mLatest of
        Nothing -> do
            logDispatchSkip requestId Nothing nodeName Nothing Nothing "no_intent"
            pure mLastDispatchId
        Just (jobId, commitSha, payload) ->
            do
                mJob <- loadJobById cfg jobId
                case mJob of
                    Nothing -> do
                        logDispatchSkip requestId (Just jobId) nodeName Nothing Nothing "job_missing"
                        pure mLastDispatchId
                    Just job | not (acceptsNodeEvents (jobSummaryStatus job)) -> do
                        logDispatchSkip requestId (Just jobId) nodeName Nothing (Just (jobSummaryStatus job)) "job_not_waiting"
                        pure mLastDispatchId
                    Just _ ->
                        case validateIntent payload of
                            Nothing -> do
                                logDispatchSkip requestId (Just jobId) nodeName Nothing Nothing "invalid_intent"
                                pure mLastDispatchId
                            Just validated -> do
                                allJobActions <- loadDeployActions cfg jobId
                                nodeActions <- loadDeployActionsByNode cfg jobId nodeName
                                case dispatchForNode validated allJobActions nodeName of
                                    Nothing -> do
                                        logDispatchSkip requestId (Just jobId) nodeName Nothing Nothing "node_not_targeted"
                                        pure mLastDispatchId
                                    Just (filteredIntent, filteredActions) -> do
                                        let dispatchFingerprintValue = dispatchFingerprint jobId filteredIntent filteredActions
                                            dispatchId = dispatchStableId jobId nodeName filteredIntent filteredActions
                                        if not (shouldDispatchJob validated nodeActions filteredActions mLastDispatchId dispatchFingerprintValue)
                                            then do
                                                logDispatchSkip requestId (Just jobId) nodeName (Just dispatchId) Nothing (dispatchSkipReason mLastDispatchId dispatchFingerprintValue)
                                                pure mLastDispatchId
                                            else do
                                                now <- getCurrentTime
                                                sendValue
                                                    ( buildDeployJobEnvelope
                                                        now
                                                        (protocolMessageId "deploy_job" nodeName (Just jobId) (Just dispatchId) Nothing)
                                                        jobId
                                                        commitSha
                                                        nodeName
                                                        dispatchId
                                                        filteredIntent
                                                        filteredActions
                                                    )
                                                logProviderEvent
                                                    ( (providerLogFields "deploy_dispatch" ProviderSeverityInfo)
                                                        { entryRequestId = Just requestId
                                                        , entryJobId = Just jobId
                                                        , entryNode = Just nodeName
                                                        , entryDispatchId = Just dispatchId
                                                        , entryProtocolVersion = Just 1
                                                        , entryDecision = Just "send"
                                                        , entryReason = Just "dispatch_ready"
                                                        }
                                                    )
                                                    [ "commit_sha" A..= commitSha
                                                    , "action_count" A..= length filteredActions
                                                    , "intent_action_count" A..= intentActionCount filteredIntent
                                                    , "intent_missing_action_id_count" A..= intentMissingActionIdCount filteredIntent
                                                    , "db_action_ids" A..= map deployActionId filteredActions
                                                    ]
                                                pure (Just dispatchFingerprintValue)

intentActionCount :: A.Value -> Int
intentActionCount intentValue =
    case intentValue of
        A.Object obj ->
            case KM.lookup "actions" obj of
                Just (A.Array values) -> length values
                _ -> 0
        _ -> 0

intentMissingActionIdCount :: A.Value -> Int
intentMissingActionIdCount intentValue =
    case intentValue of
        A.Object obj ->
            case KM.lookup "actions" obj of
                Just (A.Array values) -> length [() | value <- toList values, not (hasActionId value)]
                _ -> 0
        _ -> 0

hasActionId :: A.Value -> Bool
hasActionId value =
    case value of
        A.Object obj ->
            case KM.lookup "actionId" obj of
                Just (A.String txt) -> T.strip txt /= ""
                _ -> False
        _ -> False

sendProtocolMessage :: WS.Connection -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> A.Value -> IO ()
sendProtocolMessage connection kindText nodeName mJobId mDispatchId mActionId payload = do
    now <- getCurrentTime
    WS.sendTextData connection (A.encode (protocolEnvelope now kindText nodeName mJobId mDispatchId mActionId payload))

protocolEnvelope :: A.ToJSON a => a -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> A.Value -> A.Value
protocolEnvelope timestampValue kindText nodeName mJobId mDispatchId mActionId payload =
    A.object
        ( [ "version" A..= (1 :: Int)
          , "kind" A..= kindText
          , "messageId" A..= protocolMessageId kindText nodeName mJobId mDispatchId mActionId
          , "timestamp" A..= timestampValue
          , "node" A..= nodeName
          , "payload" A..= payload
          ]
            <> catMaybes
                [ fmap ("jobId" A..=) mJobId
                , fmap ("dispatchId" A..=) mDispatchId
                , fmap ("actionId" A..=) mActionId
                ]
        )

protocolMessageId :: Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text
protocolMessageId kindText nodeName mJobId mDispatchId mActionId =
    T.intercalate ":" (kindText : nodeName : catMaybes [mJobId, mDispatchId, mActionId])

nextRequestId :: IO Text
nextRequestId = do
    unique <- newUnique
    pure ("req-" <> T.pack (show (hashUnique unique)))

decodeRequestMethod :: BS.ByteString -> Text
decodeRequestMethod = TE.decodeUtf8

isDeployRequest :: [Text] -> Bool
isDeployRequest path =
    case path of
        ["webhook", _] -> True
        ["api", "deploy-jobs", _] -> True
        ["api", "deploy-jobs", _, _] -> True
        ["api", "deploy-intents", "by-sha"] -> True
        _ -> False

deployRequestProtocolVersion :: [Text] -> Maybe Int
deployRequestProtocolVersion path =
    case path of
        ["api", "deploy-jobs", "ws"] -> Just 1
        ["api", "deploy-jobs", _, "events"] -> Just 1
        _ -> Nothing

deployRequestNode :: Wai.Request -> Maybe Text
deployRequestNode req =
    case requestedNode req of
        Just nodeName -> Just nodeName
        Nothing -> queryTextParam "node" req

queryTextParam :: BS.ByteString -> Wai.Request -> Maybe Text
queryTextParam name req =
    case lookup name (Wai.queryString req) of
        Just (Just value) ->
            case TE.decodeUtf8' value of
                Left _ -> Nothing
                Right decoded ->
                    let trimmed = T.strip decoded
                     in if trimmed == "" then Nothing else Just trimmed
        _ -> Nothing

logDispatchSkip :: Text -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> Text -> IO ()
logDispatchSkip requestId mJobId nodeName mDispatchId mStatus reasonText =
    logProviderEvent
        ( (providerLogFields "deploy_dispatch" ProviderSeverityInfo)
            { entryRequestId = Just requestId
            , entryJobId = mJobId
            , entryNode = Just nodeName
            , entryDispatchId = mDispatchId
            , entryStatus = mStatus
            , entryProtocolVersion = Just 1
            , entryDecision = Just "skip"
            , entryReason = Just reasonText
            }
        )
        []

dispatchSkipReason :: Maybe Text -> Text -> Text
dispatchSkipReason mLastDispatchId dispatchFingerprintValue =
    if mLastDispatchId == Just dispatchFingerprintValue
        then "unchanged_dispatch"
        else "no_dispatchable_payload"

requestedNode :: Wai.Request -> Maybe Text
requestedNode req =
    queryTextParam "node" req

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
