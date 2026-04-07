module Hostenv.Provider.DeployAgent.Transport.WebSocket
  ( IncomingMessage(..)
  , SessionConnection(..)
  , SessionHooks(..)
  , SessionRuntime(..)
  , WebSocketConfig(..)
  , buildWebSocketConfig
  , decodeIncomingMessage
  , defaultSessionHooks
  , deriveWebSocketUrl
  , runSessionOverConnection
  , runWebSocketSession
  ) where

import Control.Concurrent.Async (waitAnyCatchCancel, withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (bracket_, throwIO)
import Control.Monad (forever)
import qualified Data.Aeson as A
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Hostenv.Provider.DeployAgent.Config (AgentConfig(..))
import Hostenv.Provider.DeployAgent.Dispatcher (IncomingMessage(..), SessionRuntime(..), runIncomingDispatcher)
import Hostenv.Provider.DeployAgent.Logging (Logger, logDebug, logInfo, logWarn)
import Hostenv.Provider.DeployAgent.Protocol
  ( DeployJob(..)
   , WsEnvelope(..)
   , WsMessageKind(..)
   , buildWsAuthPayload
   , buildWsResumePayload
   , decodeDeployJobMessage
   , decodeWsEnvelopeMessage
   )
import Hostenv.Provider.DeployAgent.State (AgentState, resumePayload)
import Network.URI (URI(..), URIAuth(..), parseURI)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection.PingPong as PingPong
import qualified Wuss

data WebSocketConfig = WebSocketConfig
  { url :: Text
  , nodeName :: Text
  , authToken :: Text
  , reconnectSeconds :: Int
  , buildSessionId :: IO Text
  , buildResumePayload :: IO A.Value
  , onAuthenticated :: Text -> WsEnvelope -> IO ()
  , onSessionActivity :: Text -> IO ()
  }

buildWebSocketConfig :: AgentConfig -> Text -> IO AgentState -> (Text -> WsEnvelope -> IO ()) -> (Text -> IO ()) -> Either Text WebSocketConfig
buildWebSocketConfig cfg token readState onAuthenticated onSessionActivity = do
  url <- deriveWebSocketUrl cfg.providerApiBaseUrl cfg.nodeName
  pure
    WebSocketConfig
      { url
      , nodeName = cfg.nodeName
      , authToken = token
      , reconnectSeconds = cfg.reconnectSeconds
      , buildSessionId = buildWebSocketSessionId cfg.nodeName
      , buildResumePayload = buildSessionResumePayload cfg.nodeName readState
      , onAuthenticated
      , onSessionActivity
      }

deriveWebSocketUrl :: Text -> Text -> Either Text Text
deriveWebSocketUrl apiBase nodeName
  | Just suffix <- T.stripPrefix "https://" apiBase = Right ("wss://" <> suffix <> "/api/deploy-jobs/ws?node=" <> nodeName)
  | Just suffix <- T.stripPrefix "http://" apiBase = Right ("ws://" <> suffix <> "/api/deploy-jobs/ws?node=" <> nodeName)
  | otherwise = Left ("could not derive websocket URL from providerApiBaseUrl=" <> apiBase)

decodeIncomingMessage :: BL.ByteString -> Maybe IncomingMessage
decodeIncomingMessage raw =
  do
    envelope <- decodeWsEnvelopeMessage raw
    case envelope.kind of
      WsDeployJob -> IncomingDeployJob envelope <$> decodeDeployJobMessage raw
      _ -> pure (IncomingEnvelope envelope)

runWebSocketSession :: Logger -> WebSocketConfig -> (SessionRuntime -> DeployJob -> IO ()) -> IO ()
runWebSocketSession logger cfg onJob = do
  endpoint <-
    case parseEndpoint cfg.url of
      Left err -> fail (T.unpack err)
      Right value -> pure value
  let app connection = do
        runSessionOverConnection logger cfg defaultSessionHooks (webSocketConnection logger cfg.onSessionActivity connection) onJob
  if endpoint.secure
    then Wuss.runSecureClientWith endpoint.host (fromIntegral endpoint.port) endpoint.pathAndQuery WS.defaultConnectionOptions [] app
    else WS.runClientWith endpoint.host endpoint.port endpoint.pathAndQuery WS.defaultConnectionOptions [] app

data SessionConnection = SessionConnection
  { receiveFrame :: IO BL.ByteString
  , sendFrame :: BL.ByteString -> IO ()
  , withHeartbeat :: IO () -> IO ()
  }

data SessionHooks = SessionHooks
  { workerStarted :: Text -> IO ()
  , workerStopped :: Text -> IO ()
  }

defaultSessionHooks :: SessionHooks
defaultSessionHooks =
  SessionHooks
    { workerStarted = \_ -> pure ()
    , workerStopped = \_ -> pure ()
    }

runSessionOverConnection :: Logger -> WebSocketConfig -> SessionHooks -> SessionConnection -> (SessionRuntime -> DeployJob -> IO ()) -> IO ()
runSessionOverConnection logger cfg hooks connection onJob = do
  sessionId <- cfg.buildSessionId
  incomingQueue <- newTQueueIO
  outgoingQueue <- newTQueueIO
  authPayload <- buildSessionAuthPayload cfg
  let sessionRuntime =
        SessionRuntime
          { sessionId
          , sendJson = enqueueOutgoingJson outgoingQueue
          , sendResume = cfg.buildResumePayload >>= enqueueOutgoingJson outgoingQueue
          , markAuthenticated = cfg.onAuthenticated sessionId
          }
  logInfo logger "transport.websocket" "session_connected"
    [ "session_id" A..= sessionId
    , "websocket_url" A..= cfg.url
    ]
  cfg.onSessionActivity "session_connected"
  atomically (writeTQueue outgoingQueue (A.encode authPayload))
  connection.withHeartbeat do
    withAsync (runNamedWorker hooks "receiver" (runReceiver logger sessionId cfg.onSessionActivity connection incomingQueue)) $ \receiverAsync ->
      withAsync (runNamedWorker hooks "sender" (runSender cfg.onSessionActivity connection outgoingQueue)) $ \senderAsync ->
        withAsync (runNamedWorker hooks "dispatcher" (runIncomingDispatcher logger incomingQueue sessionRuntime onJob)) $ \dispatcherAsync -> do
          outcome <- waitAnyCatchCancel [receiverAsync, senderAsync, dispatcherAsync]
          case snd outcome of
            Left err -> throwIO err
            Right () -> pure ()

enqueueOutgoingJson :: TQueue BL.ByteString -> A.Value -> IO ()
enqueueOutgoingJson outgoingQueue value =
  atomically (writeTQueue outgoingQueue (A.encode value))

runReceiver :: Logger -> Text -> (Text -> IO ()) -> SessionConnection -> TQueue IncomingMessage -> IO ()
runReceiver logger sessionId noteActivity connection incomingQueue =
  forever do
    raw <- connection.receiveFrame
    noteActivity "websocket_frame_received"
    logDebug logger "transport.websocket" "websocket_frame_received"
      [ "session_id" A..= sessionId
      , "raw_size" A..= BL.length raw
      ]
    case decodeIncomingMessage raw of
      Just incoming -> atomically (writeTQueue incomingQueue incoming)
      Nothing ->
        logWarn logger "transport.websocket" "malformed_websocket_payload"
          (["session_id" A..= sessionId, "raw_size" A..= BL.length raw, "raw_preview" A..= rawPreview raw] <> envelopeMetaFields raw)

runSender :: (Text -> IO ()) -> SessionConnection -> TQueue BL.ByteString -> IO ()
runSender noteActivity connection outgoingQueue =
  forever do
    payload <- atomically (readTQueue outgoingQueue)
    connection.sendFrame payload
    noteActivity "websocket_frame_sent"

runNamedWorker :: SessionHooks -> Text -> IO () -> IO ()
runNamedWorker hooks name =
  bracket_ (hooks.workerStarted name) (hooks.workerStopped name)

webSocketConnection :: Logger -> (Text -> IO ()) -> WS.Connection -> SessionConnection
webSocketConnection logger noteActivity connection =
  SessionConnection
    { receiveFrame = WS.receiveData connection
    , sendFrame = WS.sendTextData connection
    , withHeartbeat = \action -> PingPong.withPingPong heartbeatOptions connection (const action)
    }
  where
    heartbeatOptions =
        PingPong.defaultPingPongOptions
          { PingPong.pingInterval = 15
          , PingPong.pongTimeout = 30
          , PingPong.pingAction = do
              noteActivity "websocket_ping_sent"
              logDebug logger "transport.websocket" "websocket_ping_sent" []
          }

buildSessionAuthPayload :: WebSocketConfig -> IO A.Value
buildSessionAuthPayload cfg = do
  now <- getCurrentTime
  let timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
      messageId = "auth-" <> cfg.nodeName <> "-" <> sanitizeMessageId timestamp
  pure (buildWsAuthPayload cfg.nodeName cfg.authToken messageId timestamp)

buildSessionResumePayload :: Text -> IO AgentState -> IO A.Value
buildSessionResumePayload nodeName readState = do
  now <- getCurrentTime
  state <- readState
  let timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
      messageId = "resume-" <> nodeName <> "-" <> sanitizeMessageId timestamp
  pure (buildWsResumePayload nodeName (resumePayload state) messageId timestamp)

buildWebSocketSessionId :: Text -> IO Text
buildWebSocketSessionId nodeName = do
  now <- getCurrentTime
  let timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
  pure ("session-" <> nodeName <> "-" <> sanitizeMessageId timestamp)

sanitizeMessageId :: Text -> Text
sanitizeMessageId = T.map normalize
  where
    normalize ':' = '-'
    normalize c = c

rawPreview :: BL.ByteString -> Text
rawPreview raw =
  T.pack (take 256 (BLC.unpack raw))

envelopeMetaFields :: BL.ByteString -> [Pair]
envelopeMetaFields raw =
  case A.decode raw of
    Just (A.Object obj) ->
      foldr (\f acc -> maybe acc (: acc) f)
        []
        [ fmap ("kind" A..=) (lookupTextField "kind" obj)
        , fmap ("message_id" A..=) (lookupTextField "messageId" obj)
        , fmap ("job_id" A..=) (lookupTextField "jobId" obj)
        , fmap ("dispatch_id" A..=) (lookupTextField "dispatchId" obj)
        ]
    _ -> []

lookupTextField :: Text -> KM.KeyMap A.Value -> Maybe Text
lookupTextField key obj =
  case KM.lookup (K.fromText key) obj of
    Just (A.String value) -> Just (T.strip value)
    _ -> Nothing

data Endpoint = Endpoint
  { secure :: Bool
  , host :: String
  , port :: Int
  , pathAndQuery :: String
  }

parseEndpoint :: Text -> Either Text Endpoint
parseEndpoint rawUrl =
  case parseURI (T.unpack rawUrl) of
    Nothing -> Left ("invalid websocket URL: " <> rawUrl)
    Just uri ->
      case uri.uriAuthority of
        Nothing -> Left ("websocket URL missing authority: " <> rawUrl)
        Just authority ->
          case uri.uriScheme of
            "ws:" -> Right (mkEndpoint False 80 uri authority)
            "wss:" -> Right (mkEndpoint True 443 uri authority)
            _ -> Left ("unsupported websocket URL scheme: " <> rawUrl)

mkEndpoint :: Bool -> Int -> URI -> URIAuth -> Endpoint
mkEndpoint secure defaultPort uri authority =
  Endpoint
    { secure
    , host = authority.uriRegName
    , port = parsePort authority.uriPort defaultPort
    , pathAndQuery = if null uri.uriPath then "/" <> uri.uriQuery else uri.uriPath <> uri.uriQuery
    }

parsePort :: String -> Int -> Int
parsePort raw defaultPort =
  case raw of
    ':' : digits ->
      case reads digits of
        [(portNumber, "")] -> portNumber
        _ -> defaultPort
    _ -> defaultPort
