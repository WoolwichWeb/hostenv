module Hostenv.Provider.DeployAgent.Transport.WebSocket
  ( IncomingMessage(..)
  , WebSocketConfig(..)
  , buildWebSocketConfig
  , decodeIncomingMessage
  , deriveWebSocketUrl
  , runWebSocketSession
  ) where

import Control.Monad (forever)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Hostenv.Provider.DeployAgent.Config (AgentConfig(..))
import Hostenv.Provider.DeployAgent.Logging (Logger, logDebug, logInfo, logWarn)
import Hostenv.Provider.DeployAgent.Protocol (DeployJob(..), buildWsAuthPayload, decodeDeployJobMessage)
import Network.URI (URI(..), URIAuth(..), parseURI)
import qualified Network.WebSockets as WS
import qualified Wuss

data WebSocketConfig = WebSocketConfig
  { url :: Text
  , authPayload :: A.Value
  , reconnectSeconds :: Int
  }
  deriving (Eq, Show)

data IncomingMessage
  = IncomingDeployJob DeployJob
  | IncomingDeployHint (Maybe Text)
  | IncomingUnknown Text
  deriving (Eq, Show)

buildWebSocketConfig :: AgentConfig -> Text -> Either Text WebSocketConfig
buildWebSocketConfig cfg token = do
  url <- deriveWebSocketUrl cfg.providerApiBaseUrl cfg.nodeName
  pure
    WebSocketConfig
      { url
      , authPayload = buildWsAuthPayload cfg.nodeName token
      , reconnectSeconds = cfg.reconnectSeconds
      }

deriveWebSocketUrl :: Text -> Text -> Either Text Text
deriveWebSocketUrl apiBase nodeName
  | Just suffix <- T.stripPrefix "https://" apiBase = Right ("wss://" <> suffix <> "/api/deploy-jobs/ws?node=" <> nodeName)
  | Just suffix <- T.stripPrefix "http://" apiBase = Right ("ws://" <> suffix <> "/api/deploy-jobs/ws?node=" <> nodeName)
  | otherwise = Left ("could not derive websocket URL from providerApiBaseUrl=" <> apiBase)

decodeIncomingMessage :: BL.ByteString -> Maybe IncomingMessage
decodeIncomingMessage raw =
  case A.decode raw of
    Just (A.Object obj) ->
      case KM.lookup "type" obj of
        Just (A.String "deploy_job") -> IncomingDeployJob <$> decodeDeployJobMessage raw
        Just (A.String "deploy_hint") -> Just (IncomingDeployHint (lookupTextField "node" obj))
        Just (A.String messageType) -> Just (IncomingUnknown messageType)
        _ -> Nothing
    _ -> Nothing

runWebSocketSession :: Logger -> WebSocketConfig -> (DeployJob -> IO ()) -> IO ()
runWebSocketSession logger cfg onJob = do
  endpoint <-
    case parseEndpoint cfg.url of
      Left err -> fail (T.unpack err)
      Right value -> pure value
  let app connection = do
        WS.sendTextData connection (A.encode cfg.authPayload)
        logInfo logger "transport.websocket" "websocket session connected" ["url" A..= cfg.url]
        forever do
          raw <- WS.receiveData connection
          case decodeIncomingMessage raw of
            Just (IncomingDeployJob job) -> do
              let DeployJob { jobId = jobIdValue, commitSha = commitShaValue } = job
              logInfo logger "transport.websocket" "received deploy job" ["jobId" A..= jobIdValue, "commitSha" A..= commitShaValue]
              onJob job
            Just (IncomingDeployHint nodeName) ->
              logDebug logger "transport.websocket" "received deploy hint" ["node" A..= nodeName]
            Just (IncomingUnknown messageType) ->
              logDebug logger "transport.websocket" "ignored websocket message" ["type" A..= messageType]
            Nothing ->
              logWarn logger "transport.websocket" "received malformed websocket payload" []
  if endpoint.secure
    then Wuss.runSecureClient endpoint.host (fromIntegral endpoint.port) endpoint.pathAndQuery app
    else WS.runClient endpoint.host endpoint.port endpoint.pathAndQuery app

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

lookupTextField :: Text -> KM.KeyMap A.Value -> Maybe Text
lookupTextField key obj =
  case KM.lookup (K.fromText key) obj of
    Just (A.String value) | T.strip value /= "" -> Just (T.strip value)
    _ -> Nothing
