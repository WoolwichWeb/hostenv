module Hostenv.Provider.DeployAgent.ProviderApi
  ( ProviderApi(..)
  , buildProviderApi
  , eventUrl
  , snapshotUrl
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hostenv.Provider.DeployAgent.Config (AgentConfig(..))
import Hostenv.Provider.DeployAgent.Executor (SnapshotRequest(..))
import Hostenv.Provider.DeployAgent.Logging (Logger, logDebug, logWarn)
import Hostenv.Provider.DeployAgent.Protocol (NodeEvent(..))
import Network.HTTP.Client
  ( HttpException
  , Manager
  , Request(method, requestBody, requestHeaders, responseTimeout)
  , RequestBody(RequestBodyLBS)
  , Response(responseBody, responseStatus)
  , httpLbs
  , parseRequest
  , responseTimeoutMicro
  )
import Network.HTTP.Types.Header (HeaderName, hAuthorization, hContentType)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)

data ProviderApi = ProviderApi
  { postEvent :: T.Text -> NodeEvent -> IO ()
  , fetchSnapshot :: SnapshotRequest -> IO (Maybe A.Value)
  }

buildProviderApi :: Logger -> Manager -> AgentConfig -> T.Text -> ProviderApi
buildProviderApi logger manager cfg token =
  ProviderApi
    { postEvent = postEventIO logger manager cfg token
    , fetchSnapshot = fetchSnapshotIO logger manager cfg token
    }

postEventIO :: Logger -> Manager -> AgentConfig -> T.Text -> T.Text -> NodeEvent -> IO ()
postEventIO logger manager cfg token jobId event = attempt 1
  where
    maxAttempts = 6 :: Int
    requestBodyValue = A.encode event
    attempt currentAttempt = do
      request <- eventRequest cfg token jobId requestBodyValue
      result <- tryHttp (httpLbs request manager)
      case result of
        Right response
          | isSuccess response ->
              logDebug logger "provider-api" "posted deploy event" ["jobId" A..= jobId, "status" A..= event.status, "phase" A..= event.phase]
        Right response
          | currentAttempt >= maxAttempts ->
              logWarn logger "provider-api" "event post failed after retries" ["jobId" A..= jobId, "attempts" A..= currentAttempt, "httpStatus" A..= statusCode response.responseStatus]
          | otherwise -> retry currentAttempt
        Left _httpErr
          | currentAttempt >= maxAttempts ->
              logWarn logger "provider-api" "event post raised http exception" ["jobId" A..= jobId, "attempts" A..= currentAttempt]
          | otherwise -> retry currentAttempt
    retry currentAttempt = do
      logWarn logger "provider-api" "retrying deploy event post" ["jobId" A..= jobId, "attempt" A..= currentAttempt, "maxAttempts" A..= maxAttempts]
      sleepReconnect cfg.reconnectSeconds
      attempt (currentAttempt + 1)

fetchSnapshotIO :: Logger -> Manager -> AgentConfig -> T.Text -> SnapshotRequest -> IO (Maybe A.Value)
fetchSnapshotIO logger manager cfg token snapshotRequest = do
  request <- snapshotRequestIO cfg token snapshotRequest
  result <- tryHttp (httpLbs request manager)
  case result of
    Left _httpErr -> do
      logWarn logger "provider-api" "snapshot request failed" ["jobId" A..= snapshotRequest.jobId, "sourceNode" A..= snapshotRequest.sourceNode, "user" A..= snapshotRequest.user]
      pure Nothing
    Right response
      | statusCode response.responseStatus /= 200 -> do
          logDebug logger "provider-api" "snapshot unavailable" ["jobId" A..= snapshotRequest.jobId, "httpStatus" A..= statusCode response.responseStatus, "sourceNode" A..= snapshotRequest.sourceNode, "user" A..= snapshotRequest.user]
          pure Nothing
      | otherwise ->
          case A.decode response.responseBody of
            Just (A.Object obj) ->
              case lookupPayload obj of
                Just payload -> pure (Just payload)
                Nothing -> do
                  logWarn logger "provider-api" "snapshot payload missing" ["jobId" A..= snapshotRequest.jobId, "sourceNode" A..= snapshotRequest.sourceNode, "user" A..= snapshotRequest.user]
                  pure Nothing
            _ -> do
              logWarn logger "provider-api" "snapshot response was not valid json" ["jobId" A..= snapshotRequest.jobId, "sourceNode" A..= snapshotRequest.sourceNode, "user" A..= snapshotRequest.user]
              pure Nothing

eventRequest :: AgentConfig -> T.Text -> T.Text -> BL.ByteString -> IO Request
eventRequest cfg token jobId body = do
  request0 <- parseRequest (T.unpack (eventUrl cfg.providerApiBaseUrl jobId))
  pure
    request0
      { method = "POST"
      , requestHeaders = jsonHeaders token
      , requestBody = RequestBodyLBS body
      , responseTimeout = responseTimeoutMicro (20 * 1000000)
      }

snapshotRequestIO :: AgentConfig -> T.Text -> SnapshotRequest -> IO Request
snapshotRequestIO cfg token snapshotRequest = do
  request0 <- parseRequest (T.unpack (snapshotUrl cfg.providerApiBaseUrl cfg.nodeName snapshotRequest))
  pure
    request0
      { method = "GET"
      , requestHeaders = authHeaders token
      , responseTimeout = responseTimeoutMicro (20 * 1000000)
      }

eventUrl :: T.Text -> T.Text -> T.Text
eventUrl apiBase jobId = trimTrailingSlash apiBase <> "/api/deploy-jobs/" <> jobId <> "/events"

snapshotUrl :: T.Text -> T.Text -> SnapshotRequest -> T.Text
snapshotUrl apiBase nodeName snapshotRequest =
  trimTrailingSlash apiBase
    <> "/api/deploy-jobs/"
    <> snapshotRequest.jobId
    <> "/backup-snapshot?node="
    <> encodeQueryValue nodeName
    <> "&source="
    <> encodeQueryValue snapshotRequest.sourceNode
    <> "&user="
    <> encodeQueryValue snapshotRequest.user

jsonHeaders :: T.Text -> [(HeaderName, BS.ByteString)]
jsonHeaders token = (hContentType, "application/json") : authHeaders token

authHeaders :: T.Text -> [(HeaderName, BS.ByteString)]
authHeaders token = [(hAuthorization, "Bearer " <> TE.encodeUtf8 token)]

trimTrailingSlash :: T.Text -> T.Text
trimTrailingSlash = T.dropWhileEnd (== '/')

encodeQueryValue :: T.Text -> T.Text
encodeQueryValue = TE.decodeUtf8 . urlEncode True . TE.encodeUtf8

lookupPayload :: A.Object -> Maybe A.Value
lookupPayload = KM.lookup "payload"

isSuccess :: Response body -> Bool
isSuccess response =
  let code = statusCode response.responseStatus
   in code >= 200 && code < 300

sleepReconnect :: Int -> IO ()
sleepReconnect seconds = threadDelay (seconds * 1000000)

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp = try
