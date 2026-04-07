module Hostenv.Provider.DeployAgent.ProviderApi
  ( ProviderApi(..)
  , SnapshotDocument(..)
  , SnapshotFetchFailure(..)
  , SnapshotMetadata(..)
  , SnapshotRequest(..)
  , buildProviderApi
  , buildSnapshotMetadata
  , decodeSnapshotResponse
  , snapshotUrl
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import Control.Exception (try)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Hostenv.Provider.DeployAgent.Config (AgentConfig(..))
import Hostenv.Provider.DeployAgent.Logging (Logger, logDebug, logWarn)
import Network.HTTP.Client
  ( HttpException
  , Manager
  , Request(method, requestHeaders, responseTimeout)
  , Response(responseBody, responseStatus)
  , httpLbs
  , parseRequest
  , responseTimeoutMicro
  )
import Network.HTTP.Types.Header (HeaderName, hAuthorization)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)

data SnapshotRequest = SnapshotRequest
  { jobId :: T.Text
  , sourceNode :: T.Text
  , user :: T.Text
  }
  deriving (Eq, Show)

data SnapshotMetadata = SnapshotMetadata
  { snapshotSize :: Int64
  , snapshotChecksum :: T.Text
  , snapshotContentType :: T.Text
  , snapshotSchemaVersion :: Int
  }
  deriving (Eq, Show)

instance A.FromJSON SnapshotMetadata where
  parseJSON = A.withObject "SnapshotMetadata" $ \obj ->
    SnapshotMetadata
      <$> obj A..: "size"
      <*> obj A..: "checksum"
      <*> obj A..: "contentType"
      <*> obj A..: "schemaVersion"

instance A.ToJSON SnapshotMetadata where
  toJSON metadata =
    A.object
      [ "size" A..= metadata.snapshotSize
      , "checksum" A..= metadata.snapshotChecksum
      , "contentType" A..= metadata.snapshotContentType
      , "schemaVersion" A..= metadata.snapshotSchemaVersion
      ]

data SnapshotDocument = SnapshotDocument
  { snapshotMetadata :: SnapshotMetadata
  , snapshotPayload :: A.Value
  , snapshotBytes :: BL.ByteString
  }
  deriving (Eq, Show)

data SnapshotFetchFailure
  = SnapshotMissing
  | SnapshotMalformed T.Text
  | SnapshotIntegrityFailure SnapshotMetadata SnapshotMetadata
  deriving (Eq, Show)

data ProviderApi = ProviderApi
  { fetchSnapshot :: SnapshotRequest -> IO (Either SnapshotFetchFailure SnapshotDocument)
  }

buildProviderApi :: Logger -> Manager -> AgentConfig -> T.Text -> ProviderApi
buildProviderApi logger manager cfg token =
  ProviderApi
    { fetchSnapshot = fetchSnapshotIO logger manager cfg token
    }

fetchSnapshotIO :: Logger -> Manager -> AgentConfig -> T.Text -> SnapshotRequest -> IO (Either SnapshotFetchFailure SnapshotDocument)
fetchSnapshotIO logger manager cfg token snapshotRequest = do
  let url = snapshotUrl cfg.providerApiBaseUrl cfg.nodeName snapshotRequest
  logDebug logger "provider-api" "snapshot_requested"
    [ "job_id" A..= snapshotRequest.jobId
    , "source_node" A..= snapshotRequest.sourceNode
    , "user" A..= snapshotRequest.user
    ]
  if not (usesHttpScheme url)
    then do
      logWarn logger "provider-api" "snapshot_request_rejected"
        [ "job_id" A..= snapshotRequest.jobId
        , "source_node" A..= snapshotRequest.sourceNode
        , "user" A..= snapshotRequest.user
        , "snapshot_url" A..= url
        , "error_class" A..= ("invalid_snapshot_url" :: T.Text)
        ]
      pure (Left (SnapshotMalformed "snapshot url must use http or https"))
    else do
      request <- snapshotRequestIO token url
      result <- tryHttp (httpLbs request manager)
      case result of
        Left _httpErr -> do
          logWarn logger "provider-api" "snapshot_request_failed"
            [ "job_id" A..= snapshotRequest.jobId
            , "source_node" A..= snapshotRequest.sourceNode
            , "user" A..= snapshotRequest.user
            , "error_class" A..= ("http_exception" :: T.Text)
            ]
          pure (Left SnapshotMissing)
        Right response
          | statusCode response.responseStatus /= 200 -> do
              logDebug logger "provider-api" "snapshot_unavailable"
                [ "job_id" A..= snapshotRequest.jobId
                , "http_status" A..= statusCode response.responseStatus
                , "source_node" A..= snapshotRequest.sourceNode
                , "user" A..= snapshotRequest.user
                ]
              pure (Left SnapshotMissing)
          | otherwise ->
              case decodeSnapshotResponse (statusCode response.responseStatus) response.responseBody of
                Left failure -> do
                  logSnapshotFetchFailure logger snapshotRequest failure
                  pure (Left failure)
                Right snapshot -> do
                  logDebug logger "provider-api" "snapshot_available"
                    [ "job_id" A..= snapshotRequest.jobId
                    , "source_node" A..= snapshotRequest.sourceNode
                    , "user" A..= snapshotRequest.user
                    , "size" A..= snapshot.snapshotMetadata.snapshotSize
                    , "checksum" A..= snapshot.snapshotMetadata.snapshotChecksum
                    ]
                  pure (Right snapshot)

snapshotRequestIO :: T.Text -> T.Text -> IO Request
snapshotRequestIO token snapshotRequestUrl = do
  request0 <- parseRequest (T.unpack snapshotRequestUrl)
  pure
    request0
      { method = "GET"
      , requestHeaders = authHeaders token
      , responseTimeout = responseTimeoutMicro (20 * 1000000)
      }

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

authHeaders :: T.Text -> [(HeaderName, BS.ByteString)]
authHeaders token = [(hAuthorization, "Bearer " <> TE.encodeUtf8 token)]

trimTrailingSlash :: T.Text -> T.Text
trimTrailingSlash = T.dropWhileEnd (== '/')

encodeQueryValue :: T.Text -> T.Text
encodeQueryValue = TE.decodeUtf8 . urlEncode True . TE.encodeUtf8

decodeSnapshotResponse :: Int -> BL.ByteString -> Either SnapshotFetchFailure SnapshotDocument
decodeSnapshotResponse httpStatus rawBody
  | httpStatus /= 200 = Left SnapshotMissing
  | otherwise =
      case A.decode rawBody of
        Just (A.Object obj) -> do
          metadataValue <- maybe (Left (SnapshotMalformed "snapshot metadata missing")) Right (KM.lookup "metadata" obj)
          payloadValue <- maybe (Left (SnapshotMalformed "snapshot payload missing")) Right (KM.lookup "payload" obj)
          expectedMetadata <-
            case A.fromJSON metadataValue of
              A.Error err -> Left (SnapshotMalformed (T.pack err))
              A.Success metadata -> Right metadata
          verifySnapshotDocument expectedMetadata payloadValue
        Just _ -> Left (SnapshotMalformed "snapshot response must be a json object")
        Nothing -> Left (SnapshotMalformed "snapshot response was not valid json")

buildSnapshotMetadata :: A.Value -> SnapshotMetadata
buildSnapshotMetadata payload =
  let payloadBytes = A.encode payload
   in SnapshotMetadata
        { snapshotSize = BL.length payloadBytes
        , snapshotChecksum = sha256Hex payloadBytes
        , snapshotContentType = "application/json"
        , snapshotSchemaVersion = 1
        }

verifySnapshotDocument :: SnapshotMetadata -> A.Value -> Either SnapshotFetchFailure SnapshotDocument
verifySnapshotDocument expectedMetadata payloadValue
  | expectedMetadata.snapshotContentType /= "application/json" = Left (SnapshotMalformed "snapshot contentType must be application/json")
  | expectedMetadata.snapshotSchemaVersion /= 1 = Left (SnapshotMalformed "snapshot schemaVersion must be 1")
  | expectedMetadata.snapshotSize /= actualMetadata.snapshotSize = Left (SnapshotIntegrityFailure expectedMetadata actualMetadata)
  | expectedMetadata.snapshotChecksum /= actualMetadata.snapshotChecksum = Left (SnapshotIntegrityFailure expectedMetadata actualMetadata)
  | otherwise =
      Right
        SnapshotDocument
          { snapshotMetadata = expectedMetadata
          , snapshotPayload = payloadValue
          , snapshotBytes = payloadBytes
          }
  where
    payloadBytes = A.encode payloadValue
    actualMetadata = buildSnapshotMetadata payloadValue

logSnapshotFetchFailure :: Logger -> SnapshotRequest -> SnapshotFetchFailure -> IO ()
logSnapshotFetchFailure logger snapshotRequest = \case
  SnapshotMissing ->
    logDebug logger "provider-api" "snapshot_unavailable"
      [ "job_id" A..= snapshotRequest.jobId
      , "source_node" A..= snapshotRequest.sourceNode
      , "user" A..= snapshotRequest.user
      ]
  SnapshotMalformed reason ->
    logWarn logger "provider-api" "snapshot_payload_malformed"
      [ "job_id" A..= snapshotRequest.jobId
      , "source_node" A..= snapshotRequest.sourceNode
      , "user" A..= snapshotRequest.user
      , "error_class" A..= ("snapshot_malformed" :: T.Text)
      , "reason" A..= reason
      ]
  SnapshotIntegrityFailure expectedMetadata actualMetadata ->
    logWarn logger "provider-api" "snapshot_integrity_failed"
      [ "job_id" A..= snapshotRequest.jobId
      , "source_node" A..= snapshotRequest.sourceNode
      , "user" A..= snapshotRequest.user
      , "error_class" A..= ("integrity_check_failed" :: T.Text)
      , "expected_metadata" A..= expectedMetadata
      , "actual_metadata" A..= actualMetadata
      ]

usesHttpScheme :: T.Text -> Bool
usesHttpScheme url =
  let lower = T.toLower url
   in "http://" `T.isPrefixOf` lower || "https://" `T.isPrefixOf` lower

sha256Hex :: BL.ByteString -> T.Text
sha256Hex payloadBytes =
  let digest = hash (BL.toStrict payloadBytes) :: Digest SHA256
   in TE.decodeUtf8 (BAE.convertToBase BAE.Base16 digest)

tryHttp :: IO a -> IO (Either HttpException a)
tryHttp = try
