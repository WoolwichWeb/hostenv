module Hostenv.Provider.DeployAgent.Logging
  ( LogEntry(..)
  , LogLevel(..)
  , Logger
  , logDebug
  , logError
  , logInfo
  , logWarn
  , mkJsonLogger
  , renderLogEntry
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hostenv.Provider.DeployAgent.State (formatTimestamp)

data LogLevel = LogDebug | LogInfo | LogWarn | LogError
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

data LogEntry = LogEntry
  { timestamp :: Text
  , severity :: Text
  , component :: Text
  , event :: Text
  , sessionId :: Maybe Text
  , messageId :: Maybe Text
  , dispatchId :: Maybe Text
  , actionId :: Maybe Text
  , durationMs :: Maybe Int
  , exitCode :: Maybe Int
  , errorClass :: Maybe Text
  , fields :: KM.KeyMap A.Value
  }
  deriving (Eq, Show, Generic)

newtype Logger = Logger
  { writeLog :: LogLevel -> Text -> Text -> [Pair] -> IO ()
  }

instance A.ToJSON LogEntry where
  toJSON entry =
    A.Object
      ( KM.fromList
          [ "timestamp" A..= entry.timestamp
          , "severity" A..= entry.severity
          , "component" A..= entry.component
          , "event" A..= entry.event
          , "session_id" A..= entry.sessionId
          , "message_id" A..= entry.messageId
          , "dispatch_id" A..= entry.dispatchId
          , "action_id" A..= entry.actionId
          , "duration_ms" A..= entry.durationMs
          , "exit_code" A..= entry.exitCode
          , "error_class" A..= entry.errorClass
          ]
          <> entry.fields
      )

mkJsonLogger :: IO UTCTime -> (BL.ByteString -> IO ()) -> Logger
mkJsonLogger now sink =
  Logger \level component event fields -> do
    timestamp <- formatTimestamp <$> now
    let fieldMap = KM.fromList fields
    sink
      ( renderLogEntry
          LogEntry
            { timestamp
            , severity = renderLevel level
            , component
            , event
            , sessionId = lookupTextField "session_id" fieldMap
            , messageId = lookupTextField "message_id" fieldMap
            , dispatchId = lookupTextField "dispatch_id" fieldMap
            , actionId = lookupTextField "action_id" fieldMap
            , durationMs = lookupIntField "duration_ms" fieldMap
            , exitCode = lookupIntField "exit_code" fieldMap
            , errorClass = lookupTextField "error_class" fieldMap
            , fields = stripReservedFields fieldMap
            }
      )

renderLogEntry :: LogEntry -> BL.ByteString
renderLogEntry entry = A.encode entry <> BLC.pack "\n"

logDebug :: Logger -> Text -> Text -> [Pair] -> IO ()
logDebug logger component event fields = logger.writeLog LogDebug component event fields

logInfo :: Logger -> Text -> Text -> [Pair] -> IO ()
logInfo logger component event fields = logger.writeLog LogInfo component event fields

logWarn :: Logger -> Text -> Text -> [Pair] -> IO ()
logWarn logger component event fields = logger.writeLog LogWarn component event fields

logError :: Logger -> Text -> Text -> [Pair] -> IO ()
logError logger component event fields = logger.writeLog LogError component event fields

renderLevel :: LogLevel -> Text
renderLevel = \case
  LogDebug -> "debug"
  LogInfo -> "info"
  LogWarn -> "warn"
  LogError -> "error"

lookupTextField :: Text -> KM.KeyMap A.Value -> Maybe Text
lookupTextField key fields =
  case KM.lookup (K.fromText key) fields of
    Just (A.String value) -> Just value
    _ -> Nothing

lookupIntField :: Text -> KM.KeyMap A.Value -> Maybe Int
lookupIntField key fields =
  case KM.lookup (K.fromText key) fields of
    Just (A.Number value) -> Just (round value)
    _ -> Nothing

stripReservedFields :: KM.KeyMap A.Value -> KM.KeyMap A.Value
stripReservedFields fields =
  foldr (KM.delete . K.fromText) fields reservedFieldNames

reservedFieldNames :: [Text]
reservedFieldNames =
  [ "timestamp"
  , "severity"
  , "component"
  , "event"
  , "session_id"
  , "message_id"
  , "dispatch_id"
  , "action_id"
  , "duration_ms"
  , "exit_code"
  , "error_class"
  , "message"
  , "fields"
  ]
