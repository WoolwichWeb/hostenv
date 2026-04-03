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
  , level :: Text
  , component :: Text
  , message :: Text
  , fields :: A.Value
  }
  deriving (Eq, Show, Generic)

newtype Logger = Logger
  { writeLog :: LogLevel -> Text -> Text -> A.Value -> IO ()
  }

instance A.ToJSON LogEntry where
  toJSON entry =
    A.object
      [ "timestamp" A..= entry.timestamp
      , "level" A..= entry.level
      , "component" A..= entry.component
      , "message" A..= entry.message
      , "fields" A..= entry.fields
      ]

mkJsonLogger :: IO UTCTime -> (BL.ByteString -> IO ()) -> Logger
mkJsonLogger now sink =
  Logger \level component message fields -> do
    timestamp <- formatTimestamp <$> now
    sink (renderLogEntry LogEntry { timestamp, level = renderLevel level, component, message, fields })

renderLogEntry :: LogEntry -> BL.ByteString
renderLogEntry entry = A.encode entry <> BLC.pack "\n"

logDebug :: Logger -> Text -> Text -> [Pair] -> IO ()
logDebug logger component message fields = logger.writeLog LogDebug component message (A.object fields)

logInfo :: Logger -> Text -> Text -> [Pair] -> IO ()
logInfo logger component message fields = logger.writeLog LogInfo component message (A.object fields)

logWarn :: Logger -> Text -> Text -> [Pair] -> IO ()
logWarn logger component message fields = logger.writeLog LogWarn component message (A.object fields)

logError :: Logger -> Text -> Text -> [Pair] -> IO ()
logError logger component message fields = logger.writeLog LogError component message (A.object fields)

renderLevel :: LogLevel -> Text
renderLevel = \case
  LogDebug -> "debug"
  LogInfo -> "info"
  LogWarn -> "warn"
  LogError -> "error"
