{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Logging
  ( ProviderSeverity(..)
  , ProviderLogFields(..)
  , providerLogFields
  , providerLogValue
  , logProviderEvent
  ) where

import qualified Data.Aeson as A
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import System.IO (hFlush, stdout)

data ProviderSeverity
  = ProviderSeverityDebug
  | ProviderSeverityInfo
  | ProviderSeverityWarn
  | ProviderSeverityError
  deriving (Eq, Show)

data ProviderLogFields = ProviderLogFields
  { entryEvent :: Text
  , entrySeverity :: ProviderSeverity
  , entryRequestId :: Maybe Text
  , entryJobId :: Maybe Text
  , entryNode :: Maybe Text
  , entryDispatchId :: Maybe Text
  , entryActionId :: Maybe Text
  , entryPhase :: Maybe Text
  , entryStatus :: Maybe Text
  , entryProtocolVersion :: Maybe Int
  , entryDecision :: Maybe Text
  , entryReason :: Maybe Text
  }
  deriving (Eq, Show)

providerLogFields :: Text -> ProviderSeverity -> ProviderLogFields
providerLogFields eventName severity =
  ProviderLogFields
    { entryEvent = eventName
    , entrySeverity = severity
    , entryRequestId = Nothing
    , entryJobId = Nothing
    , entryNode = Nothing
    , entryDispatchId = Nothing
    , entryActionId = Nothing
    , entryPhase = Nothing
    , entryStatus = Nothing
    , entryProtocolVersion = Nothing
    , entryDecision = Nothing
    , entryReason = Nothing
    }

providerLogValue :: UTCTime -> ProviderLogFields -> [Pair] -> A.Value
providerLogValue timestamp fields extras =
  A.object
    ( [ "timestamp" A..= timestamp
      , "event" A..= entryEvent fields
      , "severity" A..= renderProviderSeverity (entrySeverity fields)
      , "request_id" A..= entryRequestId fields
      , "job_id" A..= entryJobId fields
      , "node" A..= entryNode fields
      , "dispatch_id" A..= entryDispatchId fields
      , "action_id" A..= entryActionId fields
      , "phase" A..= entryPhase fields
      , "status" A..= entryStatus fields
      , "protocol_version" A..= entryProtocolVersion fields
      , "decision" A..= entryDecision fields
      , "reason" A..= entryReason fields
      ]
        <> extras
    )

logProviderEvent :: ProviderLogFields -> [Pair] -> IO ()
logProviderEvent fields extras = do
  timestamp <- getCurrentTime
  BLC.putStrLn (A.encode (providerLogValue timestamp fields extras))
  hFlush stdout

renderProviderSeverity :: ProviderSeverity -> Text
renderProviderSeverity severity =
  case severity of
    ProviderSeverityDebug -> "debug"
    ProviderSeverityInfo -> "info"
    ProviderSeverityWarn -> "warn"
    ProviderSeverityError -> "error"
