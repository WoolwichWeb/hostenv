module Hostenv.Provider.DeployAgent.Protocol
  ( ActionOp(..)
  , DeployAction(..)
  , DeployIntent(..)
  , DeployJob(..)
  , EventStatus(..)
  , NodeEvent(..)
  , actionStorePath
  , buildWsAuthPayload
  , decodeDeployJobMessage
  , eventStatusText
  , intentSystemPath
  , isSafeAtom
  , jobSignature
  , renderActionOp
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isDigit)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Crypto.Hash (Digest, SHA256, hash)

data ActionOp
  = Activate
  | Reload
  | Backup
  | Restore
  | Deactivate
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

data DeployAction = DeployAction
  { op :: ActionOp
  , user :: Text
  , storePath :: Maybe Text
  , envStorePath :: Maybe Text
  , path :: Maybe Text
  , fromNode :: Maybe Text
  , toNode :: Maybe Text
  , migrations :: [Text]
  , rawAction :: A.Value
  }
  deriving (Eq, Show, Generic)

data DeployIntent = DeployIntent
  { schemaVersion :: Int
  , systemPath :: Maybe Text
  , systemToplevel :: Maybe Text
  , actions :: [DeployAction]
  }
  deriving (Eq, Show, Generic)

data DeployJob = DeployJob
  { jobId :: Text
  , commitSha :: Text
  , node :: Maybe Text
  , intent :: DeployIntent
  }
  deriving (Eq, Show, Generic)

data EventStatus
  = EventQueued
  | EventWaiting
  | EventRunning
  | EventSuccess
  | EventFailed
  | EventTimedOut
  deriving (Eq, Ord, Show, Enum, Bounded)

data NodeEvent = NodeEvent
  { node :: Text
  , status :: EventStatus
  , phase :: Maybe Text
  , message :: Maybe Text
  , payload :: A.Value
  }
  deriving (Eq, Show, Generic)

instance A.ToJSON ActionOp where
  toJSON = A.String . renderActionOp

instance A.FromJSON ActionOp where
  parseJSON = A.withText "ActionOp" $ \raw ->
    case parseActionOp raw of
      Nothing -> fail ("unsupported action op: " <> T.unpack raw)
      Just op -> pure op

instance A.ToJSON DeployAction where
  toJSON action = action.rawAction

instance A.ToJSON DeployIntent where
  toJSON intent =
    A.object
      ( [ "schemaVersion" A..= intent.schemaVersion
        , "actions" A..= map A.toJSON intent.actions
        ]
          <> catMaybes
            [ fmap ("systemPath" A..=) intent.systemPath
            , fmap ("systemToplevel" A..=) intent.systemToplevel
            ]
      )

instance A.ToJSON DeployJob where
  toJSON job =
    A.object
      [ "type" A..= ("deploy_job" :: Text)
      , "jobId" A..= job.jobId
      , "commitSha" A..= job.commitSha
      , "node" A..= job.node
      , "intent" A..= job.intent
      ]

instance A.ToJSON EventStatus where
  toJSON = A.String . eventStatusText

instance A.ToJSON NodeEvent where
  toJSON event =
    A.object
      [ "node" A..= event.node
      , "status" A..= event.status
      , "phase" A..= event.phase
      , "message" A..= event.message
      , "payload" A..= event.payload
      ]

buildWsAuthPayload :: Text -> Text -> A.Value
buildWsAuthPayload nodeName token =
  A.object
    [ "type" A..= ("auth" :: Text)
    , "node" A..= nodeName
    , "token" A..= token
    ]

decodeDeployJobMessage :: BL.ByteString -> Maybe DeployJob
decodeDeployJobMessage raw =
  A.decode raw >>= decodeDeployJobValue

decodeDeployJobValue :: A.Value -> Maybe DeployJob
decodeDeployJobValue = \case
  A.Object obj -> do
    A.String msgType <- KM.lookup "type" obj
    if msgType /= "deploy_job"
      then Nothing
      else do
        A.String rawJobId <- KM.lookup "jobId" obj
        intentValue <- KM.lookup "intent" obj
        parsedIntent <- decodeIntentValue intentValue
        let commitSha =
              case KM.lookup "commitSha" obj of
                Just (A.String value) -> T.strip value
                _ -> ""
            rawNode =
              case KM.lookup "node" obj of
                Just (A.String value) | T.strip value /= "" -> Just (T.strip value)
                _ -> Nothing
            parsedJob =
              DeployJob
                { jobId = T.strip rawJobId
                , commitSha
                , node = rawNode
                , intent = parsedIntent
                }
        validateDeployJob parsedJob
  _ -> Nothing

decodeIntentValue :: A.Value -> Maybe DeployIntent
decodeIntentValue = \case
  A.Object obj -> do
    schema <- case KM.lookup "schemaVersion" obj of
      Just (A.Number value) -> pure (round value)
      _ -> Nothing
    rawActions <- case KM.lookup "actions" obj of
      Just (A.Array values) -> pure (foldr (:) [] values)
      _ -> Nothing
    parsedActions <- traverse decodeActionValue rawActions
    pure
      DeployIntent
        { schemaVersion = schema
        , systemPath = extractTextField "systemPath" obj
        , systemToplevel = extractTextField "systemToplevel" obj
        , actions = parsedActions
        }
  _ -> Nothing

decodeActionValue :: A.Value -> Maybe DeployAction
decodeActionValue value =
  case value of
    A.Object obj -> do
      A.String rawUser <- KM.lookup "user" obj
      A.String rawOp <- KM.lookup "op" obj
      parsedOp <- parseActionOp rawOp
      pure
        DeployAction
          { op = parsedOp
          , user = T.strip rawUser
          , storePath = extractTextField "storePath" obj
          , envStorePath = extractTextField "envStorePath" obj
          , path = extractTextField "path" obj
          , fromNode = extractTextField "fromNode" obj
          , toNode = extractTextField "toNode" obj
          , migrations = extractTextListField "migrations" obj
          , rawAction = value
          }
    _ -> Nothing

validateDeployJob :: DeployJob -> Maybe DeployJob
validateDeployJob job
  | job.jobId == "" = Nothing
  | job.intent.schemaVersion /= 1 = Nothing
  | not (all validateAction job.intent.actions) = Nothing
  | otherwise = Just job

validateAction :: DeployAction -> Bool
validateAction action =
  isSafeAtom action.user
    && maybe True isSafeAtom action.fromNode
    && maybe True isSafeAtom action.toNode
    && all isSafeAtom action.migrations

actionStorePath :: DeployAction -> Text
actionStorePath action =
  firstNonEmpty [action.storePath, action.envStorePath, action.path]

intentSystemPath :: DeployIntent -> Text
intentSystemPath intent =
  firstNonEmpty [intent.systemPath, intent.systemToplevel]

jobSignature :: DeployJob -> Text
jobSignature job =
  let payload =
        A.object
          [ "jobId" A..= job.jobId
          , "commitSha" A..= job.commitSha
          , "systemPath" A..= intentSystemPath job.intent
          , "actions" A..= map A.toJSON job.intent.actions
          ]
      digest = hash (BL.toStrict (A.encode payload)) :: Digest SHA256
   in TE.decodeUtf8 (BAE.convertToBase BAE.Base16 digest)

eventStatusText :: EventStatus -> Text
eventStatusText = \case
  EventQueued -> "queued"
  EventWaiting -> "waiting"
  EventRunning -> "running"
  EventSuccess -> "success"
  EventFailed -> "failed"
  EventTimedOut -> "timed_out"

parseActionOp :: Text -> Maybe ActionOp
parseActionOp raw =
  case T.toLower (T.strip raw) of
    "activate" -> Just Activate
    "reload" -> Just Reload
    "backup" -> Just Backup
    "restore" -> Just Restore
    "deactivate" -> Just Deactivate
    _ -> Nothing

renderActionOp :: ActionOp -> Text
renderActionOp = \case
  Activate -> "activate"
  Reload -> "reload"
  Backup -> "backup"
  Restore -> "restore"
  Deactivate -> "deactivate"

extractTextField :: Text -> KM.KeyMap A.Value -> Maybe Text
extractTextField key obj =
  case KM.lookup (K.fromText key) obj of
    Just (A.String value) | T.strip value /= "" -> Just (T.strip value)
    _ -> Nothing

extractTextListField :: Text -> KM.KeyMap A.Value -> [Text]
extractTextListField key obj =
  case KM.lookup (K.fromText key) obj of
    Just (A.Array values) ->
      [ trimmed
      | A.String value <- foldr (:) [] values
      , let trimmed = T.strip value
      , trimmed /= ""
      ]
    _ -> []

firstNonEmpty :: [Maybe Text] -> Text
firstNonEmpty = go
  where
    go [] = ""
    go (Nothing : rest) = go rest
    go (Just value : rest)
      | T.strip value == "" = go rest
      | otherwise = T.strip value

isSafeAtom :: Text -> Bool
isSafeAtom txt =
  let trimmed = T.strip txt
   in trimmed /= "" && T.all allowed trimmed
  where
    allowed c = isAsciiLower c || isDigit c || c == '-' || c == '_' || c == '.' || c == ':'
