module Hostenv.Provider.DeployAgent.Protocol
  ( ActionOp(..)
    , DeployAction(..)
    , DeployIntent(..)
    , DeployJob(..)
    , EventStatus(..)
    , NodeEvent(..)
    , WsEnvelope(..)
    , WsMessageKind(..)
    , actionStorePath
    , buildWsResumePayload
    , buildWsAuthPayload
    , buildWsNodeEventEnvelope
    , decodeDeployJobMessage
    , decodeWsEnvelopeMessage
   , eventStatusText
   , intentSystemPath
   , isSafeAtom
   , jobSignature
   , renderActionOp
   , renderWsMessageKind
   ) where

import Control.Monad (guard)
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
  , actionId :: Text
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
  , dispatchId :: Text
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
  { actionId :: Maybe Text
  , node :: Text
  , status :: EventStatus
  , phase :: Maybe Text
  , message :: Maybe Text
  , payload :: A.Value
  }
  deriving (Eq, Show, Generic)

data WsMessageKind
  = WsHello
  | WsAuth
  | WsAuthOk
  | WsAuthError
  | WsDeployJob
  | WsAck
  | WsProgress
  | WsActionResult
  | WsHeartbeat
  | WsResume
  | WsError
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

data WsEnvelope = WsEnvelope
  { version :: Int
  , kind :: WsMessageKind
  , messageId :: Text
  , timestamp :: Text
  , node :: Text
  , jobId :: Maybe Text
  , dispatchId :: Maybe Text
  , actionId :: Maybe Text
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
      [ "commitSha" A..= job.commitSha
      , "intent" A..= job.intent
      ]

instance A.ToJSON EventStatus where
  toJSON = A.String . eventStatusText

instance A.ToJSON NodeEvent where
  toJSON event =
    A.object
      [ "actionId" A..= event.actionId
      , "node" A..= event.node
      , "status" A..= event.status
      , "phase" A..= event.phase
      , "message" A..= event.message
      , "payload" A..= event.payload
      ]

instance A.ToJSON WsMessageKind where
  toJSON = A.String . renderWsMessageKind

instance A.FromJSON WsMessageKind where
  parseJSON = A.withText "WsMessageKind" $ \raw ->
    case parseWsMessageKind raw of
      Nothing -> fail ("unsupported websocket message kind: " <> T.unpack raw)
      Just kind -> pure kind

instance A.ToJSON WsEnvelope where
  toJSON envelope =
    A.object
      ( [ "version" A..= envelope.version
        , "kind" A..= envelope.kind
        , "messageId" A..= envelope.messageId
        , "timestamp" A..= envelope.timestamp
        , "node" A..= envelope.node
        , "payload" A..= envelope.payload
        ]
          <> catMaybes
            [ fmap ("jobId" A..=) envelope.jobId
            , fmap ("dispatchId" A..=) envelope.dispatchId
            , fmap ("actionId" A..=) envelope.actionId
            ]
      )

buildWsAuthPayload :: Text -> Text -> Text -> Text -> A.Value
buildWsAuthPayload nodeName token messageId timestamp =
  buildWsEnvelopeValue nodeName WsAuth messageId timestamp Nothing Nothing Nothing (A.object ["token" A..= token])

buildWsResumePayload :: Text -> A.Value -> Text -> Text -> A.Value
buildWsResumePayload nodeName resumeValue messageId timestamp =
  buildWsEnvelopeValue nodeName WsResume messageId timestamp Nothing Nothing Nothing resumeValue

buildWsNodeEventEnvelope :: Text -> Text -> Text -> Text -> Text -> NodeEvent -> A.Value
buildWsNodeEventEnvelope nodeName jobId dispatchId messageId timestamp event =
  buildWsEnvelopeValue nodeName (eventKind event.status) messageId timestamp (Just jobId) (Just dispatchId) event.actionId payloadValue
  where
    payloadValue = mergeEventPayload event

eventKind :: EventStatus -> WsMessageKind
eventKind status =
  case status of
    EventQueued -> WsProgress
    EventWaiting -> WsProgress
    EventRunning -> WsProgress
    EventSuccess -> WsActionResult
    EventFailed -> WsActionResult
    EventTimedOut -> WsActionResult

mergeEventPayload :: NodeEvent -> A.Value
mergeEventPayload event =
  case event.payload of
    A.Object obj ->
      A.Object
        ( addMaybeField "message" event.message
            ( addMaybeField "phase" event.phase
                (KM.insert "status" (A.toJSON event.status) obj)
            )
        )
    _ ->
      A.object
        ( [ "status" A..= event.status
          , "details" A..= event.payload
          ]
            <> maybe [] (pure . ("phase" A..=)) event.phase
            <> maybe [] (pure . ("message" A..=)) event.message
        )

addMaybeField :: Text -> Maybe Text -> KM.KeyMap A.Value -> KM.KeyMap A.Value
addMaybeField _ Nothing obj = obj
addMaybeField fieldName (Just value) obj = KM.insert (K.fromText fieldName) (A.String value) obj

decodeDeployJobMessage :: BL.ByteString -> Maybe DeployJob
decodeDeployJobMessage raw =
  A.decode raw >>= decodeDeployJobValue

decodeWsEnvelopeMessage :: BL.ByteString -> Maybe WsEnvelope
decodeWsEnvelopeMessage raw =
  A.decode raw >>= decodeWsEnvelopeValue

decodeDeployJobValue :: A.Value -> Maybe DeployJob
decodeDeployJobValue value = do
  envelope <- decodeWsEnvelopeValue value
  guard (envelope.kind == WsDeployJob)
  rawJobId <- envelope.jobId
  rawDispatchId <- envelope.dispatchId
  (commitSha, parsedIntent) <- decodeDeployJobPayload envelope.payload
  let parsedJob =
        DeployJob
          { jobId = rawJobId
          , dispatchId = rawDispatchId
          , commitSha
          , node = Just envelope.node
          , intent = parsedIntent
          }
  validateDeployJob parsedJob

decodeWsEnvelopeValue :: A.Value -> Maybe WsEnvelope
decodeWsEnvelopeValue = \case
  A.Object obj -> do
    version <- extractIntField "version" obj
    guard (version == 1)
    rawKind <- extractRequiredTextField "kind" obj
    parsedKind <- parseWsMessageKind rawKind
    messageId <- extractRequiredTextField "messageId" obj
    timestamp <- extractRequiredTextField "timestamp" obj
    node <- extractRequiredTextField "node" obj
    payload <- KM.lookup "payload" obj
    let envelope =
          WsEnvelope
            { version
            , kind = parsedKind
            , messageId
            , timestamp
            , node
            , jobId = extractTextField "jobId" obj
            , dispatchId = extractTextField "dispatchId" obj
            , actionId = extractTextField "actionId" obj
            , payload
            }
    validateWsEnvelope envelope
  _ -> Nothing

decodeDeployJobPayload :: A.Value -> Maybe (Text, DeployIntent)
decodeDeployJobPayload = \case
  A.Object obj -> do
    intentValue <- KM.lookup "intent" obj
    parsedIntent <- decodeIntentValue intentValue
    let commitSha =
          case KM.lookup "commitSha" obj of
            Just (A.String value) -> T.strip value
            _ -> ""
    pure (commitSha, parsedIntent)
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
      A.String rawActionId <- KM.lookup "actionId" obj
      A.String rawUser <- KM.lookup "user" obj
      A.String rawOp <- KM.lookup "op" obj
      parsedOp <- parseActionOp rawOp
      pure
        DeployAction
          { op = parsedOp
          , actionId = T.strip rawActionId
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
  | not (isSafeAtom job.dispatchId) = Nothing
  | job.intent.schemaVersion /= 1 = Nothing
  | not (maybe True isSafeAtom job.node) = Nothing
  | not (all validateAction job.intent.actions) = Nothing
  | otherwise = Just job

validateAction :: DeployAction -> Bool
validateAction action =
  isSafeAtom action.actionId
    && isSafeAtom action.user
    && maybe True isSafeAtom action.fromNode
    && maybe True isSafeAtom action.toNode
    && all isSafeAtom action.migrations

validateWsEnvelope :: WsEnvelope -> Maybe WsEnvelope
validateWsEnvelope envelope
  | requiresJobIdentity envelope.kind && envelope.jobId == Nothing = Nothing
  | requiresDispatchIdentity envelope.kind && envelope.dispatchId == Nothing = Nothing
  | requiresActionIdentity envelope.kind && envelope.actionId == Nothing = Nothing
  | otherwise = Just envelope

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

parseWsMessageKind :: Text -> Maybe WsMessageKind
parseWsMessageKind raw =
  case T.toLower (T.strip raw) of
    "hello" -> Just WsHello
    "auth" -> Just WsAuth
    "auth_ok" -> Just WsAuthOk
    "auth_error" -> Just WsAuthError
    "deploy_job" -> Just WsDeployJob
    "ack" -> Just WsAck
    "progress" -> Just WsProgress
    "action_result" -> Just WsActionResult
    "heartbeat" -> Just WsHeartbeat
    "resume" -> Just WsResume
    "error" -> Just WsError
    _ -> Nothing

renderWsMessageKind :: WsMessageKind -> Text
renderWsMessageKind = \case
  WsHello -> "hello"
  WsAuth -> "auth"
  WsAuthOk -> "auth_ok"
  WsAuthError -> "auth_error"
  WsDeployJob -> "deploy_job"
  WsAck -> "ack"
  WsProgress -> "progress"
  WsActionResult -> "action_result"
  WsHeartbeat -> "heartbeat"
  WsResume -> "resume"
  WsError -> "error"

extractTextField :: Text -> KM.KeyMap A.Value -> Maybe Text
extractTextField key obj =
  case KM.lookup (K.fromText key) obj of
    Just (A.String value) | T.strip value /= "" -> Just (T.strip value)
    _ -> Nothing

extractRequiredTextField :: Text -> KM.KeyMap A.Value -> Maybe Text
extractRequiredTextField = extractTextField

extractIntField :: Text -> KM.KeyMap A.Value -> Maybe Int
extractIntField key obj =
  case KM.lookup (K.fromText key) obj of
    Just (A.Number value) -> pure (round value)
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

requiresJobIdentity :: WsMessageKind -> Bool
requiresJobIdentity = \case
  WsDeployJob -> True
  WsAck -> True
  WsProgress -> True
  WsActionResult -> True
  WsError -> True
  _ -> False

requiresDispatchIdentity :: WsMessageKind -> Bool
requiresDispatchIdentity = \case
  WsDeployJob -> True
  WsAck -> True
  WsProgress -> True
  WsActionResult -> True
  WsError -> True
  _ -> False

requiresActionIdentity :: WsMessageKind -> Bool
requiresActionIdentity = \case
  WsProgress -> True
  WsActionResult -> True
  _ -> False

isSafeAtom :: Text -> Bool
isSafeAtom txt =
  let trimmed = T.strip txt
   in trimmed /= "" && T.all allowed trimmed
  where
    allowed c = isAsciiLower c || isDigit c || c == '-' || c == '_' || c == '.' || c == ':'

buildWsEnvelopeValue :: Text -> WsMessageKind -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> A.Value -> A.Value
buildWsEnvelopeValue nodeName kind messageId timestamp jobId dispatchId actionId payload =
  A.toJSON
    WsEnvelope
      { version = 1
      , kind
      , messageId
      , timestamp
      , node = nodeName
      , jobId
      , dispatchId
      , actionId
      , payload
      }
