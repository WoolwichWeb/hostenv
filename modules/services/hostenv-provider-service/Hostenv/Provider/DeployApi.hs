{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Hostenv.Provider.DeployApi
  ( NodeEvent(..)
  , ProjectedNodeEvent(..)
  , NodeEventIntakeDecision(..)
  , DeployUpdateEmitter
  , DeployFailureRecorder
  , DeploySuccessRecorder
  , buildDeployJobEnvelope
  , intentByJobHandler
  , intentByShaHandler
  , jobStatusHandler
  , jobStatusesHandler
  , jobActionsHandler
  , backupSnapshotHandler
    , eventHandler
    , processNodeEvent
    , validateIntent
  , acceptsNodeEvents
    , dispatchFingerprint
    , dispatchStableId
    , currentDispatchIdFor
    , extractBearer
  , dispatchForNode
  , backupSnapshotResponseValue
  , classifyProjectedNodeEvent
  , projectNodeEvent
  , isValidDeployWsAuth
  , normalizeStatus
  , shouldDispatchJob
  ) where

import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.Aeson.Types (Pair)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (toList)
import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Control.Monad.IO.Class (liftIO)
import Servant

import Hostenv.Provider.Config (AppConfig(..), lookupDeployNodeAuthToken)
import Hostenv.Provider.DB (DeployAction(..), DeployBackupSnapshot(..), DeployBackupSnapshotLookup(..), DeployBackupSnapshotMetadata(..), DeployEvent(..), DeployStatus(..), appendDeployEvent, applyDeployActionEvent, deployActionId, deployIntentExists, loadDeployActionById, loadDeployActions, loadDeployActionsByNode, loadDeployBackupSnapshot, loadDeployIntentByJob, loadDeployIntentBySha, loadDeployIntentNodes, loadDeployStatusByNode, loadDeployStatuses)
import Hostenv.Provider.Jobs (jobSummaryStatus, loadJobById)
import Hostenv.Provider.Logging (ProviderLogFields(..), ProviderSeverity(..), logProviderEvent, providerLogFields)


data NodeEvent = NodeEvent
  { kind :: Text
  , messageId :: Text
  , timestamp :: Text
  , jobId :: Text
  , dispatchId :: Text
  , actionId :: Maybe Text
  , node :: Text
  , status :: Text
  , phase :: Maybe Text
  , message :: Maybe Text
  , payload :: A.Value
  } deriving (Eq, Show)

data ProjectedNodeEvent = ProjectedNodeEvent
  { projectedNode :: Text
  , projectedStatus :: Text
  , projectedPhase :: Maybe Text
  , projectedMessage :: Maybe Text
  , projectedActionId :: Maybe Text
  , projectedDispatchId :: Maybe Text
  } deriving (Eq, Show)

data NodeEventIntakeDecision
  = AcceptProjectedNodeEvent
  | IgnoreDuplicateProjectedNodeEvent
  | IgnoreStaleProjectedNodeEvent
  | RejectUnknownProjectedNodeEvent
  deriving (Eq, Show)

instance A.FromJSON NodeEvent where
  parseJSON = A.withObject "NodeEvent" $ \o -> do
    version <- o A..: "version"
    if version /= (1 :: Int)
      then fail "unsupported deploy protocol version"
      else pure ()
    kindText <- requiredNonBlankText "kind" o
    if kindText `notElem` allowedNodeEventKinds
      then fail "unsupported deploy event kind"
      else pure ()
    messageIdText <- requiredNonBlankText "messageId" o
    timestampText <- requiredNonBlankText "timestamp" o
    jobIdText <- requiredNonBlankText "jobId" o
    dispatchIdText <- requiredNonBlankText "dispatchId" o
    nodeText <- requiredNonBlankText "node" o
    payloadValue <- requiredObjectField "payload" o
    actionIdText <- optionalNonBlankText "actionId" o
    let statusText =
          fromMaybe
            "running"
            ( firstNonBlankText
                [ payloadTextAtPath ["status"] (Just payloadValue)
                ]
            )
        phaseText =
          firstNonBlankText
            [ payloadTextAtPath ["phase"] (Just payloadValue)
            ]
        messageText =
          firstNonBlankText
            [ payloadTextAtPath ["message"] (Just payloadValue)
            ]
    NodeEvent
      <$> pure kindText
      <*> pure messageIdText
      <*> pure timestampText
      <*> pure jobIdText
      <*> pure dispatchIdText
      <*> pure actionIdText
      <*> pure nodeText
      <*> pure statusText
      <*> pure phaseText
      <*> pure messageText
      <*> pure payloadValue

instance A.ToJSON NodeEvent where
  toJSON event =
    A.object
      [ "version" A..= (1 :: Int)
      , "kind" A..= event.kind
      , "messageId" A..= event.messageId
      , "timestamp" A..= event.timestamp
      , "jobId" A..= event.jobId
      , "dispatchId" A..= event.dispatchId
      , "actionId" A..= event.actionId
      , "node" A..= event.node
      , "payload" A..= event.payload
      ]

buildDeployJobEnvelope :: UTCTime -> Text -> Text -> Text -> Text -> Text -> A.Value -> [DeployAction] -> A.Value
buildDeployJobEnvelope timestampText messageIdText jobIdText commitSha nodeName dispatchIdText filteredIntent filteredActions =
  A.object
    [ "version" A..= (1 :: Int)
    , "kind" A..= ("deploy_job" :: Text)
    , "messageId" A..= messageIdText
    , "timestamp" A..= timestampText
    , "node" A..= nodeName
    , "jobId" A..= jobIdText
    , "dispatchId" A..= dispatchIdText
    , "payload" A..=
        A.object
          [ "commitSha" A..= commitSha
          , "intent" A..= filteredIntent
          , "actions" A..= filteredActions
          ]
    ]

payloadTextAtPath :: [Text] -> Maybe A.Value -> Maybe Text
payloadTextAtPath path = (>>= valueTextAtPath path)

valueTextAtPath :: [Text] -> A.Value -> Maybe Text
valueTextAtPath [] _ = Nothing
valueTextAtPath [segment] (A.Object obj) =
  case KM.lookup (K.fromText segment) obj of
    Just (A.String txt) -> Just txt
    _ -> Nothing
valueTextAtPath (segment:rest) (A.Object obj) =
  KM.lookup (K.fromText segment) obj >>= valueTextAtPath rest
valueTextAtPath _ _ = Nothing

firstNonBlankText :: [Maybe Text] -> Maybe Text
firstNonBlankText values =
  listToMaybe
    [ trimmed
    | Just value <- values
    , let trimmed = T.strip value
    , trimmed /= ""
    ]

type DeployUpdateEmitter = A.Value -> IO ()
type DeployFailureRecorder = Text -> Text -> IO ()
type DeploySuccessRecorder = Text -> IO ()

intentByJobHandler :: AppConfig -> Text -> Text -> Maybe Text -> Handler A.Value
intentByJobHandler cfg jobId nodeName mAuth = do
  requireNodeAuth cfg nodeName mAuth
  intent <- liftIO (loadDeployIntentByJob cfg jobId nodeName)
  case intent of
    Nothing -> throwError err404
    Just payload -> do
      mJob <- liftIO (loadJobById cfg jobId)
      case mJob of
        Nothing -> throwError err404
        Just job ->
          if not (acceptsNodeEvents (jobSummaryStatus job))
            then throwError err404
            else do
              validated <- maybe (throwError err500) pure (validateIntent payload)
              pure
                ( A.object
                    [ "jobId" A..= jobId
                    , "node" A..= nodeName
                    , "intent" A..= validated
                    ]
                )

intentByShaHandler :: AppConfig -> Text -> Text -> Maybe Text -> Handler A.Value
intentByShaHandler cfg commitSha nodeName mAuth = do
  requireNodeAuth cfg nodeName mAuth
  resolved <- liftIO (loadDeployIntentBySha cfg commitSha nodeName)
  case resolved of
    Nothing -> throwError err404
    Just (jobId, payload) -> do
      mJob <- liftIO (loadJobById cfg jobId)
      case mJob of
        Nothing -> throwError err404
        Just job ->
          if not (acceptsNodeEvents (jobSummaryStatus job))
            then throwError err404
            else do
              validated <- maybe (throwError err500) pure (validateIntent payload)
              pure
                ( A.object
                    [ "jobId" A..= jobId
                    , "commitSha" A..= commitSha
                    , "node" A..= nodeName
                    , "intent" A..= validated
                    ]
                )

jobStatusHandler :: AppConfig -> Text -> Text -> Maybe Text -> Handler A.Value
jobStatusHandler cfg jobId nodeName mAuth = do
  requireNodeAuth cfg nodeName mAuth
  hasIntent <- liftIO (deployIntentExists cfg jobId nodeName)
  if not hasIntent
    then throwError err404
    else do
      mStatus <- liftIO (loadDeployStatusByNode cfg jobId nodeName)
      pure $
        A.object
          [ "jobId" A..= jobId
          , "node" A..= nodeName
          , "status" A..= mStatus
          ]

jobStatusesHandler :: AppConfig -> Text -> Text -> Maybe Text -> Handler A.Value
jobStatusesHandler cfg jobId nodeName mAuth = do
  requireNodeAuth cfg nodeName mAuth
  hasIntent <- liftIO (deployIntentExists cfg jobId nodeName)
  if not hasIntent
    then throwError err404
    else do
      statuses <- liftIO (loadDeployStatuses cfg jobId)
      pure
        ( A.object
            [ "jobId" A..= jobId
            , "node" A..= nodeName
            , "statuses" A..= statuses
            ]
        )

jobActionsHandler :: AppConfig -> Text -> Text -> Maybe Text -> Handler A.Value
jobActionsHandler cfg jobId nodeName mAuth = do
  requireNodeAuth cfg nodeName mAuth
  hasIntent <- liftIO (deployIntentExists cfg jobId nodeName)
  if not hasIntent
    then throwError err404
    else do
      actions <- liftIO (loadDeployActionsByNode cfg jobId nodeName)
      pure
        ( A.object
            [ "jobId" A..= jobId
            , "node" A..= nodeName
            , "actions" A..= actions
            ]
        )

backupSnapshotHandler :: AppConfig -> Text -> Text -> Text -> Text -> Maybe Text -> Handler A.Value
backupSnapshotHandler cfg jobId nodeName sourceNode userName mAuth = do
  requireNodeAuth cfg nodeName mAuth
  liftIO
    ( logProviderEvent
        ( (providerLogFields "deploy_backup_snapshot" ProviderSeverityInfo)
            { entryJobId = Just jobId
            , entryNode = Just nodeName
            , entryPhase = Just "backup"
            , entryDecision = Just "lookup"
            , entryReason = Just "snapshot_requested"
            }
        )
        [ "source_node" A..= sourceNode
        , "snapshot_user" A..= userName
        ]
    )
  hasIntent <- liftIO (deployIntentExists cfg jobId nodeName)
  if not hasIntent
    then throwError err404
    else do
      snapshotLookup <- liftIO (loadDeployBackupSnapshot cfg jobId sourceNode userName)
      case snapshotLookup of
        DeployBackupSnapshotMissing -> do
          liftIO
            ( logProviderEvent
                ( (providerLogFields "deploy_backup_snapshot" ProviderSeverityWarn)
                    { entryJobId = Just jobId
                    , entryNode = Just nodeName
                    , entryPhase = Just "backup"
                    , entryDecision = Just "reject"
                    , entryReason = Just "missing"
                    }
                )
                [ "source_node" A..= sourceNode
                , "snapshot_user" A..= userName
                ]
            )
          throwError err404
        DeployBackupSnapshotMalformed -> do
          liftIO
            ( logProviderEvent
                ( (providerLogFields "deploy_backup_snapshot" ProviderSeverityError)
                    { entryJobId = Just jobId
                    , entryNode = Just nodeName
                    , entryPhase = Just "backup"
                    , entryDecision = Just "reject"
                    , entryReason = Just "malformed"
                    }
                )
                [ "source_node" A..= sourceNode
                , "snapshot_user" A..= userName
                ]
            )
          throwError err500
        DeployBackupSnapshotAvailable snapshot@DeployBackupSnapshot { snapshotMetadata = DeployBackupSnapshotMetadata { snapshotChecksum = checksum } } -> do
          liftIO
            ( logProviderEvent
                ( (providerLogFields "deploy_backup_snapshot" ProviderSeverityInfo)
                    { entryJobId = Just jobId
                    , entryNode = Just nodeName
                    , entryPhase = Just "backup"
                    , entryDecision = Just "accept"
                    , entryReason = Just "available"
                    }
                )
                [ "source_node" A..= sourceNode
                , "snapshot_user" A..= userName
                , "checksum" A..= checksum
                ]
            )
          pure
            (backupSnapshotResponseValue jobId nodeName sourceNode userName snapshot)

backupSnapshotResponseValue :: Text -> Text -> Text -> Text -> DeployBackupSnapshot -> A.Value
backupSnapshotResponseValue jobId nodeName sourceNode userName snapshot =
  A.object
    [ "jobId" A..= jobId
    , "node" A..= nodeName
    , "sourceNode" A..= sourceNode
    , "user" A..= userName
    , "metadata" A..= snapshot.snapshotMetadata
    , "payload" A..= snapshot.snapshotPayload
    ]

eventHandler :: AppConfig -> DeployUpdateEmitter -> DeployFailureRecorder -> DeploySuccessRecorder -> Text -> Maybe Text -> NodeEvent -> Handler NoContent
eventHandler cfg emitUpdate recordFailure recordSuccess jobId mAuth event = do
  let nodeName = (projectNodeEvent event).projectedNode
  requireNodeAuth cfg nodeName mAuth
  result <- liftIO (processNodeEvent cfg emitUpdate recordFailure recordSuccess jobId event)
  either throwError pure result

processNodeEvent :: AppConfig -> DeployUpdateEmitter -> DeployFailureRecorder -> DeploySuccessRecorder -> Text -> NodeEvent -> IO (Either ServerError NoContent)
processNodeEvent cfg emitUpdate recordFailure recordSuccess jobId event = do
  let projectedEvent = projectNodeEvent event
      nodeName = projectedEvent.projectedNode
      statusText = normalizeStatus projectedEvent.projectedStatus
      actionTargetRequired = actionIdRequiredForPhase projectedEvent.projectedPhase || isJust projectedEvent.projectedActionId
      reject severity decisionReason serverError extras = do
        logNodeEventOutcome (nodeEventLogFields jobId projectedEvent statusText severity) "reject" decisionReason extras
        pure (Left serverError)
      ignore decisionReason extras = do
        logNodeEventOutcome (nodeEventLogFields jobId projectedEvent statusText ProviderSeverityInfo) "ignore" decisionReason extras
        pure (Right NoContent)
      acceptStored actionUpdateCount storedEvent latestStatuses latestActions = do
        let DeployEvent eventId _ _ _ _ _ _ = storedEvent
        logNodeEventOutcome
          (nodeEventLogFields jobId projectedEvent statusText ProviderSeverityInfo)
          "accept"
          "stored"
          [ "action_row_update_count" A..= actionUpdateCount
          , "deploy_event_id" A..= eventId
          ]
        emitUpdate (A.object ["type" A..= ("deploy_event" :: Text), "jobId" A..= jobId, "event" A..= storedEvent])
        emitUpdate (A.object ["type" A..= ("deploy_status" :: Text), "jobId" A..= jobId, "statuses" A..= latestStatuses])
        emitUpdate (A.object ["type" A..= ("deploy_actions" :: Text), "jobId" A..= jobId, "actions" A..= latestActions])
        if statusText == "failed" || statusText == "timed_out"
          then recordFailure jobId (renderFailureMessage nodeName statusText projectedEvent.projectedMessage)
          else
            if statusText /= "success"
              then pure ()
              else do
                intentNodes <- loadDeployIntentNodes cfg jobId
                let latestByNode = Map.fromList [(nodeVal, (statusVal, phaseVal)) | DeployStatus _ nodeVal statusVal phaseVal _ _ <- latestStatuses]
                    nodeComplete nodeVal = Map.lookup nodeVal latestByNode == Just ("success" :: Text, Just ("intent" :: Text))
                    actionSucceeded :: DeployAction -> Bool
                    actionSucceeded action = action.status == ("success" :: Text)
                    allActionsSucceeded = all actionSucceeded latestActions
                    allSucceeded = not (null intentNodes) && all nodeComplete intentNodes && allActionsSucceeded
                if allSucceeded
                  then recordSuccess jobId
                  else pure ()
        pure (Right NoContent)

  if nodeName == ""
    then reject ProviderSeverityWarn "blank_node" err400 []
    else if T.strip event.jobId /= jobId
      then reject ProviderSeverityWarn "job_id_mismatch" err400 []
      else if statusText == ""
        then reject ProviderSeverityWarn "invalid_status" err400 []
        else if actionTargetRequired && projectedEvent.projectedActionId == Nothing
          then reject ProviderSeverityWarn "missing_action_id" err400 []
          else do
            hasIntent <- deployIntentExists cfg jobId nodeName
            if not hasIntent
              then reject ProviderSeverityWarn "missing_intent" err404 []
              else do
                mJob <- loadJobById cfg jobId
                case mJob of
                  Nothing -> reject ProviderSeverityWarn "missing_job" err404 []
                  Just job ->
                    if not (acceptsNodeEvents (jobSummaryStatus job))
                      then reject ProviderSeverityWarn "job_not_waiting" err409 ["job_status" A..= jobSummaryStatus job]
                      else do
                        targetAction <-
                          case projectedEvent.projectedActionId of
                            Nothing -> pure Nothing
                            Just actionId -> loadDeployActionById cfg jobId nodeName actionId
                        currentDispatchId <- loadCurrentDispatchId cfg jobId nodeName
                        case classifyProjectedNodeEvent actionTargetRequired currentDispatchId targetAction event.dispatchId statusText of
                          RejectUnknownProjectedNodeEvent -> reject ProviderSeverityWarn "unknown_action" err404 []
                          IgnoreDuplicateProjectedNodeEvent -> ignore "duplicate_status" ["action_row_update_count" A..= (0 :: Int)]
                          IgnoreStaleProjectedNodeEvent -> ignore "stale_dispatch" ["current_dispatch_id" A..= currentDispatchId]
                          AcceptProjectedNodeEvent -> do
                            actionUpdateCount <- applyDeployActionEvent cfg jobId nodeName statusText projectedEvent.projectedPhase projectedEvent.projectedMessage projectedEvent.projectedActionId
                            let actionProjectionExpected = isJust projectedEvent.projectedActionId
                            if actionProjectionExpected && actionUpdateCount == 0
                              then ignore "no_action_rows_updated" ["action_row_update_count" A..= actionUpdateCount]
                              else do
                                let storedPayload = attachProjectedIdentity projectedEvent event.payload
                                mStoredEvent <- appendDeployEvent cfg jobId nodeName statusText projectedEvent.projectedPhase projectedEvent.projectedMessage (Just storedPayload)
                                case mStoredEvent of
                                  Nothing -> ignore "duplicate_event" ["action_row_update_count" A..= actionUpdateCount]
                                  Just storedEvent -> do
                                    latestStatuses <- loadDeployStatuses cfg jobId
                                    latestActions <- loadDeployActions cfg jobId
                                    acceptStored actionUpdateCount storedEvent latestStatuses latestActions

nodeEventLogFields :: Text -> ProjectedNodeEvent -> Text -> ProviderSeverity -> ProviderLogFields
nodeEventLogFields jobId projectedEvent statusText severity =
  (providerLogFields "deploy_node_event_intake" severity)
    { entryJobId = Just jobId
    , entryNode = normalizeText projectedEvent.projectedNode
    , entryDispatchId = projectedEvent.projectedDispatchId
    , entryActionId = projectedEvent.projectedActionId
    , entryPhase = projectedEvent.projectedPhase
    , entryStatus = normalizeText statusText
    , entryProtocolVersion = Just 1
    }

logNodeEventOutcome :: ProviderLogFields -> Text -> Text -> [Pair] -> IO ()
logNodeEventOutcome fields decisionText reasonText extras =
  logProviderEvent
    ( fields
        { entryDecision = Just decisionText
        , entryReason = Just reasonText
        }
    )
    extras

requireNodeAuth :: AppConfig -> Text -> Maybe Text -> Handler ()
requireNodeAuth cfg nodeName mAuth =
  case (lookupDeployNodeAuthToken cfg nodeName, extractBearer mAuth) of
    (Nothing, _) -> throwError err403
    (_, Nothing) -> throwError err403
    (Just expected, Just supplied) ->
      if expected == supplied
        then pure ()
        else throwError err403

validateIntent :: A.Value -> Maybe A.Value
validateIntent value =
  case value of
    A.Object obj -> do
      schema <- KM.lookup (K.fromString "schemaVersion") obj
      actions <- KM.lookup (K.fromString "actions") obj
      case (schema, actions) of
        (A.Number n, A.Array arr) | n == 1 && all validateAction arr -> Just value
        _ -> Nothing
    _ -> Nothing
  where
    validateAction actionValue =
      case actionValue of
        A.Object actionObj ->
          case (KM.lookup (K.fromString "user") actionObj, KM.lookup (K.fromString "op") actionObj) of
            (Just (A.String userName), Just (A.String operation)) ->
              validUser userName
                && operation `elem` allowedOps
                && validOptionalNode (KM.lookup (K.fromString "fromNode") actionObj)
                && validOptionalNode (KM.lookup (K.fromString "toNode") actionObj)
                && validMigrations (KM.lookup (K.fromString "migrations") actionObj)
            _ -> False
        _ -> False

    validUser userName = isSafeAtom (T.strip userName)

    validOptionalNode nodeValue =
      case nodeValue of
        Nothing -> True
        Just A.Null -> True
        Just (A.String nodeName) -> isSafeAtom (T.strip nodeName)
        _ -> False

    validMigrations migrationsValue =
      case migrationsValue of
        Nothing -> True
        Just (A.Array arr) -> all validMigration arr
        _ -> False

    validMigration entry =
      case entry of
        A.String txt -> isSafeAtom (T.strip txt)
        _ -> False

    isSafeAtom txt =
      T.length txt > 0 && T.all isAllowed txt

    isAllowed c =
      isAsciiLower c || isDigit c || c == '-' || c == '_' || c == '.' || c == ':'

extractBearer :: Maybe Text -> Maybe Text
extractBearer mHeader =
  case mHeader of
    Nothing -> Nothing
    Just raw ->
      let trimmed = T.strip raw
          lower = T.toLower trimmed
       in if "bearer " `T.isPrefixOf` lower
            then Just (T.strip (T.drop 7 trimmed))
            else Nothing

normalizeStatus :: Text -> Text
normalizeStatus raw =
  let status = T.toLower (T.strip raw)
   in if status `elem` allowedStatuses then status else ""

acceptsNodeEvents :: Text -> Bool
acceptsNodeEvents raw =
  T.toLower (T.strip raw) == "waiting"

allowedStatuses :: [Text]
allowedStatuses =
  [ "queued"
  , "waiting"
  , "running"
  , "success"
  , "failed"
  , "timed_out"
  ]

allowedOps :: [Text]
allowedOps =
  [ "activate"
  , "reload"
  , "backup"
  , "restore"
  , "deactivate"
  ]

renderFailureMessage :: Text -> Text -> Maybe Text -> Text
renderFailureMessage nodeName status mMessage =
  let base =
        if status == "timed_out"
          then "Node deploy action timed out"
          else "Node deploy action failed"
      detail =
        case mMessage of
          Just msg | T.strip msg /= "" -> ": " <> msg
          _ -> ""
   in base <> " on " <> nodeName <> detail

hasPendingActions :: [DeployAction] -> Bool
hasPendingActions actions =
  any (\action -> action.status `elem` ["queued", "waiting", "running"]) actions

intentHasActions :: A.Value -> Bool
intentHasActions value =
  case value of
    A.Object obj ->
      case KM.lookup (K.fromString "actions") obj of
        Just (A.Array arr) -> not (null arr)
        _ -> False
    _ -> False

dispatchForNode :: A.Value -> [DeployAction] -> Text -> Maybe (A.Value, [DeployAction])
dispatchForNode validatedIntent allActions nodeName =
  let nodeActions = filter ((== nodeName) . (.node)) allActions
      executable = executableActionIndexes nodeActions allActions
      filteredActions = filter (\action -> Set.member action.actionIndex executable) nodeActions
      filteredIntent = filterIntentActionsByIndexes validatedIntent executable
   in fmap (\intent -> (intent, filteredActions)) filteredIntent

dispatchFingerprint :: Text -> A.Value -> [DeployAction] -> Text
dispatchFingerprint jobId filteredIntent actions =
  T.intercalate ":"
    [ jobId
    , TE.decodeUtf8 (BL.toStrict (A.encode filteredIntent))
    , T.intercalate "," (map render actions)
    ]
  where
    render :: DeployAction -> Text
    render action =
      T.intercalate "/" [action.node, T.pack (show action.actionIndex), action.op, action.user, action.status]

dispatchStableId :: Text -> Text -> A.Value -> [DeployAction] -> Text
dispatchStableId jobId nodeName filteredIntent actions =
  "dispatch-"
    <> TE.decodeUtf8
      (BAE.convertToBase BAE.Base16 (hash payload :: Digest SHA256))
  where
    payload =
      BL.toStrict
        ( A.encode
            ( A.object
                [ "jobId" A..= jobId
                , "node" A..= nodeName
                , "intent" A..= filteredIntent
                , "actionIds" A..= map deployActionId actions
                ]
            )
        )

shouldDispatchJob :: A.Value -> [DeployAction] -> [DeployAction] -> Maybe Text -> Text -> Bool
shouldDispatchJob validatedIntent nodeActions filteredActions mLastDispatchId dispatchId =
  (not (null filteredActions) || (not (intentHasActions validatedIntent) && not (hasPendingActions nodeActions)))
    && mLastDispatchId /= Just dispatchId

classifyProjectedNodeEvent :: Bool -> Maybe Text -> Maybe DeployAction -> Text -> Text -> NodeEventIntakeDecision
classifyProjectedNodeEvent actionTargetRequired mCurrentDispatchId mTargetAction incomingDispatchId incomingStatus
  | actionTargetRequired && mTargetAction == Nothing = RejectUnknownProjectedNodeEvent
  | mCurrentDispatchId /= Just (T.strip incomingDispatchId) = IgnoreStaleProjectedNodeEvent
  | maybe False (sameStatus incomingStatus) mTargetAction = IgnoreDuplicateProjectedNodeEvent
  | otherwise = AcceptProjectedNodeEvent
  where
    sameStatus :: Text -> DeployAction -> Bool
    sameStatus statusText action = normalizeStatus action.status == normalizeStatus statusText

loadCurrentDispatchId :: AppConfig -> Text -> Text -> IO (Maybe Text)
loadCurrentDispatchId cfg jobId nodeName = do
  mIntent <- loadDeployIntentByJob cfg jobId nodeName
  case mIntent >>= validateIntent of
    Nothing -> pure Nothing
    Just validatedIntent -> do
      allActions <- loadDeployActions cfg jobId
      pure (currentDispatchIdFor jobId nodeName validatedIntent allActions)

currentDispatchIdFor :: Text -> Text -> A.Value -> [DeployAction] -> Maybe Text
currentDispatchIdFor jobId nodeName validatedIntent allActions =
  let nodeActions = filter ((== nodeName) . (.node)) allActions
   in if null nodeActions && not (intentHasActions validatedIntent)
        then Nothing
        else Just (dispatchStableId jobId nodeName validatedIntent nodeActions)

executableActionIndexes :: [DeployAction] -> [DeployAction] -> Set.Set Int
executableActionIndexes nodeActions allActions =
  Set.fromList
    [ action.actionIndex
    | action <- nodeActions
    , action.status `elem` ["queued", "waiting", "running"]
    , canRun action
    ]
  where
    pendingStatuses :: [Text]
    pendingStatuses = ["queued", "waiting", "running"]

    statusMap :: Map.Map Int Text
    statusMap = Map.fromList [(entry.actionIndex, entry.status) | entry <- nodeActions]

    pendingByNodeAndUser :: Text -> Text -> [DeployAction]
    pendingByNodeAndUser opName userName =
      filter
        (\entry -> entry.op == opName && entry.user == userName && entry.status `elem` pendingStatuses)
        nodeActions

    hasSucceededGlobal :: Text -> Text -> Bool
    hasSucceededGlobal opName userName =
      any (\entry -> entry.op == opName && entry.user == userName && entry.status == "success") allActions

    priorIndexes :: Int -> [Int]
    priorIndexes actionIndex =
      mapMaybe
        (\entry -> if entry.actionIndex < actionIndex then Just entry.actionIndex else Nothing)
        nodeActions

    priorComplete :: Int -> Bool
    priorComplete actionIndex =
      all
        (\idx -> Map.lookup idx statusMap == Just "success")
        (priorIndexes actionIndex)

    canRun :: DeployAction -> Bool
    canRun action
      | action.op == "restore" =
          case pendingByNodeAndUser "restore" action.user of
            (firstRestore:_) | firstRestore.actionIndex == action.actionIndex ->
              hasSucceededGlobal "backup" action.user && priorComplete action.actionIndex
            _ -> False
      | action.op == "deactivate" =
          hasSucceededGlobal "restore" action.user && priorComplete action.actionIndex
      | otherwise = priorComplete action.actionIndex

filterIntentActionsByIndexes :: A.Value -> Set.Set Int -> Maybe A.Value
filterIntentActionsByIndexes value allowedIndexes =
  case value of
    A.Object obj ->
      case KM.lookup (K.fromString "actions") obj of
        Just (A.Array arr) ->
          let indexed = zip [0..] (toList arr)
              kept = [item | (idx, item) <- indexed, Set.member idx allowedIndexes]
           in Just (A.Object (KM.insert (K.fromString "actions") (A.toJSON kept) obj))
        _ -> Nothing
    _ -> Nothing

projectNodeEvent :: NodeEvent -> ProjectedNodeEvent
projectNodeEvent event =
  ProjectedNodeEvent
    { projectedNode = T.strip event.node
    , projectedStatus = T.strip event.status
    , projectedPhase = normalizeMaybeText event.phase
    , projectedMessage = normalizeMaybeText event.message
    , projectedActionId = normalizeMaybeText event.actionId
    , projectedDispatchId = Just (T.strip event.dispatchId)
    }

isValidDeployWsAuth :: AppConfig -> Text -> BL.ByteString -> Bool
isValidDeployWsAuth cfg nodeName rawMessage =
  case A.decode rawMessage >>= decodeDeployWsAuth of
    Just auth
      | auth.authNode == T.strip nodeName ->
          case lookupDeployNodeAuthToken cfg nodeName of
            Just expected -> expected == auth.authToken
            Nothing -> False
    _ -> False

allowedNodeEventKinds :: [Text]
allowedNodeEventKinds = ["progress", "action_result"]

data DeployWsAuth = DeployWsAuth
  { authNode :: Text
  , authToken :: Text
  }

decodeDeployWsAuth :: A.Value -> Maybe DeployWsAuth
decodeDeployWsAuth = \case
  A.Object obj -> do
    version <- lookupIntField "version" obj
    if version /= 1
      then Nothing
      else do
        kindText <- lookupRequiredTextField "kind" obj
        if kindText /= "auth"
          then Nothing
          else do
            _ <- lookupRequiredTextField "messageId" obj
            _ <- lookupRequiredTextField "timestamp" obj
            nodeName <- lookupRequiredTextField "node" obj
            payloadObj <- lookupObjectField "payload" obj
            tokenText <- lookupRequiredTextField "token" payloadObj
            pure DeployWsAuth { authNode = nodeName, authToken = tokenText }
  _ -> Nothing

requiredNonBlankText :: Text -> KM.KeyMap A.Value -> Parser Text
requiredNonBlankText fieldName obj =
  case lookupRequiredTextField fieldName obj of
    Just value -> pure value
    Nothing -> fail ("missing or invalid field: " <> T.unpack fieldName)

requiredObjectField :: Text -> KM.KeyMap A.Value -> Parser A.Value
requiredObjectField fieldName obj =
  case lookupObjectField fieldName obj of
    Just payloadValue -> pure (A.Object payloadValue)
    Nothing -> fail ("missing or invalid object field: " <> T.unpack fieldName)

lookupObjectField :: Text -> KM.KeyMap A.Value -> Maybe (KM.KeyMap A.Value)
lookupObjectField fieldName obj =
  case KM.lookup (K.fromText fieldName) obj of
    Just (A.Object payloadObj) -> Just payloadObj
    _ -> Nothing

lookupRequiredTextField :: Text -> KM.KeyMap A.Value -> Maybe Text
lookupRequiredTextField fieldName obj =
  case KM.lookup (K.fromText fieldName) obj of
    Just (A.String value) ->
      let trimmed = T.strip value
       in if trimmed == "" then Nothing else Just trimmed
    _ -> Nothing

optionalNonBlankText :: Text -> KM.KeyMap A.Value -> Parser (Maybe Text)
optionalNonBlankText fieldName obj =
  case KM.lookup (K.fromText fieldName) obj of
    Nothing -> pure Nothing
    Just A.Null -> pure Nothing
    Just (A.String value) -> pure (normalizeMaybeText (Just value))
    _ -> fail ("invalid text field: " <> T.unpack fieldName)

normalizeMaybeText :: Maybe Text -> Maybe Text
normalizeMaybeText = (>>= normalizeText)

normalizeText :: Text -> Maybe Text
normalizeText raw =
  let trimmed = T.strip raw
   in if trimmed == "" then Nothing else Just trimmed

lookupIntField :: Text -> KM.KeyMap A.Value -> Maybe Int
lookupIntField fieldName obj =
  case KM.lookup (K.fromText fieldName) obj of
    Just (A.Number value) -> Just (round value)
    _ -> Nothing

actionIdRequiredForPhase :: Maybe Text -> Bool
actionIdRequiredForPhase mPhase =
  case fmap (T.toLower . T.strip) mPhase of
    Just phaseName -> phaseName `elem` allowedOps
    _ -> False

attachProjectedIdentity :: ProjectedNodeEvent -> A.Value -> A.Value
attachProjectedIdentity projectedEvent payloadValue =
  case payloadValue of
    A.Object payloadObj ->
      A.Object
        ( addOptionalTextField "actionId" projectedEvent.projectedActionId
            (addOptionalTextField "dispatchId" projectedEvent.projectedDispatchId payloadObj)
        )
    _ -> payloadValue

addOptionalTextField :: Text -> Maybe Text -> KM.KeyMap A.Value -> KM.KeyMap A.Value
addOptionalTextField _ Nothing obj = obj
addOptionalTextField fieldName (Just value) obj = KM.insert (K.fromText fieldName) (A.String value) obj
