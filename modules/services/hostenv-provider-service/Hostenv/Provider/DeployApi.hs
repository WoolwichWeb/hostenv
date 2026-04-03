{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.DeployApi
  ( NodeEvent(..)
  , ProjectedNodeEvent(..)
  , DeployUpdateEmitter
  , DeployFailureRecorder
  , DeploySuccessRecorder
  , intentByJobHandler
  , intentByShaHandler
  , jobStatusHandler
  , jobStatusesHandler
  , jobActionsHandler
  , backupSnapshotHandler
  , eventHandler
  , validateIntent
  , acceptsNodeEvents
  , dispatchFingerprint
  , dispatchStableId
  , extractBearer
  , dispatchForNode
  , projectNodeEvent
  , isValidDeployWsAuth
  , normalizeStatus
  , shouldDispatchJob
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (toList)
import Data.Char (isAsciiLower, isDigit)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Servant

import Hostenv.Provider.Config (AppConfig(..), lookupDeployNodeAuthToken)
import Hostenv.Provider.DB (DeployAction(..), DeployStatus(..), appendDeployEvent, applyDeployActionEvent, deployActionId, deployIntentExists, loadDeployActions, loadDeployActionsByNode, loadDeployBackupSnapshot, loadDeployIntentByJob, loadDeployIntentBySha, loadDeployIntentNodes, loadDeployStatusByNode, loadDeployStatuses)
import Hostenv.Provider.Jobs (jobSummaryStatus, loadJobById)


data NodeEvent = NodeEvent
  { node :: Text
  , status :: Text
  , phase :: Maybe Text
  , message :: Maybe Text
  , payload :: Maybe A.Value
  } deriving (Eq, Show)

data ProjectedNodeEvent = ProjectedNodeEvent
  { projectedNode :: Text
  , projectedStatus :: Text
  , projectedPhase :: Maybe Text
  , projectedMessage :: Maybe Text
  , projectedActionId :: Maybe Text
  , projectedDispatchId :: Maybe Text
  } deriving (Eq, Show)

instance A.FromJSON NodeEvent where
  parseJSON = A.withObject "NodeEvent" $ \o -> do
    payloadValue <- o A..:? "payload"
    rawStatus <- o A..:? "status"
    rawPhase <- o A..:? "phase"
    rawMessage <- o A..:? "message"
    let statusText =
          fromMaybe
            "running"
            ( firstNonBlankText
                [ rawStatus
                , payloadTextAtPath ["status"] payloadValue
                , payloadTextAtPath ["event", "status"] payloadValue
                ]
            )
        phaseText =
          firstNonBlankText
            [ rawPhase
            , payloadTextAtPath ["phase"] payloadValue
            , payloadTextAtPath ["action", "phase"] payloadValue
            ]
        messageText =
          firstNonBlankText
            [ rawMessage
            , payloadTextAtPath ["message"] payloadValue
            , payloadTextAtPath ["event", "message"] payloadValue
            , payloadTextAtPath ["detail"] payloadValue
            ]
    NodeEvent
      <$> o A..: "node"
      <*> pure statusText
      <*> pure phaseText
      <*> pure messageText
      <*> pure payloadValue

instance A.ToJSON NodeEvent where
  toJSON event =
    A.object
      [ "node" A..= event.node
      , "status" A..= event.status
      , "phase" A..= event.phase
      , "message" A..= event.message
      , "payload" A..= event.payload
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
  hasIntent <- liftIO (deployIntentExists cfg jobId nodeName)
  if not hasIntent
    then throwError err404
    else do
      payload <- liftIO (loadDeployBackupSnapshot cfg jobId sourceNode userName)
      case payload of
        Nothing -> throwError err404
        Just snapshot ->
          pure
            ( A.object
                [ "jobId" A..= jobId
                , "node" A..= nodeName
                , "sourceNode" A..= sourceNode
                , "user" A..= userName
                , "payload" A..= snapshot
                ]
            )

eventHandler :: AppConfig -> DeployUpdateEmitter -> DeployFailureRecorder -> DeploySuccessRecorder -> Text -> Maybe Text -> NodeEvent -> Handler NoContent
eventHandler cfg emitUpdate recordFailure recordSuccess jobId mAuth event = do
  let projectedEvent = projectNodeEvent event
  let nodeName = projectedEvent.projectedNode
  let statusText = normalizeStatus projectedEvent.projectedStatus
  if nodeName == ""
    then throwError err400
    else if statusText == ""
      then throwError err400
    else do
      requireNodeAuth cfg nodeName mAuth
      hasIntent <- liftIO (deployIntentExists cfg jobId nodeName)
      if not hasIntent
        then throwError err404
        else do
          mJob <- liftIO (loadJobById cfg jobId)
          case mJob of
            Nothing -> throwError err404
            Just job ->
              if not (acceptsNodeEvents (jobSummaryStatus job))
                then throwError err409
                else do
                  storedEvent <- liftIO (appendDeployEvent cfg jobId nodeName statusText projectedEvent.projectedPhase projectedEvent.projectedMessage event.payload)
                  actionUpdateCount <- liftIO (applyDeployActionEvent cfg jobId nodeName statusText projectedEvent.projectedPhase projectedEvent.projectedMessage projectedEvent.projectedActionId)
                  liftIO (logZeroActionProjection jobId nodeName statusText projectedEvent.projectedPhase actionUpdateCount)
                  liftIO
                    ( emitUpdate
                        ( A.object
                            [ "type" A..= ("deploy_event" :: Text)
                            , "jobId" A..= jobId
                            , "event" A..= storedEvent
                            ]
                        )
                    )
                  latestStatuses <- liftIO (loadDeployStatuses cfg jobId)
                  latestActions <- liftIO (loadDeployActions cfg jobId)
                  liftIO
                    ( emitUpdate
                        ( A.object
                            [ "type" A..= ("deploy_status" :: Text)
                            , "jobId" A..= jobId
                            , "statuses" A..= latestStatuses
                            ]
                        )
                    )
                  liftIO
                    ( emitUpdate
                        ( A.object
                            [ "type" A..= ("deploy_actions" :: Text)
                            , "jobId" A..= jobId
                            , "actions" A..= latestActions
                            ]
                        )
                    )
                  if statusText == "failed" || statusText == "timed_out"
                    then liftIO (recordFailure jobId (renderFailureMessage nodeName statusText projectedEvent.projectedMessage))
                    else
                      if statusText /= "success"
                        then pure ()
                        else do
                          intentNodes <- liftIO (loadDeployIntentNodes cfg jobId)
                          let latestByNode = Map.fromList [(nodeVal, (statusVal, phaseVal)) | DeployStatus _ nodeVal statusVal phaseVal _ _ <- latestStatuses]
                              nodeComplete nodeVal = Map.lookup nodeVal latestByNode == Just ("success" :: Text, Just ("intent" :: Text))
                              actionSucceeded :: DeployAction -> Bool
                              actionSucceeded action = action.status == ("success" :: Text)
                              allActionsSucceeded = all actionSucceeded latestActions
                              allSucceeded = not (null intentNodes) && all nodeComplete intentNodes && allActionsSucceeded
                          if allSucceeded
                            then liftIO (recordSuccess jobId)
                            else pure ()
                  pure NoContent

logZeroActionProjection :: Text -> Text -> Text -> Maybe Text -> Int64 -> IO ()
logZeroActionProjection jobId nodeName statusText mPhase actionUpdateCount =
  case fmap (T.toLower . T.strip) mPhase of
    Just "intent" -> pure ()
    Just phaseName ->
      if actionUpdateCount > 0 || statusText == "queued"
        then pure ()
        else putStrLn (T.unpack ("hostenv-provider-service: zero action rows updated for job=" <> jobId <> " node=" <> nodeName <> " phase=" <> phaseName <> " status=" <> statusText))
    _ -> pure ()

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
  TE.decodeUtf8
    ( BL.toStrict
        ( A.encode
            ( A.object
                [ "jobId" A..= jobId
                , "node" A..= nodeName
                , "intent" A..= filteredIntent
                , "actionIds" A..= map deployActionId actions
                ]
            )
        )
    )

shouldDispatchJob :: A.Value -> [DeployAction] -> [DeployAction] -> Maybe Text -> Text -> Bool
shouldDispatchJob validatedIntent nodeActions filteredActions mLastDispatchId dispatchId =
  (not (null filteredActions) || (not (intentHasActions validatedIntent) && not (hasPendingActions nodeActions)))
    && mLastDispatchId /= Just dispatchId

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
  let payloadValue = event.payload
   in ProjectedNodeEvent
        { projectedNode = T.strip event.node
        , projectedStatus =
            fromMaybe
              "running"
              ( firstNonBlankText
                  [ Just event.status
                  , payloadTextAtPath ["status"] payloadValue
                  , payloadTextAtPath ["event", "status"] payloadValue
                  ]
              )
        , projectedPhase =
            firstNonBlankText
              [ event.phase
              , payloadTextAtPath ["phase"] payloadValue
              , payloadTextAtPath ["action", "phase"] payloadValue
              ]
        , projectedMessage =
            firstNonBlankText
              [ event.message
              , payloadTextAtPath ["message"] payloadValue
              , payloadTextAtPath ["event", "message"] payloadValue
              , payloadTextAtPath ["detail"] payloadValue
              ]
        , projectedActionId =
            firstNonBlankText
              [ payloadTextAtPath ["actionId"] payloadValue
              , payloadTextAtPath ["action_id"] payloadValue
              , payloadTextAtPath ["action", "id"] payloadValue
              ]
        , projectedDispatchId =
            firstNonBlankText
              [ payloadTextAtPath ["dispatchId"] payloadValue
              , payloadTextAtPath ["dispatch_id"] payloadValue
              , payloadTextAtPath ["dispatch", "id"] payloadValue
              ]
        }

isValidDeployWsAuth :: AppConfig -> Text -> BL.ByteString -> Bool
isValidDeployWsAuth cfg nodeName rawMessage =
  case A.decode rawMessage of
    Just (A.Object obj) ->
      case (lookupAuthField [["token"], ["auth", "token"]] obj, lookupAuthField [["node"], ["auth", "node"]] obj) of
        (Just suppliedToken, Just suppliedNode)
          | suppliedNode == T.strip nodeName ->
              case lookupDeployNodeAuthToken cfg nodeName of
                Just expected -> expected == suppliedToken
                Nothing -> False
        _ -> False
    _ -> False

lookupAuthField :: [[Text]] -> KM.KeyMap A.Value -> Maybe Text
lookupAuthField paths obj =
  firstNonBlankText [valueTextAtPath path (A.Object obj) | path <- paths]
