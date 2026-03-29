{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.DeployApi
  ( NodeEvent(..)
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
  , extractBearer
  , dispatchForNode
  , normalizeStatus
  , shouldDispatchJob
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (toList)
import Data.Char (isAsciiLower, isDigit)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Servant

import Hostenv.Provider.Config (AppConfig(..), DeployConfig(..))
import Hostenv.Provider.DB (DeployAction(..), DeployStatus(..), appendDeployEvent, applyDeployActionEvent, deployIntentExists, loadDeployActions, loadDeployActionsByNode, loadDeployBackupSnapshot, loadDeployIntentByJob, loadDeployIntentBySha, loadDeployIntentNodes, loadDeployStatusByNode, loadDeployStatuses)
import Hostenv.Provider.Jobs (jobSummaryStatus, loadJobById)


data NodeEvent = NodeEvent
  { node :: Text
  , status :: Text
  , phase :: Maybe Text
  , message :: Maybe Text
  , payload :: Maybe A.Value
  } deriving (Eq, Show)

instance A.FromJSON NodeEvent where
  parseJSON = A.withObject "NodeEvent" $ \o ->
    NodeEvent
      <$> o A..: "node"
      <*> o A..:? "status" A..!= "running"
      <*> o A..:? "phase"
      <*> o A..:? "message"
      <*> o A..:? "payload"

instance A.ToJSON NodeEvent where
  toJSON event =
    A.object
      [ "node" A..= event.node
      , "status" A..= event.status
      , "phase" A..= event.phase
      , "message" A..= event.message
      , "payload" A..= event.payload
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
  let nodeName = T.strip event.node
  let statusText = normalizeStatus event.status
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
                  storedEvent <- liftIO (appendDeployEvent cfg jobId nodeName statusText event.phase event.message event.payload)
                  actionUpdateCount <- liftIO (applyDeployActionEvent cfg jobId nodeName statusText event.phase event.message)
                  liftIO (logZeroActionProjection jobId nodeName statusText event.phase actionUpdateCount)
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
                    then liftIO (recordFailure jobId (renderFailureMessage nodeName statusText event.message))
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
  let tokenMap = cfg.appDeploy.nodeAuthTokens
   in case (Map.lookup nodeName tokenMap, extractBearer mAuth) of
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
