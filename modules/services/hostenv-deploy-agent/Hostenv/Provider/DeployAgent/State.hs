module Hostenv.Provider.DeployAgent.State
  ( ActionJournal(..)
  , ActionJournalStatus(..)
  , AgentState(..)
  , AppliedStorePath(..)
  , CurrentJob(..)
  , emptyState
  , emptyStateFor
  , ensureStateFile
  , finalizeJobState
  , formatTimestamp
  , isCompletedSideEffect
  , isReportedSideEffect
  , isSideEffectAction
  , loadStateFile
  , markActionCompletedLocal
  , markActionReportedFinal
  , markActionStarted
  , markJobStarted
  , markSystemState
  , markUserState
  , resumePayload
  , writeStateFile
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Hostenv.Provider.DeployAgent.Protocol (ActionOp(..), DeployAction(..), DeployJob(..), actionStorePath, renderActionOp)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, openTempFile)

data AppliedStorePath = AppliedStorePath
  { storePath :: Text
  , updatedAt :: Text
  }
  deriving (Eq, Show, Generic)

data CurrentJob = CurrentJob
  { jobId :: Text
  , dispatchId :: Text
  , startedAt :: Text
  }
  deriving (Eq, Show, Generic)

data ActionJournalStatus
  = ActionStarted
  | ActionCompletedLocal
  deriving (Eq, Show, Generic)

data ActionJournal = ActionJournal
  { status :: ActionJournalStatus
  , startedAt :: Text
  , completedAt :: Maybe Text
  , reportedFinal :: Bool
  , sideEffect :: Text
  , resultPayload :: Maybe A.Value
  }
  deriving (Eq, Show, Generic)

data AgentState = AgentState
  { journalVersion :: Int
  , node :: Text
  , current :: Maybe CurrentJob
  , actions :: Map Text ActionJournal
  , system :: Maybe AppliedStorePath
  , users :: Map Text AppliedStorePath
  , updatedAt :: Text
  }
  deriving (Eq, Show, Generic)

instance A.ToJSON AppliedStorePath where
  toJSON value =
    A.object
      [ "storePath" A..= value.storePath
      , "updatedAt" A..= value.updatedAt
      ]

instance A.FromJSON AppliedStorePath where
  parseJSON = A.withObject "AppliedStorePath" $ \obj ->
    AppliedStorePath
      <$> obj A..: "storePath"
      <*> obj A..:? "updatedAt" A..!= ""

instance A.ToJSON CurrentJob where
  toJSON value =
    A.object
      [ "jobId" A..= value.jobId
      , "dispatchId" A..= value.dispatchId
      , "startedAt" A..= value.startedAt
      ]

instance A.FromJSON CurrentJob where
  parseJSON = A.withObject "CurrentJob" $ \obj ->
    CurrentJob
      <$> obj A..: "jobId"
      <*> obj A..: "dispatchId"
      <*> obj A..: "startedAt"

instance A.ToJSON ActionJournalStatus where
  toJSON = A.String . renderActionJournalStatus

instance A.FromJSON ActionJournalStatus where
  parseJSON = A.withText "ActionJournalStatus" $ \raw ->
    case parseActionJournalStatus raw of
      Nothing -> fail ("unsupported action journal status: " <> T.unpack raw)
      Just value -> pure value

instance A.ToJSON ActionJournal where
  toJSON value =
    A.object
      [ "status" A..= value.status
      , "startedAt" A..= value.startedAt
      , "completedAt" A..= value.completedAt
      , "reportedFinal" A..= value.reportedFinal
      , "sideEffect" A..= value.sideEffect
      , "resultPayload" A..= value.resultPayload
      ]

instance A.FromJSON ActionJournal where
  parseJSON = A.withObject "ActionJournal" $ \obj ->
    ActionJournal
      <$> obj A..: "status"
      <*> obj A..: "startedAt"
      <*> obj A..:? "completedAt"
      <*> obj A..:? "reportedFinal" A..!= False
      <*> obj A..: "sideEffect"
      <*> obj A..:? "resultPayload"

instance A.ToJSON AgentState where
  toJSON state =
    A.object
      [ "journalVersion" A..= state.journalVersion
      , "node" A..= state.node
      , "current" A..= state.current
      , "actions" A..= state.actions
      , "system" A..= state.system
      , "users" A..= state.users
      , "updatedAt" A..= state.updatedAt
      ]

instance A.FromJSON AgentState where
  parseJSON = A.withObject "AgentState" $ \obj -> do
    version <- obj A..: "journalVersion"
    if version /= 1
      then fail ("unsupported journal version: " <> show version)
      else
        AgentState
          <$> pure version
          <*> obj A..: "node"
          <*> obj A..:? "current"
          <*> obj A..:? "actions" A..!= Map.empty
          <*> obj A..:? "system"
          <*> obj A..:? "users" A..!= Map.empty
          <*> obj A..:? "updatedAt" A..!= ""

emptyState :: AgentState
emptyState = emptyStateFor ""

emptyStateFor :: Text -> AgentState
emptyStateFor nodeName =
  AgentState
    { journalVersion = 1
    , node = nodeName
    , current = Nothing
    , actions = Map.empty
    , system = Nothing
    , users = Map.empty
    , updatedAt = ""
    }

ensureStateFile :: Text -> FilePath -> IO ()
ensureStateFile nodeName path = do
  createDirectoryIfMissing True (takeDirectory path)
  exists <- doesFileExist path
  if not exists
    then writeStateFile path (emptyStateFor nodeName)
    else do
      bytes <- BL.readFile path
      if BL.null bytes then writeStateFile path (emptyStateFor nodeName) else pure ()

loadStateFile :: Text -> FilePath -> IO AgentState
loadStateFile nodeName path = do
  ensureStateFile nodeName path
  bytes <- BL.readFile path
  case A.eitherDecode bytes of
    Left err -> fail ("failed to decode deploy-agent journal " <> path <> ": " <> err)
    Right state
      | nodeName /= "" && state.node /= "" && state.node /= nodeName ->
          fail ("deploy-agent journal node mismatch for " <> path <> ": expected " <> T.unpack nodeName <> ", got " <> T.unpack state.node)
      | otherwise -> pure state

writeStateFile :: FilePath -> AgentState -> IO ()
writeStateFile path state = do
  let parentDir = takeDirectory path
  createDirectoryIfMissing True parentDir
  (tempPath, handle) <- openTempFile parentDir "state.json.tmp"
  hClose handle
  BL.writeFile tempPath (A.encode state)
  renameFile tempPath path

markJobStarted :: Text -> DeployJob -> AgentState -> AgentState
markJobStarted timestamp job state =
  state
    { current = Just nextCurrent
    , actions = nextActions
    , updatedAt = timestamp
    }
  where
    nextCurrent =
      case state.current of
        Just existing | existing.dispatchId == job.dispatchId -> existing
        _ ->
          CurrentJob
            { jobId = job.jobId
            , dispatchId = job.dispatchId
            , startedAt = timestamp
            }
    nextActions =
      case state.current of
        Just existing | existing.dispatchId == job.dispatchId -> state.actions
        _ -> Map.empty

markSystemState :: Text -> Text -> AgentState -> AgentState
markSystemState timestamp storePathValue state =
  state
    { system = Just AppliedStorePath { storePath = storePathValue, updatedAt = timestamp }
    , updatedAt = timestamp
    }

markUserState :: Text -> Text -> Text -> AgentState -> AgentState
markUserState timestamp userName storePathValue state =
  state
    { users = Map.insert userName AppliedStorePath { storePath = storePathValue, updatedAt = timestamp } state.users
    , updatedAt = timestamp
    }

markActionStarted :: Text -> DeployJob -> DeployAction -> AgentState -> AgentState
markActionStarted timestamp job action state
  | not (isSideEffectAction action.op) = state
  | otherwise =
      let baseState = markJobStarted timestamp job state
          startedAtValue =
            case Map.lookup action.actionId baseState.actions of
              Just existing -> existing.startedAt
              Nothing -> timestamp
       in baseState
            { actions =
                Map.insert
                  action.actionId
                  ActionJournal
                    { status = ActionStarted
                    , startedAt = startedAtValue
                    , completedAt = Nothing
                    , reportedFinal = False
                    , sideEffect = renderActionOp action.op
                    , resultPayload = Nothing
                    }
                  baseState.actions
            , updatedAt = timestamp
            }

markActionCompletedLocal :: Text -> DeployAction -> A.Value -> AgentState -> AgentState
markActionCompletedLocal timestamp action payload state
  | not (isSideEffectAction action.op) = state
  | otherwise =
      stateWithUser
        { actions =
            Map.insert
              action.actionId
              ActionJournal
                { status = ActionCompletedLocal
                , startedAt = startedAtValue
                , completedAt = Just timestamp
                , reportedFinal = False
                , sideEffect = renderActionOp action.op
                , resultPayload = Just payload
                }
              stateWithUser.actions
        , updatedAt = timestamp
        }
  where
    stateWithUser =
      if shouldPersistUserPath action
        then markUserState timestamp action.user (actionStorePath action) state
        else state
    startedAtValue =
      case Map.lookup action.actionId state.actions of
        Just existing -> existing.startedAt
        Nothing -> timestamp

markActionReportedFinal :: Text -> Text -> AgentState -> AgentState
markActionReportedFinal timestamp actionIdValue state =
  state
    { actions = Map.adjust markReported actionIdValue state.actions
    , updatedAt = timestamp
    }
  where
    markReported entry = entry { reportedFinal = True }

finalizeJobState :: Text -> AgentState -> AgentState
finalizeJobState timestamp state =
  state
    { current = Nothing
    , actions = Map.empty
    , updatedAt = timestamp
    }

resumePayload :: AgentState -> A.Value
resumePayload state =
  A.object
    [ "journalVersion" A..= state.journalVersion
    , "current" A..= state.current
    , "actions" A..= state.actions
    , "updatedAt" A..= state.updatedAt
    ]

isCompletedSideEffect :: DeployJob -> DeployAction -> AgentState -> Bool
isCompletedSideEffect job action state =
  isMatchingCurrent job state && maybe False ((== ActionCompletedLocal) . (.status)) (Map.lookup action.actionId state.actions)

isReportedSideEffect :: DeployJob -> DeployAction -> AgentState -> Bool
isReportedSideEffect job action state =
  isMatchingCurrent job state && maybe False (.reportedFinal) (Map.lookup action.actionId state.actions)

isSideEffectAction :: ActionOp -> Bool
isSideEffectAction op = op `elem` [Backup, Restore, Deactivate]

formatTimestamp :: UTCTime -> Text
formatTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

parseActionJournalStatus :: Text -> Maybe ActionJournalStatus
parseActionJournalStatus raw =
  case T.toLower (T.strip raw) of
    "started" -> Just ActionStarted
    "completed_local" -> Just ActionCompletedLocal
    _ -> Nothing

renderActionJournalStatus :: ActionJournalStatus -> Text
renderActionJournalStatus = \case
  ActionStarted -> "started"
  ActionCompletedLocal -> "completed_local"

isMatchingCurrent :: DeployJob -> AgentState -> Bool
isMatchingCurrent job state =
  maybe False (\currentJob -> currentJob.dispatchId == job.dispatchId && currentJob.jobId == job.jobId) state.current

shouldPersistUserPath :: DeployAction -> Bool
shouldPersistUserPath action =
  actionStorePath action /= ""
