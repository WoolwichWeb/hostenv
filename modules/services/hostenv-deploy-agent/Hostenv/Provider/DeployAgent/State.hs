module Hostenv.Provider.DeployAgent.State
  ( AgentState(..)
  , AppliedStorePath(..)
  , emptyState
  , ensureStateFile
  , finalizeJobState
  , formatTimestamp
  , loadStateFile
  , markSystemState
  , markUserState
  , writeStateFile
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Hostenv.Provider.DeployAgent.Protocol (ActionOp(..), DeployAction(..), DeployIntent(..), DeployJob(..), actionStorePath)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, openTempFile)

data AppliedStorePath = AppliedStorePath
  { storePath :: Text
  , updatedAt :: Text
  }
  deriving (Eq, Show, Generic)

data AgentState = AgentState
  { lastAppliedJobId :: Text
  , lastAppliedSignature :: Text
  , lastCommitSha :: Text
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

instance A.ToJSON AgentState where
  toJSON state =
    A.object
      [ "lastAppliedJobId" A..= state.lastAppliedJobId
      , "lastAppliedSignature" A..= state.lastAppliedSignature
      , "lastCommitSha" A..= state.lastCommitSha
      , "system" A..= maybe (A.object []) A.toJSON state.system
      , "users" A..= state.users
      , "updatedAt" A..= state.updatedAt
      ]

instance A.FromJSON AgentState where
  parseJSON = A.withObject "AgentState" $ \obj -> do
    rawSystem <- obj A..:? "system"
    AgentState
      <$> obj A..:? "lastAppliedJobId" A..!= ""
      <*> obj A..:? "lastAppliedSignature" A..!= ""
      <*> obj A..:? "lastCommitSha" A..!= ""
      <*> pure (parseSystem rawSystem)
      <*> obj A..:? "users" A..!= Map.empty
      <*> obj A..:? "updatedAt" A..!= ""

emptyState :: AgentState
emptyState =
  AgentState
    { lastAppliedJobId = ""
    , lastAppliedSignature = ""
    , lastCommitSha = ""
    , system = Nothing
    , users = Map.empty
    , updatedAt = ""
    }

ensureStateFile :: FilePath -> IO ()
ensureStateFile path = do
  createDirectoryIfMissing True (takeDirectory path)
  exists <- doesFileExist path
  if not exists
    then writeStateFile path emptyState
    else do
      bytes <- BL.readFile path
      if BL.null bytes then writeStateFile path emptyState else pure ()

loadStateFile :: FilePath -> IO AgentState
loadStateFile path = do
  ensureStateFile path
  bytes <- BL.readFile path
  pure (fromMaybe emptyState (A.decode bytes))

writeStateFile :: FilePath -> AgentState -> IO ()
writeStateFile path state = do
  let parentDir = takeDirectory path
  createDirectoryIfMissing True parentDir
  (tempPath, handle) <- openTempFile parentDir "state.json.tmp"
  hClose handle
  BL.writeFile tempPath (A.encode state)
  renameFile tempPath path

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

finalizeJobState :: Text -> DeployJob -> Text -> AgentState -> AgentState
finalizeJobState timestamp job signature state =
  state
    { lastAppliedJobId = job.jobId
    , lastAppliedSignature = signature
    , lastCommitSha = job.commitSha
    , users = appliedUsers `Map.union` state.users
    , updatedAt = timestamp
    }
  where
    DeployJob { intent = DeployIntent { actions = jobActions } } = job
    appliedUsers =
      Map.fromList
        [ (userName, AppliedStorePath { storePath = storePathValue, updatedAt = timestamp })
        | action <- jobActions
        , let DeployAction { user = userName, op = actionOp } = action
        , actionOp `elem` [Activate, Reload, Restore, Backup]
        , let storePathValue = actionStorePath action
        , storePathValue /= ""
        ]

formatTimestamp :: UTCTime -> Text
formatTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

parseSystem :: Maybe A.Value -> Maybe AppliedStorePath
parseSystem = \case
  Nothing -> Nothing
  Just A.Null -> Nothing
  Just (A.Object obj)
    | KM.null obj -> Nothing
  Just value ->
      case A.fromJSON value of
        A.Success parsed -> Just parsed
        A.Error _ -> Nothing
