module Hostenv.Provider.DeployAgent.Executor
  ( ExecutionDeps(..)
  , JobResult(..)
  , SnapshotRequest(..)
  , executeJob
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Hostenv.Provider.DeployAgent.Config (AgentConfig(..))
import Hostenv.Provider.DeployAgent.Dispatcher (DispatchDecision(..), decideDispatch)
import Hostenv.Provider.DeployAgent.Executor.Classify
  ( ActionStep(..)
  , FailureClassification(..)
  , SystemStep(..)
  , buildCommandPayload
  , classifyActionFailure
  , classifySystemFailure
  , actionStepLabel
  )
import Hostenv.Provider.DeployAgent.Executor.Process
  ( ProcessResult(..)
  , ProcessRunner
  , ProcessSpec(..)
  , trimStderrSummary
  )
import Hostenv.Provider.DeployAgent.Logging (Logger, logDebug)
import Hostenv.Provider.DeployAgent.Protocol
  ( ActionOp(..)
  , DeployAction(..)
  , DeployIntent(..)
  , DeployJob(..)
  , EventStatus(..)
  , NodeEvent(..)
  , actionStorePath
  , intentSystemPath
  , renderActionOp
  )
import Hostenv.Provider.DeployAgent.State
  ( AgentState(..)
  , finalizeJobState
  , formatTimestamp
  , markSystemState
  , markUserState
  )
import Hostenv.Provider.DeployAgent.Systemd
  ( defaultWantedUnitsDir
  , normalizeWantedUnits
  , verifyUnitsSpec
  )
import Hostenv.Provider.DeployAgent.UserSession
  ( UserIdentity(..)
  , UserSession(..)
  , buildUserSession
  , isSafeUserValue
  , runUserSpec
  )
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

data SnapshotRequest = SnapshotRequest
  { jobId :: Text
  , sourceNode :: Text
  , user :: Text
  }
  deriving (Eq, Show)

data ExecutionDeps = ExecutionDeps
  { logger :: Logger
  , emitEvent :: Text -> NodeEvent -> IO ()
  , fetchSnapshot :: SnapshotRequest -> IO (Maybe A.Value)
  , resolveUser :: Text -> IO (Maybe UserIdentity)
  , ensureUserSession :: UserSession -> IO (Maybe ProcessResult)
  , readState :: IO AgentState
  , writeState :: AgentState -> IO ()
  , processRunner :: ProcessRunner IO
  , pathExists :: FilePath -> IO Bool
  , listDirectory :: FilePath -> IO [FilePath]
  , currentTime :: IO UTCTime
  }

data JobResult
  = JobSucceeded AgentState
  | JobFailed FailureClassification
  | JobSkipped Text
  deriving (Eq, Show)

executeJob :: ExecutionDeps -> AgentConfig -> DeployJob -> IO JobResult
executeJob deps cfg job = do
  state0 <- deps.readState
  let DeployJob { intent = DeployIntent { actions = jobActions } } = job
  case decideDispatch state0 job of
    DispatchSkipDuplicate -> pure (JobSkipped "duplicate signature")
    DispatchHandle signature _ -> do
      now <- deps.currentTime
      let timestamp = formatTimestamp now
      if intentSystemPath job.intent == "" && null jobActions
        then do
          let state1 = state0 { lastAppliedSignature = signature, updatedAt = timestamp }
          deps.writeState state1
          pure (JobSucceeded state1)
        else do
          postEvent deps cfg job EventRunning "intent" "Deploy job started" (A.object [])
          systemResult <- runSystemPhase deps cfg job timestamp state0
          case systemResult of
            Left failure -> pure (JobFailed failure)
            Right stateAfterSystem -> do
              actionResult <- foldM (runActionPhase deps cfg job timestamp) (Right stateAfterSystem) jobActions
              case actionResult of
                Left failure -> pure (JobFailed failure)
                Right stateAfterActions -> do
                  postEvent deps cfg job EventSuccess "intent" "Deploy job complete" (A.object [])
                  let finalState = finalizeJobState timestamp job signature stateAfterActions
                  deps.writeState finalState
                  pure (JobSucceeded finalState)

runSystemPhase :: ExecutionDeps -> AgentConfig -> DeployJob -> Text -> AgentState -> IO (Either FailureClassification AgentState)
runSystemPhase deps cfg job timestamp state0 = do
  let systemPathValue = T.unpack (intentSystemPath job.intent)
  if null systemPathValue
    then pure (Right state0)
    else do
      postEvent deps cfg job EventRunning "system" "Switching system profile" (A.object [])
      let realiseSpec =
            ProcessSpec
              { description = "system-realise"
              , executable = "nix-store"
              , arguments = ["--realise", systemPathValue]
              , environment = []
              , workingDirectory = Nothing
              , timeoutSeconds = Nothing
              }
          setSpec =
            ProcessSpec
              { description = "system-set"
              , executable = "nix-env"
              , arguments = ["-p", "/nix/var/nix/profiles/system", "--set", systemPathValue]
              , environment = []
              , workingDirectory = Nothing
              , timeoutSeconds = Nothing
              }
          switchSpec =
            ProcessSpec
              { description = "system-switch"
              , executable = systemPathValue </> "bin" </> "switch-to-configuration"
              , arguments = ["switch"]
              , environment = []
              , workingDirectory = Nothing
              , timeoutSeconds = Nothing
              }
      runAndHandleSystemStep deps cfg job SystemRealise realiseSpec >>= \case
        Left failure -> pure (Left failure)
        Right _ ->
          runAndHandleSystemStep deps cfg job SystemSet setSpec >>= \case
            Left failure -> pure (Left failure)
            Right _ ->
              runAndHandleSystemStep deps cfg job SystemSwitch switchSpec >>= \case
                Left failure -> pure (Left failure)
                Right result -> do
                  let state1 = markSystemState timestamp (T.pack systemPathValue) state0
                      payload = buildCommandPayload 0 (trimStderrSummary cfg.eventStderrMaxLines result.stderrText) (A.object ["step" A..= ("system-switch" :: Text)])
                  deps.writeState state1
                  postEvent deps cfg job EventSuccess "system" "System switch complete" payload
                  pure (Right state1)

runActionPhase :: ExecutionDeps -> AgentConfig -> DeployJob -> Text -> Either FailureClassification AgentState -> DeployAction -> IO (Either FailureClassification AgentState)
runActionPhase deps cfg job timestamp prior action =
  case prior of
    Left failure -> pure (Left failure)
    Right state0 -> do
      let opName = renderActionOp action.op
      postEvent deps cfg job EventRunning opName ("Running action " <> opName <> " for " <> action.user) (A.object [])
      mIdentity <- deps.resolveUser action.user
      case validateActionContext action mIdentity of
        Just failure -> do
          postFailure deps cfg job failure emptyProcessResult
          pure (Left failure)
        Nothing -> do
          let session = buildUserSession (maybe (error "resolved user missing") id mIdentity)
          prepared <- prepareAction deps session action
          case prepared of
            Left (failure, result) -> do
              postFailure deps cfg job failure result
              pure (Left failure)
            Right executablePath -> do
              ran <- runActionCommand deps cfg job session action executablePath
              case ran of
                Left (failure, result) -> do
                  postFailure deps cfg job failure result
                  pure (Left failure)
                Right result -> do
                  verified <- verifyUnitsIfNeeded deps cfg session action
                  case verified of
                    Left (failure, verifyResult) -> do
                      postFailure deps cfg job failure verifyResult
                      pure (Left failure)
                    Right () -> do
                      let state1 =
                            if shouldMarkUserState action.op && actionStorePath action /= ""
                              then markUserState timestamp action.user (actionStorePath action) state0
                              else state0
                          payload =
                            buildCommandPayload
                              result.exitCode
                              (trimStderrSummary cfg.eventStderrMaxLines result.stderrText)
                              (A.object ["user" A..= action.user, "op" A..= opName])
                      deps.writeState state1
                      postEvent deps cfg job EventSuccess opName "Action complete" payload
                      pure (Right state1)

runAndHandleSystemStep :: ExecutionDeps -> AgentConfig -> DeployJob -> SystemStep -> ProcessSpec -> IO (Either FailureClassification ProcessResult)
runAndHandleSystemStep deps cfg job step spec = do
  result <- runSpec deps spec
  if result.exitCode == 0
    then pure (Right result)
    else do
      let failure = classifySystemFailure step result.exitCode
      postFailure deps cfg job failure result
      pure (Left failure)

prepareAction :: ExecutionDeps -> UserSession -> DeployAction -> IO (Either (FailureClassification, ProcessResult) FilePath)
prepareAction deps session action = do
  let storePathValue = T.unpack (actionStorePath action)
      UserSession { profile = profilePath } = session
  if null storePathValue
    then resolveExecutablePath deps session action
    else do
      let realiseSpec =
            ProcessSpec
              { description = actionStepLabel (RealiseAction action.op action.user)
              , executable = "nix-store"
              , arguments = ["--realise", storePathValue]
              , environment = []
              , workingDirectory = Nothing
              , timeoutSeconds = Nothing
              }
      realiseResult <- runSpec deps realiseSpec
      if realiseResult.exitCode /= 0
        then pure (Left (classifyActionFailure action.op (RealiseAction action.op action.user) realiseResult.exitCode, realiseResult))
        else do
          profileDirReady <- ensureProfileDirectory deps session
          case profileDirReady of
            Just failure -> pure (Left (classifyActionFailure action.op (SetProfile action.user) failure.exitCode, failure))
            Nothing -> do
              setProfileResult <-
                runUserCommand deps session
                  ProcessSpec
                    { description = actionStepLabel (SetProfile action.user)
                    , executable = "nix-env"
                    , arguments = ["-p", profilePath, "--set", storePathValue]
                    , environment = []
                    , workingDirectory = Nothing
                    , timeoutSeconds = Nothing
                    }
              if setProfileResult.exitCode /= 0
                then pure (Left (classifyActionFailure action.op (SetProfile action.user) setProfileResult.exitCode, setProfileResult))
                else resolveExecutablePath deps session action

resolveExecutablePath :: ExecutionDeps -> UserSession -> DeployAction -> IO (Either (FailureClassification, ProcessResult) FilePath)
resolveExecutablePath deps session action = do
  let UserSession { profile = profilePath } = session
      opName = T.unpack (renderActionOp action.op)
      candidates =
        filter (not . null)
          [ profilePath </> "bin" </> opName
          , let storePathValue = T.unpack (actionStorePath action)
             in if null storePathValue then "" else storePathValue </> "bin" </> opName
          ]
  firstExisting deps candidates >>= \case
    Nothing -> pure (Left (classifyActionFailure action.op (ResolveExecutable action.op action.user) 3, emptyProcessResult))
    Just executablePath -> pure (Right executablePath)

runActionCommand :: ExecutionDeps -> AgentConfig -> DeployJob -> UserSession -> DeployAction -> FilePath -> IO (Either (FailureClassification, ProcessResult) ProcessResult)
runActionCommand deps cfg job session action executablePath =
  case (action.op, action.fromNode) of
    (Restore, Just sourceNode) -> do
      snapshot <- deps.fetchSnapshot SnapshotRequest { jobId = job.jobId, sourceNode, user = action.user }
      case snapshot of
        Nothing -> pure (Left (classifyActionFailure action.op (FetchSnapshot action.op action.user) 4, emptyProcessResult))
        Just payload ->
          withSnapshotFile payload \snapshotPath -> do
            let spec = actionCommandSpec session action executablePath cfg.actionTimeoutSeconds [("HOSTENV_RESTORE_SNAPSHOT_FILE", snapshotPath), ("HOSTENV_MIGRATIONS", migrationsCsv action)]
            runActionSpec deps session action spec
    _ -> runActionSpec deps session action (actionCommandSpec session action executablePath cfg.actionTimeoutSeconds [])

runActionSpec :: ExecutionDeps -> UserSession -> DeployAction -> ProcessSpec -> IO (Either (FailureClassification, ProcessResult) ProcessResult)
runActionSpec deps session action spec = do
  result <- runUserCommand deps session spec
  if result.exitCode == 0
    then pure (Right result)
    else pure (Left (classifyActionFailure action.op (RunAction action.op action.user) result.exitCode, result))

verifyUnitsIfNeeded :: ExecutionDeps -> AgentConfig -> UserSession -> DeployAction -> IO (Either (FailureClassification, ProcessResult) ())
verifyUnitsIfNeeded deps cfg session action = do
  let storePathValue = T.unpack (actionStorePath action)
  if null storePathValue || not (shouldVerifyUnits action.op)
    then pure (Right ())
    else do
      let systemdUserDir = storePathValue </> "systemd" </> "user"
      hasSystemdUserDir <- deps.pathExists systemdUserDir
      if not hasSystemdUserDir
        then pure (Right ())
        else do
          let wantsDir = defaultWantedUnitsDir storePathValue
          wantsDirExists <- deps.pathExists wantsDir
          units <- if wantsDirExists then normalizeWantedUnits <$> deps.listDirectory wantsDir else pure []
          let baseSpec = verifyUnitsSpec session units
              verifySpec = baseSpec { timeoutSeconds = Just cfg.actionTimeoutSeconds }
          verifyResult <- runUserCommand deps session verifySpec
          if verifyResult.exitCode == 0
            then pure (Right ())
            else pure (Left (classifyActionFailure action.op (VerifyAction action.op action.user) verifyResult.exitCode, verifyResult))

validateActionContext :: DeployAction -> Maybe UserIdentity -> Maybe FailureClassification
validateActionContext action mIdentity
  | not (isSafeUserValue action.user) = invalid
  | maybe False (not . isSafeUserValue) action.fromNode = invalid
  | maybe False (not . isSafeUserValue) action.toNode = invalid
  | maybe False (const False) mIdentity = Nothing
  | otherwise =
      case mIdentity of
        Nothing -> invalid
        Just _ -> Nothing
  where
    invalid = Just (classifyActionFailure action.op (ValidateAction action.op action.user) 3)

actionCommandSpec :: UserSession -> DeployAction -> FilePath -> Int -> [(String, String)] -> ProcessSpec
actionCommandSpec _ action executablePath timeoutSeconds extraEnv =
  ProcessSpec
    { description = actionStepLabel (RunAction action.op action.user)
    , executable = executablePath
    , arguments = []
    , environment = extraEnv
    , workingDirectory = Nothing
    , timeoutSeconds = Just timeoutSeconds
    }

postEvent :: ExecutionDeps -> AgentConfig -> DeployJob -> EventStatus -> Text -> Text -> A.Value -> IO ()
postEvent deps cfg job status phase message payload =
  deps.emitEvent
    job.jobId
    NodeEvent
      { node = cfg.nodeName
      , status
      , phase = Just phase
      , message = Just message
      , payload
      }

postFailure :: ExecutionDeps -> AgentConfig -> DeployJob -> FailureClassification -> ProcessResult -> IO ()
postFailure deps cfg job failure result =
  postEvent deps cfg job failure.status failure.phase failure.message payload
  where
    payload = buildCommandPayload result.exitCode (trimStderrSummary cfg.eventStderrMaxLines result.stderrText) failure.payload

runSpec :: ExecutionDeps -> ProcessSpec -> IO ProcessResult
runSpec deps spec = do
  logDebug deps.logger "executor.process" "running command" ["description" A..= spec.description, "executable" A..= spec.executable, "arguments" A..= spec.arguments]
  deps.processRunner spec

runUserCommand :: ExecutionDeps -> UserSession -> ProcessSpec -> IO ProcessResult
runUserCommand deps session spec = do
  ensured <- deps.ensureUserSession session
  case ensured of
    Just failure -> pure failure
    Nothing -> runSpec deps (runUserSpec session spec)

ensureProfileDirectory :: ExecutionDeps -> UserSession -> IO (Maybe ProcessResult)
ensureProfileDirectory deps session = do
  let UserSession { identity = UserIdentity { name = identityName }, profileDir = profileDirValue } = session
      mkdirSpec =
        ProcessSpec
          { description = "profile-dir-mkdir"
          , executable = "mkdir"
          , arguments = ["-p", profileDirValue]
          , environment = []
          , workingDirectory = Nothing
          , timeoutSeconds = Nothing
          }
      chownSpec =
        ProcessSpec
          { description = "profile-dir-chown"
          , executable = "chown"
          , arguments = [T.unpack identityName, profileDirValue]
          , environment = []
          , workingDirectory = Nothing
          , timeoutSeconds = Nothing
          }
  mkdirResult <- runSpec deps mkdirSpec
  if mkdirResult.exitCode /= 0
    then pure (Just mkdirResult)
    else do
      chownResult <- runSpec deps chownSpec
      if chownResult.exitCode /= 0 then pure (Just chownResult) else pure Nothing

firstExisting :: ExecutionDeps -> [FilePath] -> IO (Maybe FilePath)
firstExisting deps = go
  where
    go [] = pure Nothing
    go (candidate : rest) = do
      exists <- deps.pathExists candidate
      if exists then pure (Just candidate) else go rest

withSnapshotFile :: A.Value -> (FilePath -> IO a) -> IO a
withSnapshotFile payload action = do
  tempDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tempDir "hostenv-restore-snapshot.json"
  hClose handle
  BL.writeFile path (A.encode payload)
  result <- action path
  removeFile path
  pure result

migrationsCsv :: DeployAction -> String
migrationsCsv action = T.unpack (T.intercalate "," action.migrations)

shouldVerifyUnits :: ActionOp -> Bool
shouldVerifyUnits op = op `elem` [Activate, Reload, Restore, Backup]

shouldMarkUserState :: ActionOp -> Bool
shouldMarkUserState op = op `elem` [Activate, Reload, Restore, Backup]

emptyProcessResult :: ProcessResult
emptyProcessResult =
  ProcessResult
    { exitCode = 0
    , stdoutText = ""
    , stderrText = ""
    }
