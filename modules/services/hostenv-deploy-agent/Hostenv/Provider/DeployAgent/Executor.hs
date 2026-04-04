module Hostenv.Provider.DeployAgent.Executor
  ( ExecutionDeps(..)
  , JobResult(..)
  , SnapshotRequest(..)
  , executeJob
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, diffUTCTime)
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
import Hostenv.Provider.DeployAgent.Logging (Logger, logDebug, logInfo, logWarn)
import Hostenv.Provider.DeployAgent.ProviderApi
  ( SnapshotDocument(..)
  , SnapshotFetchFailure(..)
  , SnapshotMetadata(..)
  , SnapshotRequest(..)
  , buildSnapshotMetadata
  )
import Hostenv.Provider.DeployAgent.Protocol
  ( ActionOp(..)
  , DeployAction(..)
  , DeployIntent(..)
  , DeployJob(..)
  , EventStatus(..)
    , NodeEvent(..)
    , actionStorePath
    , eventStatusText
    , intentSystemPath
    , renderActionOp
    )
import Hostenv.Provider.DeployAgent.State
  ( ActionJournal(..)
    , AgentState(..)
    , finalizeJobState
   , formatTimestamp
   , isCompletedSideEffect
   , isReportedSideEffect
   , isSideEffectAction
   , markActionCompletedLocal
   , markActionReportedFinal
   , markActionStarted
   , markJobStarted
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

data ExecutionDeps = ExecutionDeps
  { logger :: Logger
  , emitEvent :: Text -> NodeEvent -> IO ()
  , fetchSnapshot :: SnapshotRequest -> IO (Either SnapshotFetchFailure SnapshotDocument)
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
  now <- deps.currentTime
  let timestamp = formatTimestamp now
      startedState = markJobStarted timestamp job state0
  deps.writeState startedState
  let DeployJob { intent = DeployIntent { actions = jobActions } } = job
  case decideDispatch startedState job of
    DispatchHandle _ ->
      if intentSystemPath job.intent == "" && null jobActions
        then do
          let finalState = finalizeJobState timestamp startedState
          deps.writeState finalState
          pure (JobSucceeded finalState)
        else do
          postEvent deps cfg job Nothing EventRunning "intent" "Deploy job started" (A.object [])
          systemResult <- runSystemPhase deps cfg job timestamp startedState
          case systemResult of
            Left failure -> pure (JobFailed failure) -- @todo: caller logs but does not report failure to back-end; fix this.
            Right stateAfterSystem -> do
              actionResult <- foldM (runActionPhase deps cfg job timestamp) (Right stateAfterSystem) jobActions
              case actionResult of
                Left failure -> pure (JobFailed failure)
                Right stateAfterActions -> do
                  postEvent deps cfg job Nothing EventSuccess "intent" "Deploy job complete" (A.object [])
                  let finalState = finalizeJobState timestamp stateAfterActions
                  deps.writeState finalState
                  pure (JobSucceeded finalState)

runSystemPhase :: ExecutionDeps -> AgentConfig -> DeployJob -> Text -> AgentState -> IO (Either FailureClassification AgentState)
runSystemPhase deps cfg job timestamp state0 = do
  let systemPathValue = T.unpack (intentSystemPath job.intent)
  if null systemPathValue
    then pure (Right state0)
    else do
      postEvent deps cfg job Nothing EventRunning "system" "Switching system profile" (A.object [])
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
                  postEvent deps cfg job Nothing EventSuccess "system" "System switch complete" payload
                  pure (Right state1)

runActionPhase :: ExecutionDeps -> AgentConfig -> DeployJob -> Text -> Either FailureClassification AgentState -> DeployAction -> IO (Either FailureClassification AgentState)
runActionPhase deps cfg job timestamp prior action =
  case prior of
    Left failure -> pure (Left failure)
    Right state0 -> do
      case sideEffectDecision job action state0 of
        ResumeSkipCompleted
          | isReportedSideEffect job action state0 -> pure (Right state0)
          | otherwise -> do
              let opName = renderActionOp action.op
                  payload = resumedActionPayload action state0
              postEvent deps cfg job (Just action.actionId) EventSuccess opName "Action complete" payload
              let state1 = markActionReportedFinal timestamp action.actionId state0
              deps.writeState state1
              pure (Right state1)
        ResumeRunAction -> do
          let opName = renderActionOp action.op
          postEvent deps cfg job (Just action.actionId) EventRunning opName ("Running action " <> opName <> " for " <> action.user) (A.object [])
          mIdentity <- deps.resolveUser action.user
          case validateActionContext action mIdentity of
            Just failure -> do
              postFailure deps cfg job (Just action) failure emptyProcessResult
              pure (Left failure)
            Nothing -> do
              let session = buildUserSession (maybe (error "resolved user missing") id mIdentity)
              prepared <- prepareAction deps session action
              case prepared of
                Left (failure, result) -> do
                  postFailure deps cfg job (Just action) failure result
                  pure (Left failure)
                Right executablePath -> do
                  let state1 = if isSideEffectAction action.op then markActionStarted timestamp job action state0 else state0
                  deps.writeState state1
                  ran <- runActionCommand deps cfg job session action executablePath
                  case ran of
                    Left (failure, result) -> do
                      postFailure deps cfg job (Just action) failure result
                      pure (Left failure)
                    Right result -> do
                      verified <- verifyUnitsIfNeeded deps cfg session action
                      case verified of
                        Left (failure, verifyResult) -> do
                          postFailure deps cfg job (Just action) failure verifyResult
                          pure (Left failure)
                        Right () -> do
                          case buildActionSuccessPayload cfg action result of
                            Left failure -> do
                              postFailure deps cfg job (Just action) failure result
                              pure (Left failure)
                            Right payload -> do
                              let stateBeforeReport =
                                    if isSideEffectAction action.op
                                      then markActionCompletedLocal timestamp action payload state1
                                      else
                                        if shouldMarkUserState action.op && actionStorePath action /= ""
                                          then markUserState timestamp action.user (actionStorePath action) state1
                                          else state1
                              deps.writeState stateBeforeReport
                              postEvent deps cfg job (Just action.actionId) EventSuccess opName "Action complete" payload
                              let stateAfterReport =
                                    if isSideEffectAction action.op
                                      then markActionReportedFinal timestamp action.actionId stateBeforeReport
                                      else stateBeforeReport
                              deps.writeState stateAfterReport
                              pure (Right stateAfterReport)

runAndHandleSystemStep :: ExecutionDeps -> AgentConfig -> DeployJob -> SystemStep -> ProcessSpec -> IO (Either FailureClassification ProcessResult)
runAndHandleSystemStep deps cfg job step spec = do
      result <- runSpec deps spec
      if result.exitCode == 0
        then pure (Right result)
        else do
          let failure = classifySystemFailure step result.exitCode
          postFailure deps cfg job Nothing failure result
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
      snapshotResult <- deps.fetchSnapshot SnapshotRequest { jobId = job.jobId, sourceNode, user = action.user }
      case snapshotResult of
        Left failure -> pure (Left (classifySnapshotFetchFailure action failure, emptyProcessResult))
        Right snapshot@SnapshotDocument { snapshotMetadata = SnapshotMetadata { snapshotSize = snapshotSizeValue, snapshotChecksum = snapshotChecksumValue } } ->
          withSnapshotFile snapshot.snapshotBytes \snapshotPath -> do
            logDebug deps.logger "executor.restore" "snapshot_restore_handoff"
              [ "job_id" A..= job.jobId
              , "dispatch_id" A..= job.dispatchId
              , "action_id" A..= action.actionId
              , "source_node" A..= sourceNode
              , "user" A..= action.user
              , "size" A..= snapshotSizeValue
              , "checksum" A..= snapshotChecksumValue
              ]
            let spec = actionCommandSpec session action executablePath cfg.actionTimeoutSeconds [("HOSTENV_RESTORE_SNAPSHOT_FILE", snapshotPath), ("HOSTENV_MIGRATIONS", migrationsCsv action)]
            runActionSpec deps session action spec
    _ -> runActionSpec deps session action (actionCommandSpec session action executablePath cfg.actionTimeoutSeconds [])

buildActionSuccessPayload :: AgentConfig -> DeployAction -> ProcessResult -> Either FailureClassification A.Value
buildActionSuccessPayload cfg action result =
  fmap
    (buildCommandPayload result.exitCode (trimStderrSummary cfg.eventStderrMaxLines result.stderrText))
    (buildActionSuccessFields action result)

buildActionSuccessFields :: DeployAction -> ProcessResult -> Either FailureClassification A.Value
buildActionSuccessFields action result
  | action.op == Backup =
      case decodeBackupSnapshotPayload result.stdoutText of
        Left failure -> Left (classifyBackupSnapshotPayloadFailure action failure)
        Right snapshotPayload ->
          Right
            ( A.object
                [ "user" A..= action.user
                , "op" A..= renderActionOp action.op
                , "snapshotPayload" A..= snapshotPayload
                , "snapshotMetadata" A..= buildSnapshotMetadata snapshotPayload
                ]
            )
  | otherwise =
      Right (A.object ["user" A..= action.user, "op" A..= renderActionOp action.op])

decodeBackupSnapshotPayload :: Text -> Either Text A.Value
decodeBackupSnapshotPayload stdoutText =
  case A.eitherDecodeStrict' (TE.encodeUtf8 stdoutText) of
    Left err -> Left (T.pack err)
    Right payload -> Right payload

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

postEvent :: ExecutionDeps -> AgentConfig -> DeployJob -> Maybe Text -> EventStatus -> Text -> Text -> A.Value -> IO ()
postEvent deps cfg job mActionId status phase message payload =
  deps.emitEvent
    job.jobId
    NodeEvent
      { actionId = mActionId
      , node = cfg.nodeName
      , status
      , phase = Just phase
      , message = Just message
      , payload
      }

postFailure :: ExecutionDeps -> AgentConfig -> DeployJob -> Maybe DeployAction -> FailureClassification -> ProcessResult -> IO ()
postFailure deps cfg job mAction failure result = do
  logWarn deps.logger "executor" failureEvent
    ( [ "job_id" A..= job.jobId
      , "dispatch_id" A..= job.dispatchId
      , "phase" A..= failure.phase
      , "status" A..= eventStatusText failure.status
      , "exit_code" A..= result.exitCode
      , "error_class" A..= failureErrorClass failure result
      , "stderr_summary" A..= trimStderrSummary cfg.eventStderrMaxLines result.stderrText
      ]
        <> maybe [] actionFields mAction
        <> payloadFields failure.payload
    )
  postEvent deps cfg job (fmap (.actionId) mAction) failure.status failure.phase failure.message payload
  where
    failureEvent = maybe "system_failed" (const "action_failed") mAction
    actionFields action =
      [ "action_id" A..= action.actionId
      , "op" A..= renderActionOp action.op
      , "user" A..= action.user
      ]
    payload = buildCommandPayload result.exitCode (trimStderrSummary cfg.eventStderrMaxLines result.stderrText) failure.payload

runSpec :: ExecutionDeps -> ProcessSpec -> IO ProcessResult
runSpec deps spec = do
  startedAt <- deps.currentTime
  logDebug deps.logger "executor.process" "process_started"
    [ "description" A..= spec.description
    , "executable" A..= spec.executable
    , "arguments" A..= spec.arguments
    ]
  result <- deps.processRunner spec
  completedAt <- deps.currentTime
  let durationMs = processDurationMs startedAt completedAt
      logFields =
        [ "description" A..= spec.description
        , "duration_ms" A..= durationMs
        , "exit_code" A..= result.exitCode
        ]
  if result.exitCode == 0
    then logInfo deps.logger "executor.process" "process_completed" logFields
    else logWarn deps.logger "executor.process" "process_failed" (logFields <> ["stderr_summary" A..= result.stderrText, "error_class" A..= ("command_failed" :: Text)])
  pure result

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

withSnapshotFile :: BL.ByteString -> (FilePath -> IO a) -> IO a
withSnapshotFile payloadBytes action = do
  tempDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tempDir "hostenv-restore-snapshot.json"
  hClose handle
  BL.writeFile path payloadBytes
  result <- action path
  removeFile path
  pure result

classifySnapshotFetchFailure :: DeployAction -> SnapshotFetchFailure -> FailureClassification
classifySnapshotFetchFailure action snapshotFailure =
  case snapshotFailure of
    SnapshotMissing ->
      FailureClassification
        { status = EventFailed
        , phase = opName
        , message = "Restore snapshot unavailable during " <> stepLabel
        , payload = A.object ["op" A..= opName, "step" A..= stepLabel, "reason" A..= ("snapshot_missing" :: Text)]
        }
    SnapshotMalformed reason ->
      FailureClassification
        { status = EventFailed
        , phase = opName
        , message = "Restore snapshot payload malformed during " <> stepLabel
        , payload = A.object ["op" A..= opName, "step" A..= stepLabel, "reason" A..= reason]
        }
    SnapshotIntegrityFailure expectedMetadata actualMetadata ->
      FailureClassification
        { status = EventFailed
        , phase = opName
        , message = "Restore snapshot integrity verification failed during " <> stepLabel
        , payload = A.object ["op" A..= opName, "step" A..= stepLabel, "reason" A..= ("integrity_check_failed" :: Text), "expectedMetadata" A..= expectedMetadata, "actualMetadata" A..= actualMetadata]
        }
  where
    opName = renderActionOp action.op
    stepLabel = actionStepLabel (FetchSnapshot action.op action.user)

classifyBackupSnapshotPayloadFailure :: DeployAction -> Text -> FailureClassification
classifyBackupSnapshotPayloadFailure action reason =
  FailureClassification
    { status = EventFailed
    , phase = renderActionOp action.op
    , message = "Backup snapshot payload malformed during " <> actionStepLabel (RunAction action.op action.user)
    , payload = A.object ["op" A..= renderActionOp action.op, "step" A..= actionStepLabel (RunAction action.op action.user), "reason" A..= reason]
    }

migrationsCsv :: DeployAction -> String
migrationsCsv action = T.unpack (T.intercalate "," action.migrations)

shouldVerifyUnits :: ActionOp -> Bool
shouldVerifyUnits op = op `elem` [Activate, Reload, Restore, Backup]

shouldMarkUserState :: ActionOp -> Bool
shouldMarkUserState op = op `elem` [Activate, Reload, Restore, Backup]

data ActionResumeDecision
  = ResumeRunAction
  | ResumeSkipCompleted

sideEffectDecision :: DeployJob -> DeployAction -> AgentState -> ActionResumeDecision
sideEffectDecision job action state
  | isCompletedSideEffect job action state = ResumeSkipCompleted
  | otherwise = ResumeRunAction

resumedActionPayload :: DeployAction -> AgentState -> A.Value
resumedActionPayload action state =
  case Map.lookup action.actionId state.actions >>= extractResultPayload of
    Just payload -> payload
    Nothing ->
      buildCommandPayload
        0
        ""
        (A.object ["user" A..= action.user, "op" A..= renderActionOp action.op, "resumed" A..= True])
  where
    extractResultPayload ActionJournal { resultPayload = payload } = payload

emptyProcessResult :: ProcessResult
emptyProcessResult =
  ProcessResult
    { exitCode = 0
    , stdoutText = ""
    , stderrText = ""
    }

failureErrorClass :: FailureClassification -> ProcessResult -> Text
failureErrorClass failure result =
  case payloadTextField "reason" failure.payload of
    Just reason -> reason
    Nothing
      | failure.status == EventTimedOut -> "timeout"
      | result.exitCode == 3 -> "executable_not_found"
      | result.exitCode == 4 -> "snapshot_missing"
      | result.exitCode `elem` [98, 99] -> "superseded"
      | otherwise -> "command_failed"

payloadFields :: A.Value -> [Pair]
payloadFields = \case
  A.Object objectValue ->
    [ (key, value)
    | (key, value) <- KM.toList objectValue
    , key `notElem` map K.fromText ["stderrSummary", "exitCode"]
    ]
  _ -> []

payloadTextField :: Text -> A.Value -> Maybe Text
payloadTextField name = \case
  A.Object objectValue ->
    case KM.lookup (K.fromText name) objectValue of
      Just (A.String value) -> Just value
      _ -> Nothing
  _ -> Nothing

processDurationMs :: UTCTime -> UTCTime -> Int
processDurationMs startedAt completedAt =
  floor (realToFrac (diffUTCTime completedAt startedAt) * 1000 :: Double)
