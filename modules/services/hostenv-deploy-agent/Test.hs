import Control.Concurrent (MVar, ThreadId, myThreadId, newEmptyMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (cancel, waitCatch, withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (Exception, finally, throwIO, try)
import Control.Monad (void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Hostenv.Provider.DeployAgent.Config (AgentConfig(..), defaultEventStderrMaxLines, loadConfig)
import Hostenv.Provider.DeployAgent.Dispatcher (DispatchDecision(..), decideDispatch)
import Hostenv.Provider.DeployAgent.Executor.Classify
  ( ActionStep(..)
  , FailureClassification(..)
  , SystemStep(..)
  , buildCommandPayload
   , classifyActionFailure
   , classifySystemFailure
   )
import Hostenv.Provider.DeployAgent.Executor (ExecutionDeps(..), JobResult(..), SnapshotRequest(..), executeJob)
import Hostenv.Provider.DeployAgent.Executor.Process (ProcessResult(..), ProcessSpec(..))
import Hostenv.Provider.DeployAgent.Logging (Logger, logInfo, mkJsonLogger)
import Hostenv.Provider.DeployAgent.ProviderApi (SnapshotDocument(..), SnapshotFetchFailure(..), SnapshotMetadata(..), buildSnapshotMetadata, decodeSnapshotResponse, snapshotUrl)
import Hostenv.Provider.DeployAgent.Protocol
  ( ActionOp(..)
    , DeployAction(..)
    , DeployIntent(..)
    , DeployJob(..)
    , EventStatus(..)
    , NodeEvent(..)
    , WsEnvelope(..)
    , WsMessageKind(..)
    , actionStorePath
    , buildWsAuthPayload
    , buildWsResumePayload
    , decodeDeployJobMessage
    , decodeWsEnvelopeMessage
     )
import Hostenv.Provider.DeployAgent.State
  ( ActionJournal(..)
  , ActionJournalStatus(..)
  , AgentState(..)
  , AppliedStorePath(..)
  , CurrentJob(..)
  , emptyStateFor
  , isReportedSideEffect
  , loadStateFile
  , resumePayload
  , writeStateFile
  )
import Hostenv.Provider.DeployAgent.Supervisor (PreparedRuntime(..), SupervisorHooks(..), runSupervisorLoopWith)
import Hostenv.Provider.DeployAgent.Transport.WebSocket
  ( IncomingMessage(..)
  , SessionConnection(..)
  , SessionHooks(..)
  , SessionRuntime(..)
  , WebSocketConfig(..)
  , decodeIncomingMessage
  , defaultSessionHooks
  , deriveWebSocketUrl
  , runSessionOverConnection
  )
import Hostenv.Provider.DeployAgent.UserSession (UserIdentity(..), UserSession(..), buildUserSession, runUserSpec)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..), exitFailure)
import System.IO (hClose, openTempFile)

assert :: Bool -> String -> IO ()
assert cond msg =
  if cond then pure () else putStrLn ("FAIL: " <> msg) >> exitFailure

main :: IO ()
main = do
  testAgentConfigParsingAndValidation
  testWebSocketAuthPayload
  testWsEnvelopeRoundTrip
  testDecodeWsEnvelopeKinds
  testDecodeDeployJobMessage
  testDecodeDeployJobMessageFallsBackToPayloadActionIds
  testDecodeDeployJobMessageRequiresDispatchId
  testDecodeWsEnvelopeRequiresActionId
  testDecodeIncomingWebSocketMessage
  testJournalPersistenceReload
  testDispatcher
  testSystemFailureClassification
  testActionFailureClassification
  testCommandPayloadShape
  testSnapshotUrl
  testSnapshotResponseMissing
  testSnapshotResponseMalformed
  testSnapshotResponseChecksumMismatch
  testUserSessionRunuserEnv
  testStructuredLogging
  testSessionDoesNotSignalReadyBeforeAuthOk
  testSessionSendsResumeAfterAuthOk
  testSessionUsesSingleSenderWriter
  testHeartbeatTimeoutTearsDownSession
  testSessionTeardownOnForcedClose
  testSupervisorReconnectsAfterForcedClose
  testSupervisorReconnectsDuplicateDispatchWithoutRerun
  testSupervisorRetryLogging
  testActionFailureLogging
  testCrashAfterLocalCompletionBeforeReport
  testResumeSkipsCompletedSideEffect
  testResumeRunsNewDispatchAfterJournalMismatch
  putStrLn "ok"

testAgentConfigParsingAndValidation :: IO ()
testAgentConfigParsingAndValidation = do
  withTempStateFile $ \path -> do
    BLC.writeFile path (A.encode validConfigValue)
    cfg <- loadConfig path
    assert (cfg.providerApiBaseUrl == "https://provider.test") "config loader should trim providerApiBaseUrl"
    assert (cfg.nodeName == "node-a") "config loader should trim nodeName"
    assert (cfg.actionTimeoutSeconds == 1800) "config loader should default missing actionTimeoutSeconds"
    assert (cfg.reconnectSeconds == 1) "config loader should clamp reconnectSeconds to a positive value"
    assert (cfg.eventStderrMaxLines == defaultEventStderrMaxLines) "config loader should default eventStderrMaxLines"

  withTempStateFile $ \path -> do
    BLC.writeFile path (A.encode invalidProviderConfigValue)
    result <- try (loadConfig path) :: IO (Either ExitCode AgentConfig)
    assert (result == Left (ExitFailure 1)) "config loader should reject blank providerApiBaseUrl"

  withTempStateFile $ \path -> do
    BLC.writeFile path (A.encode invalidNodeConfigValue)
    result <- try (loadConfig path) :: IO (Either ExitCode AgentConfig)
    assert (result == Left (ExitFailure 1)) "config loader should reject blank nodeName"

testWsEnvelopeRoundTrip :: IO ()
testWsEnvelopeRoundTrip = do
  let envelope = mkEnvelope WsActionResult (Just "dispatch-1") (Just "action-1") (A.object ["status" A..= ("success" :: Text), "phase" A..= ("activate" :: Text)])
  assert (decodeWsEnvelopeMessage (A.encode envelope) == Just envelope) "websocket envelopes should round-trip through the final protocol"

testWebSocketAuthPayload :: IO ()
testWebSocketAuthPayload = do
  let payload = buildWsAuthPayload "node-a" "secret-token" "msg-auth-1" "2026-04-03T13:00:00Z"
  assert (payload == A.object ["version" A..= (1 :: Int), "kind" A..= ("auth" :: Text), "messageId" A..= ("msg-auth-1" :: Text), "timestamp" A..= ("2026-04-03T13:00:00Z" :: Text), "node" A..= ("node-a" :: Text), "payload" A..= A.object ["token" A..= ("secret-token" :: Text)]]) "ws auth payload should use the final websocket envelope"
  assert (deriveWebSocketUrl "https://provider.test" "node-a" == Right "wss://provider.test/api/deploy-jobs/ws?node=node-a") "https api base should derive wss url"

testDecodeWsEnvelopeKinds :: IO ()
testDecodeWsEnvelopeKinds = do
  mapM_ assertEnvelopeDecodes allMessageKinds
  where
    allMessageKinds =
      [ WsHello
      , WsAuth
      , WsAuthOk
      , WsAuthError
      , WsDeployJob
      , WsAck
      , WsProgress
      , WsActionResult
      , WsHeartbeat
      , WsResume
      , WsError
      ]

    assertEnvelopeDecodes kind =
      case decodeWsEnvelopeMessage (A.encode (mkEnvelope kind (kindDispatchId kind) (kindActionId kind) (A.object []))) of
        Just envelope -> assert (envelope.kind == kind) ("websocket envelope should decode kind " <> show kind)
        Nothing -> assert False ("websocket envelope should decode kind " <> show kind)

testDecodeDeployJobMessage :: IO ()
testDecodeDeployJobMessage = do
  let raw = A.encode (deployJobEnvelope (Just "dispatch-1") (Just "action-activate-alice"))
  case decodeDeployJobMessage raw of
    Nothing -> assert False "deploy job websocket payload should decode"
    Just job -> do
      assert (job.jobId == "job-1") "decoded job should keep job id"
      assert (job.dispatchId == "dispatch-1") "decoded job should keep dispatch id"
      case job.intent.actions of
        [action] -> do
          assert (action.actionId == "action-activate-alice") "decoded job should keep action id"
          assert (actionStorePath action == Just "/nix/store/user") "action store path should fall back to envStorePath"
        _ -> assert False "decoded job should contain exactly one action"

testDecodeDeployJobMessageFallsBackToPayloadActionIds :: IO ()
testDecodeDeployJobMessageFallsBackToPayloadActionIds = do
  let raw = A.encode (deployJobEnvelopeWithPayloadActions (Just "dispatch-1") Nothing (Just "job-1:node-a:0"))
  case decodeDeployJobMessage raw of
    Nothing -> assert False "deploy job websocket payload should decode when action ids are supplied by payload.actions"
    Just job ->
      case job.intent.actions of
        [action] -> do
          assert (action.actionId == "job-1:node-a:0") "decoded job should fall back to dispatched action ids"
          assert (A.toJSON action == A.object ["op" A..= ("activate" :: Text), "user" A..= ("alice" :: Text), "envStorePath" A..= ("/nix/store/user" :: Text), "actionId" A..= ("job-1:node-a:0" :: Text)]) "fallback action id should be injected into the decoded raw action"
        _ -> assert False "decoded job should contain exactly one action when falling back to payload.actions"

testDecodeDeployJobMessageRequiresDispatchId :: IO ()
testDecodeDeployJobMessageRequiresDispatchId = do
  let raw = A.encode (deployJobEnvelope Nothing (Just "action-activate-alice"))
  assert (decodeDeployJobMessage raw == Nothing) "deploy job websocket payload should reject missing dispatchId"

testDecodeWsEnvelopeRequiresActionId :: IO ()
testDecodeWsEnvelopeRequiresActionId = do
  let progressRaw = A.encode (mkEnvelope WsProgress (Just "dispatch-1") Nothing (A.object []))
      resultRaw = A.encode (mkEnvelope WsActionResult (Just "dispatch-1") Nothing (A.object []))
  assert (decodeWsEnvelopeMessage progressRaw == Nothing) "progress envelope should reject missing actionId"
  assert (decodeWsEnvelopeMessage resultRaw == Nothing) "action_result envelope should reject missing actionId"

testDecodeIncomingWebSocketMessage :: IO ()
testDecodeIncomingWebSocketMessage = do
  let raw = A.encode (deployJobEnvelope (Just "dispatch-1") (Just "action-activate-alice"))
  case decodeIncomingMessage raw of
    Just (IncomingDeployJob envelope DeployJob { jobId = jobIdValue, dispatchId = dispatchIdValue }) -> do
      assert (envelope.kind == WsDeployJob) "transport should preserve deploy_job envelope kind"
      assert (jobIdValue == "job-1") "transport should surface deploy_job payloads"
      assert (dispatchIdValue == "dispatch-1") "transport should surface dispatch identity"
    _ -> assert False "transport should decode deploy_job websocket messages"

  case decodeIncomingMessage (A.encode (mkEnvelope WsHeartbeat Nothing Nothing (A.object []))) of
    Just (IncomingEnvelope envelope) -> assert (envelope.kind == WsHeartbeat) "transport should surface non-job websocket envelopes"
    _ -> assert False "transport should decode non-job websocket envelopes"

testJournalPersistenceReload :: IO ()
testJournalPersistenceReload =
  withTempStateFile $ \path -> do
      let journal =
            (emptyStateFor "node-a")
              { current = Just CurrentJob { jobId = "job-1", dispatchId = "dispatch-1", startedAt = "2026-04-03T13:00:00Z" }
              , actions =
                  Map.singleton
                    "action-restore-alice"
                    ActionJournal
                      { status = ActionCompletedLocal
                      , startedAt = "2026-04-03T13:01:00Z"
                      , completedAt = Just "2026-04-03T13:02:00Z"
                      , reportedFinal = False
                      , sideEffect = "restore"
                      , resultPayload = Nothing
                      }
              , system = Just AppliedStorePath { storePath = "/nix/store/system", updatedAt = "2026-04-03T13:00:30Z" }
              , users = Map.singleton "alice" AppliedStorePath { storePath = "/nix/store/user", updatedAt = "2026-04-03T13:02:00Z" }
              , updatedAt = "2026-04-03T13:02:00Z"
              }
      writeStateFile path journal
      reloaded <- loadStateFile "node-a" path
      assert (reloaded == journal) "journal state should persist and reload exactly"
      let legacyRaw = BLC.pack "{\"lastAppliedJobId\":\"job-1\",\"lastAppliedSignature\":\"sig\",\"lastCommitSha\":\"abc123\",\"system\":{},\"users\":{},\"updatedAt\":\"\"}"
      assert ((A.decode legacyRaw :: Maybe AgentState) == Nothing) "legacy parity state payload should no longer decode"
      let resume = buildWsResumePayload "node-a" (resumePayload reloaded) "msg-resume-1" "2026-04-03T13:03:00Z"
      case decodeWsEnvelopeMessage (A.encode resume) of
        Just envelope -> assert (envelope.kind == WsResume) "resume payload should encode as a websocket resume envelope"
        Nothing -> assert False "resume payload should be a valid websocket envelope"

testDispatcher :: IO ()
testDispatcher = do
  let action = mkAction "action-activate-alice" Activate "alice" ["storePath" A..= ("/nix/store/user" :: Text)]
      job = DeployJob "job-1" "dispatch-1" "abc123" Nothing (DeployIntent 1 Nothing Nothing [action])
      resumedState = emptyStateFor "node-a"
  case decideDispatch resumedState job of
    DispatchHandle handledJob ->
      assert (handledJob.dispatchId == "dispatch-1") "dispatcher should always hand the authoritative dispatch to the executor"

testSystemFailureClassification :: IO ()
testSystemFailureClassification = do
  let superseded = classifySystemFailure SystemRealise 99
      normalFailure = classifySystemFailure SystemSwitch 1
      FailureClassification { phase = supersededPhase } = superseded
      FailureClassification { message = normalMessage } = normalFailure
  assert (supersededPhase == ("intent" :: Text)) "superseded system failure should report intent phase"
  assert (normalMessage == ("System switch failed" :: Text)) "system switch failure message should match shell behavior"

testActionFailureClassification :: IO ()
testActionFailureClassification = do
  let timedOut = classifyActionFailure Restore (RunAction Restore "alice") 124
      missingExec = classifyActionFailure Activate (ResolveExecutable Activate "alice") 3
      snapshotMissing = classifyActionFailure Restore (FetchSnapshot Restore "alice") 4
      FailureClassification { status = timedOutStatus } = timedOut
      FailureClassification { message = missingMessage } = missingExec
      FailureClassification { message = snapshotMessage } = snapshotMissing
  assert (timedOutStatus == EventTimedOut) "timeout should map to timed_out status"
  assert (missingMessage == ("Action executable not found during resolve-exec-activate-alice" :: Text)) "missing executable classification should preserve step label"
  assert (snapshotMessage == ("Restore snapshot unavailable during fetch-snapshot-restore-alice" :: Text)) "snapshot fetch failures should classify restore unavailability"

testCommandPayloadShape :: IO ()
testCommandPayloadShape = do
  let payload = buildCommandPayload 17 "trimmed stderr" (A.object ["step" A..= ("system-switch" :: Text)])
  assert (payload == A.object ["exitCode" A..= (17 :: Int), "stderrSummary" A..= ("trimmed stderr" :: Text), "step" A..= ("system-switch" :: Text)]) "command payload should merge extra object fields"

testSnapshotUrl :: IO ()
testSnapshotUrl = do
  let request = SnapshotRequest { jobId = "job-1", sourceNode = "node a", user = "alice@example.com" }
      url = snapshotUrl "https://provider.test" "node-b" request
  assert (url == "https://provider.test/api/deploy-jobs/job-1/backup-snapshot?node=node-b&source=node%20a&user=alice%40example.com") "snapshot URL should match provider backup-snapshot contract"

testSnapshotResponseMissing :: IO ()
testSnapshotResponseMissing =
  assert
    (decodeSnapshotResponse 404 BLC.empty == Left SnapshotMissing)
    "snapshot fetch should treat non-200 responses as missing"

testSnapshotResponseMalformed :: IO ()
testSnapshotResponseMalformed =
  case decodeSnapshotResponse 200 (A.encode (A.object ["metadata" A..= buildSnapshotMetadata (A.object ["snapshots" A..= A.object []])])) of
    Left (SnapshotMalformed _) -> pure ()
    _ -> assert False "snapshot fetch should reject malformed payload responses"

testSnapshotResponseChecksumMismatch :: IO ()
testSnapshotResponseChecksumMismatch =
  case decodeSnapshotResponse 200 (A.encode (A.object ["metadata" A..= badMetadata, "payload" A..= payload])) of
    Left (SnapshotIntegrityFailure _ _) -> pure ()
    _ -> assert False "snapshot fetch should reject checksum mismatches"
  where
    payload = A.object ["snapshots" A..= A.object ["db" A..= ("snapshot-1" :: Text)]]
    goodMetadata = buildSnapshotMetadata payload
    badMetadata = A.object ["size" A..= goodMetadata.snapshotSize, "checksum" A..= ("deadbeef" :: Text), "contentType" A..= goodMetadata.snapshotContentType, "schemaVersion" A..= goodMetadata.snapshotSchemaVersion]

testUserSessionRunuserEnv :: IO ()
testUserSessionRunuserEnv = do
  let session = buildUserSession UserIdentity { name = "alice", uid = 42, home = "/srv/alice" }
      UserSession { profile = profilePath } = session
      wrapped =
        runUserSpec session
          ProcessSpec
            { description = "activate-alice"
            , executable = "/nix/store/profile/bin/activate"
            , arguments = []
            , environment = [("HOSTENV_MIGRATIONS", "db")]
            , workingDirectory = Nothing
            , timeoutSeconds = Just 30
            }
  assert (take 4 wrapped.arguments == ["-u", "alice", "--", "env"]) "runuser command should wrap env invocation"
  assert (profilePath == "/nix/var/nix/profiles/per-user/alice/profile") "user session should derive the per-user profile path"
  assert ("HOME=/srv/alice" `elem` wrapped.arguments) "runuser environment should inject HOME"
  assert ("XDG_RUNTIME_DIR=/run/user/42" `elem` wrapped.arguments) "runuser environment should inject runtime dir"
  assert ("DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/42/bus" `elem` wrapped.arguments) "runuser environment should inject dbus address"
  assert ("XDG_STATE_HOME=/srv/alice/.local/state" `elem` wrapped.arguments) "runuser environment should inject state dir"

testStructuredLogging :: IO ()
testStructuredLogging = do
  sinkRef <- newIORef ([] :: [BLC.ByteString])
  let now = pure (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0))
      logger = mkJsonLogger now (writeIORefAppend sinkRef)
  logInfo logger "executor" "job_started"
    [ "job_id" A..= ("job-1" :: Text)
    , "dispatch_id" A..= ("dispatch-1" :: Text)
    , "duration_ms" A..= (17 :: Int)
    , "exit_code" A..= (0 :: Int)
    , "error_class" A..= ("none" :: Text)
    ]
  entries <- readIORef sinkRef
  case entries of
    [entry] ->
      case A.decode entry of
        Nothing -> assert False "structured log line should decode as json"
        Just value -> do
          assert (value == expectedLogValue) "structured log entry should emit stable structured fields"
          case value of
            A.Object obj -> do
              assert (isNothing (KM.lookup "message" obj)) "structured log entry should not include a free-form message field"
              assert (isNothing (KM.lookup "fields" obj)) "structured log entry should not nest dynamic fields"
            _ -> assert False "structured log entry should decode to an object"
    _ -> assert False "logger should emit exactly one entry"

testSessionDoesNotSignalReadyBeforeAuthOk :: IO ()
testSessionDoesNotSignalReadyBeforeAuthOk = do
  readySignalsRef <- newIORef ([] :: [Text])
  receiveQueue <- newReceiveQueue
  enqueueReceiveStep receiveQueue (throwIO TestForcedClose)
  let connection =
        SessionConnection
          { receiveFrame = nextReceiveFrame receiveQueue
          , sendFrame = \_ -> pure ()
          , withHeartbeat = id
          }
      cfg =
        testWebSocketConfig
          { onAuthenticated = \sessionId _ -> modifyIORef' readySignalsRef (<> [sessionId])
          }
  _ <- try (runSessionOverConnection testLogger cfg defaultSessionHooks connection (\_ _ -> pure ())) :: IO (Either TestForcedClose ())
  readySignals <- readIORef readySignalsRef
  assert (null readySignals) "session readiness callback should not run before auth_ok"

testSessionSendsResumeAfterAuthOk :: IO ()
testSessionSendsResumeAfterAuthOk = do
  resumeSent <- (newEmptyMVar :: IO (MVar ()))
  readySignalsRef <- newIORef ([] :: [Text])
  sentRef <- newIORef ([] :: [SentFrame])
  receiveQueue <- newReceiveQueue
  enqueueReceiveStep receiveQueue (pure (A.encode (mkEnvelope WsAuthOk Nothing Nothing (A.object []))))
  enqueueReceiveStep receiveQueue (takeMVar resumeSent >> throwIO TestForcedClose)
  let connection =
        SessionConnection
          { receiveFrame = nextReceiveFrame receiveQueue
          , sendFrame = \payload -> do
              threadId <- myThreadId
              modifyIORef' sentRef (<> [SentFrame { senderThread = threadId, body = payload }])
              case decodeWsEnvelopeMessage payload of
                Just envelope | envelope.kind == WsResume -> void (tryPutMVar resumeSent ())
                _ -> pure ()
          , withHeartbeat = id
          }
      cfg =
        testWebSocketConfig
          { onAuthenticated = \sessionId envelope ->
              modifyIORef' readySignalsRef (<> [sessionId <> ":" <> envelope.messageId])
          }
  _ <- try (runSessionOverConnection testLogger cfg defaultSessionHooks connection (\_ _ -> pure ())) :: IO (Either TestForcedClose ())
  sentFrames <- readIORef sentRef
  readySignals <- readIORef readySignalsRef
  assert (map frameKind sentFrames == [Just WsAuth, Just WsResume]) "session should send a resume envelope after auth_ok"
  assert (readySignals == ["session-node-a-2026-04-03T13-00-00Z:msg-1"]) "session should signal readiness exactly once after auth_ok"

testSessionUsesSingleSenderWriter :: IO ()
testSessionUsesSingleSenderWriter = do
  ackSent <- (newEmptyMVar :: IO (MVar ()))
  sentRef <- newIORef ([] :: [SentFrame])
  receiveQueue <- newReceiveQueue
  enqueueReceiveStep receiveQueue (pure (A.encode (deployJobEnvelope (Just "dispatch-1") (Just "action-activate-alice"))))
  enqueueReceiveStep receiveQueue (takeMVar ackSent >> throwIO TestForcedClose)
  let connection =
        SessionConnection
          { receiveFrame = nextReceiveFrame receiveQueue
          , sendFrame = \payload -> do
              threadId <- myThreadId
              modifyIORef' sentRef (<> [SentFrame { senderThread = threadId, body = payload }])
              case decodeWsEnvelopeMessage payload of
                Just envelope | envelope.kind == WsAck -> void (tryPutMVar ackSent ())
                _ -> pure ()
          , withHeartbeat = id
          }
      onJob sessionRuntime _ =
        sessionRuntime.sendJson (A.toJSON (mkEnvelope WsAck (Just "dispatch-1") Nothing (A.object [])))
  _ <- try (runSessionOverConnection testLogger testWebSocketConfig defaultSessionHooks connection onJob) :: IO (Either TestForcedClose ())
  sentFrames <- readIORef sentRef
  assert (length sentFrames == 2) "session should send auth and ack through the sender queue"
  assert (length (nub (map (.senderThread) sentFrames)) == 1) "only the sender thread should write websocket payloads"
  assert (map frameKind sentFrames == [Just WsAuth, Just WsAck]) "sender queue should preserve websocket send order"

testHeartbeatTimeoutTearsDownSession :: IO ()
testHeartbeatTimeoutTearsDownSession = do
  authSent <- (newEmptyMVar :: IO (MVar ()))
  stalledReceive <- (newEmptyMVar :: IO (MVar BLC.ByteString))
  (hooks, readStarted, readStopped) <- recordSessionHooks
  let connection =
        SessionConnection
          { receiveFrame = takeMVar stalledReceive
          , sendFrame = \payload ->
              case decodeWsEnvelopeMessage payload of
                Just envelope | envelope.kind == WsAuth -> void (tryPutMVar authSent ())
                _ -> pure ()
          , withHeartbeat = \action ->
              withAsync action $ \sessionAsync -> do
                takeMVar authSent
                threadDelay 10000
                cancel sessionAsync
                _ <- waitCatch sessionAsync
                throwIO TestHeartbeatTimeout
          }
  result <- try (runSessionOverConnection testLogger testWebSocketConfig hooks connection (\_ _ -> pure ())) :: IO (Either TestHeartbeatTimeout ())
  case result of
    Left TestHeartbeatTimeout -> pure ()
    Right () -> assert False "heartbeat timeout should fail the session"
  assertSessionWorkersStopped readStarted readStopped "heartbeat timeout should tear down the session"

testSessionTeardownOnForcedClose :: IO ()
testSessionTeardownOnForcedClose = do
  dispatcherStarted <- (newEmptyMVar :: IO (MVar ()))
  dispatcherStopped <- (newEmptyMVar :: IO (MVar ()))
  blockedJob <- (newEmptyMVar :: IO (MVar ()))
  receiveQueue <- newReceiveQueue
  (hooks, readStarted, readStopped) <- recordSessionHooks
  enqueueReceiveStep receiveQueue (pure (A.encode (deployJobEnvelope (Just "dispatch-1") (Just "action-activate-alice"))))
  enqueueReceiveStep receiveQueue (takeMVar dispatcherStarted >> throwIO TestForcedClose)
  let connection =
        SessionConnection
          { receiveFrame = nextReceiveFrame receiveQueue
          , sendFrame = \_ -> pure ()
          , withHeartbeat = id
          }
      onJob _ _ = do
        void (tryPutMVar dispatcherStarted ())
        takeMVar blockedJob `finally` void (tryPutMVar dispatcherStopped ())
  result <- try (runSessionOverConnection testLogger testWebSocketConfig hooks connection onJob) :: IO (Either TestForcedClose ())
  case result of
    Left TestForcedClose -> pure ()
    Right () -> assert False "forced close should fail the session"
  takeMVar dispatcherStopped
  assertSessionWorkersStopped readStarted readStopped "forced close should cancel receiver, sender, and dispatcher"

testSupervisorReconnectsAfterForcedClose :: IO ()
testSupervisorReconnectsAfterForcedClose = do
  attemptRef <- newIORef (0 :: Int)
  sleepRef <- newIORef ([] :: [Int])
  let hooks =
        SupervisorHooks
          { runSession = \_ -> do
              attempt <- incrementCounter attemptRef
              if attempt == 1 then throwIO TestForcedClose else pure ()
          , sleepBeforeReconnect = \seconds -> modifyIORef' sleepRef (<> [seconds])
          , shouldReconnect = \attempt _ -> pure (attempt < 2)
          }
  runSupervisorLoopWith testLogger testExecutionDeps testAgentConfig testPreparedRuntime hooks
  attempts <- readIORef attemptRef
  sleeps <- readIORef sleepRef
  assert (attempts == 2) "supervisor should open a new session after a forced close"
  assert (sleeps == [testAgentConfig.reconnectSeconds]) "supervisor should wait exactly one reconnect interval before retrying"

testSupervisorReconnectsDuplicateDispatchWithoutRerun :: IO ()
testSupervisorReconnectsDuplicateDispatchWithoutRerun = do
  attemptRef <- newIORef (0 :: Int)
  activeAttemptRef <- newIORef (0 :: Int)
  sleepRef <- newIORef ([] :: [Int])
  sentRef <- newIORef ([] :: [AttemptedSentFrame])
  stateRef <- newIORef (emptyStateFor "node-a")
  commandsRef <- newIORef ([] :: [Text])
  eventsRef <- newIORef ([] :: [EventStatus])
  secondAttemptDone <- (newEmptyMVar :: IO (MVar ()))
  firstReceiveQueue <- newReceiveQueue
  secondReceiveQueue <- newReceiveQueue
  let authOkFrame = A.encode (mkEnvelope WsAuthOk Nothing Nothing (A.object []))
      deployJobFrame = A.encode (deployJobEnvelope (Just sideEffectJob.dispatchId) (Just sideEffectAction.actionId))
  enqueueReceiveStep firstReceiveQueue (pure authOkFrame)
  enqueueReceiveStep firstReceiveQueue (pure deployJobFrame)
  enqueueReceiveStep secondReceiveQueue (pure authOkFrame)
  enqueueReceiveStep secondReceiveQueue (pure deployJobFrame)
  enqueueReceiveStep secondReceiveQueue (takeMVar secondAttemptDone >> throwIO TestForcedClose)
  let deps =
        (mkExecutionDeps stateRef commandsRef eventsRef emitForAttempt)
          { logger = testLogger
          }
      emitForAttempt recordedEventsRef phaseValue statusValue = do
        attempt <- readIORef activeAttemptRef
        if attempt == 1
          then crashOnActionSuccess recordedEventsRef phaseValue statusValue
          else recordEventOnly recordedEventsRef phaseValue statusValue
      connectionFor attempt queue =
        SessionConnection
          { receiveFrame = nextReceiveFrame queue
          , sendFrame = \payload -> do
              let kindValue = decodeWsEnvelopeMessage payload >>= (Just . (.kind))
              modifyIORef' sentRef (<> [AttemptedSentFrame { connectionAttempt = attempt, sentKind = kindValue }])
          , withHeartbeat = id
          }
      hooks =
        SupervisorHooks
          { runSession = \onJob -> do
              attempt <- incrementCounter attemptRef
              writeIORef activeAttemptRef attempt
              let connection =
                    if attempt == 1
                      then connectionFor attempt firstReceiveQueue
                      else connectionFor attempt secondReceiveQueue
              runSessionOverConnection testLogger testWebSocketConfig defaultSessionHooks connection $ \sessionRuntime _ -> do
                result <- onJob sessionRuntime sideEffectJob
                if attempt == 2
                  then void (tryPutMVar secondAttemptDone ()) >> pure result
                  else pure result
          , sleepBeforeReconnect = \seconds -> modifyIORef' sleepRef (<> [seconds])
          , shouldReconnect = \attempt _ -> pure (attempt < 2)
          }
  runSupervisorLoopWith testLogger deps testAgentConfig testPreparedRuntime hooks
  attempts <- readIORef attemptRef
  sleeps <- readIORef sleepRef
  commands <- readIORef commandsRef
  sentFrames <- readIORef sentRef
  finalState <- readIORef stateRef
  let reconnectFrames =
        filter
          (\(_, mKind) -> mKind `elem` [Just WsAuth, Just WsResume])
          (map (\frame -> (frame.connectionAttempt, frame.sentKind)) sentFrames)
  assert (attempts == 2) "supervisor should reconnect after an in-process websocket disconnect"
  assert (sleeps == [testAgentConfig.reconnectSeconds]) "reconnect-on-disconnect should use the configured backoff"
  assert (commands == ["deactivate-alice"]) "duplicate dispatch replay after reconnect should not rerun a completed side effect"
  assert (reconnectFrames == [(1, Just WsAuth), (1, Just WsResume), (2, Just WsAuth), (2, Just WsResume)]) "each reconnect attempt should re-authenticate and resend resume state"
  assert (finalState.current == Nothing) "resume after duplicate replay should clear the in-flight job marker"
  assert (Map.null finalState.actions) "resume after duplicate replay should compact completed action journal entries"

testSupervisorRetryLogging :: IO ()
testSupervisorRetryLogging = do
  sinkRef <- newIORef ([] :: [BLC.ByteString])
  attemptRef <- newIORef (0 :: Int)
  let logger = testCapturedLogger sinkRef
      hooks =
        SupervisorHooks
          { runSession = \_ -> do
              attempt <- incrementCounter attemptRef
              if attempt == 1 then throwIO TestForcedClose else pure ()
          , sleepBeforeReconnect = \_ -> pure ()
          , shouldReconnect = \attempt _ -> pure (attempt < 2)
          }
  runSupervisorLoopWith logger testExecutionDeps testAgentConfig testPreparedRuntime hooks
  retryEntry <- findLogEntry sinkRef "session_reconnect_scheduled"
  assert (lookupIntField "attempt" retryEntry == Just 1) "retry log should include the reconnect attempt"
  assert (lookupIntField "backoff_seconds" retryEntry == Just testAgentConfig.reconnectSeconds) "retry log should include the reconnect backoff"

testActionFailureLogging :: IO ()
testActionFailureLogging = do
  sinkRef <- newIORef ([] :: [BLC.ByteString])
  stateRef <- newIORef (emptyStateFor "node-a")
  commandsRef <- newIORef ([] :: [Text])
  eventsRef <- newIORef ([] :: [EventStatus])
  let logger = testCapturedLogger sinkRef
      deps =
        (mkExecutionDeps stateRef commandsRef eventsRef recordEventOnly)
          { logger
          , processRunner = \spec -> do
              modifyIORef' commandsRef (<> [spec.description])
              pure
                ( if spec.description == "deactivate-alice"
                    then ProcessResult { exitCode = 17, stdoutText = "", stderrText = "stderr line 1\nstderr line 2" }
                    else ProcessResult { exitCode = 0, stdoutText = "", stderrText = "" }
                )
          }
  result <- executeJob deps testAgentConfig sideEffectJob
  case result of
    JobFailed _ -> pure ()
    _ -> assert False "failing action should surface a failed job result"
  failureEntry <- findLogEntry sinkRef "action_failed"
  assert (lookupTextFieldValue "dispatch_id" failureEntry == Just sideEffectJob.dispatchId) "failure log should include the dispatch id"
  assert (lookupTextFieldValue "action_id" failureEntry == Just sideEffectAction.actionId) "failure log should include the action id"
  assert (lookupIntField "exit_code" failureEntry == Just 17) "failure log should include the action exit code"
  assert (lookupTextFieldValue "error_class" failureEntry == Just "command_failed") "failure log should include the classified error"
  assert (lookupTextFieldValue "stderr_summary" failureEntry == Just "stderr line 1\nstderr line 2") "failure log should include the stderr summary"

testCrashAfterLocalCompletionBeforeReport :: IO ()
testCrashAfterLocalCompletionBeforeReport = do
  stateRef <- newIORef (emptyStateFor "node-a")
  commandsRef <- newIORef ([] :: [Text])
  eventsRef <- newIORef ([] :: [EventStatus])
  let job = sideEffectJob
      crashingDeps = mkExecutionDeps stateRef commandsRef eventsRef crashOnActionSuccess
  firstResult <- try (executeJob crashingDeps testAgentConfig job) :: IO (Either TestForcedClose JobResult)
  case firstResult of
    Left TestForcedClose -> pure ()
    Right _ -> assert False "executor should surface a crash between local completion and final report"
  crashedState <- readIORef stateRef
  assert (isReportedSideEffect job sideEffectAction crashedState == False) "crash window should leave the completed side effect marked as unreported"
  case Map.lookup sideEffectAction.actionId crashedState.actions of
    Just journalEntry -> do
      assert (journalEntry.status == ActionCompletedLocal) "crash window should persist local completion before the final report"
      assert (journalEntry.sideEffect == "deactivate") "crash window should record the side-effect kind"
    Nothing -> assert False "crash window should persist a journal entry for the completed side effect"
  commandsAfterCrash <- readIORef commandsRef
  assert (not (null commandsAfterCrash)) "crash window should run the side effect before the crash"

  resumedEventsRef <- newIORef ([] :: [EventStatus])
  let resumedDeps = mkExecutionDeps stateRef commandsRef resumedEventsRef recordEventOnly
  resumedResult <- executeJob resumedDeps testAgentConfig job
  case resumedResult of
    JobSucceeded finalState -> do
      finalCommands <- readIORef commandsRef
      resumedEvents <- readIORef resumedEventsRef
      assert (finalCommands == commandsAfterCrash) "resume should not rerun a side effect already completed locally"
      assert (EventSuccess `elem` resumedEvents) "resume should safely re-emit the missing final success"
      assert (finalState.current == Nothing) "successful resume should clear the in-flight job marker"
      assert (Map.null finalState.actions) "successful resume should compact action journal entries"
    _ -> assert False "resume after crash should complete successfully"

testResumeSkipsCompletedSideEffect :: IO ()
testResumeSkipsCompletedSideEffect = do
  stateRef <-
    newIORef
      ( (emptyStateFor "node-a")
          { current = Just CurrentJob { jobId = sideEffectJob.jobId, dispatchId = sideEffectJob.dispatchId, startedAt = "2026-01-01T00:00:00Z" }
          , actions =
              Map.singleton
                sideEffectAction.actionId
                ActionJournal
                  { status = ActionCompletedLocal
                  , startedAt = "2026-01-01T00:00:00Z"
                  , completedAt = Just "2026-01-01T00:00:00Z"
                  , reportedFinal = True
                  , sideEffect = "deactivate"
                  , resultPayload = Nothing
                  }
          , updatedAt = "2026-01-01T00:00:00Z"
          }
      )
  commandsRef <- newIORef ([] :: [Text])
  eventsRef <- newIORef ([] :: [EventStatus])
  result <- executeJob (mkExecutionDeps stateRef commandsRef eventsRef recordEventOnly) testAgentConfig sideEffectJob
  case result of
    JobSucceeded finalState -> do
      commands <- readIORef commandsRef
      events <- readIORef eventsRef
      assert (null commands) "resume should skip rerunning an already reported side effect"
      assert (events == [EventRunning, EventSuccess]) "resume should emit only job-level intent events when the side effect was already reported"
      assert (finalState.current == Nothing) "resume skip should still clear the in-flight job marker"
    _ -> assert False "resume skip should still let the job complete"

testResumeRunsNewDispatchAfterJournalMismatch :: IO ()
testResumeRunsNewDispatchAfterJournalMismatch = do
  let newerJob = DeployJob "job-side-effect" "dispatch-new" "abc123" Nothing (DeployIntent 1 Nothing Nothing [sideEffectAction])
  stateRef <-
    newIORef
      ( (emptyStateFor "node-a")
          { current = Just CurrentJob { jobId = "job-side-effect", dispatchId = "dispatch-old", startedAt = "2026-01-01T00:00:00Z" }
          , actions =
              Map.singleton
                sideEffectAction.actionId
                ActionJournal
                  { status = ActionCompletedLocal
                  , startedAt = "2026-01-01T00:00:00Z"
                  , completedAt = Just "2026-01-01T00:00:00Z"
                  , reportedFinal = True
                  , sideEffect = "deactivate"
                  , resultPayload = Nothing
                  }
          , updatedAt = "2026-01-01T00:00:00Z"
          }
      )
  commandsRef <- newIORef ([] :: [Text])
  eventsRef <- newIORef ([] :: [EventStatus])
  result <- executeJob (mkExecutionDeps stateRef commandsRef eventsRef recordEventOnly) testAgentConfig newerJob
  case result of
    JobSucceeded finalState -> do
      commands <- readIORef commandsRef
      assert (commands == ["deactivate-alice"]) "resume should rerun a side effect when provider state advances to a new dispatch"
      assert (finalState.current == Nothing) "rerunning a new dispatch should still clear the in-flight job marker"
    _ -> assert False "a new dispatch should be allowed to rerun the side effect"

expectedLogValue :: A.Value
expectedLogValue =
  A.object
    [ "timestamp" A..= ("2026-01-01T00:00:00Z" :: Text)
    , "severity" A..= ("info" :: Text)
    , "component" A..= ("executor" :: Text)
    , "event" A..= ("job_started" :: Text)
    , "session_id" A..= (Nothing :: Maybe Text)
    , "message_id" A..= (Nothing :: Maybe Text)
    , "dispatch_id" A..= Just ("dispatch-1" :: Text)
    , "action_id" A..= (Nothing :: Maybe Text)
    , "duration_ms" A..= Just (17 :: Int)
    , "exit_code" A..= Just (0 :: Int)
    , "error_class" A..= Just ("none" :: Text)
    , "job_id" A..= ("job-1" :: Text)
    ]

mkAction :: Text -> ActionOp -> Text -> [Pair] -> DeployAction
mkAction actionId op user fields =
  DeployAction
    { op
    , actionId
    , user
    , storePath = lookupTextField "storePath" fields
    , envStorePath = lookupTextField "envStorePath" fields
    , path = Nothing
    , fromNode = Nothing
    , toNode = Nothing
    , migrations = []
    , rawAction = A.object (["actionId" A..= actionId, "op" A..= op, "user" A..= user] <> fields)
    }

mkEnvelope :: WsMessageKind -> Maybe Text -> Maybe Text -> A.Value -> WsEnvelope
mkEnvelope kind dispatchId actionId payload =
  WsEnvelope
    { version = 1
    , kind
    , messageId = "msg-1"
    , timestamp = "2026-04-03T13:00:00Z"
    , node = "node-a"
    , jobId = kindJobId kind
    , dispatchId
    , actionId
    , payload
    }

kindJobId :: WsMessageKind -> Maybe Text
kindJobId = \case
  WsDeployJob -> Just "job-1"
  WsAck -> Just "job-1"
  WsProgress -> Just "job-1"
  WsActionResult -> Just "job-1"
  WsError -> Just "job-1"
  _ -> Nothing

kindDispatchId :: WsMessageKind -> Maybe Text
kindDispatchId = \case
  WsDeployJob -> Just "dispatch-1"
  WsAck -> Just "dispatch-1"
  WsProgress -> Just "dispatch-1"
  WsActionResult -> Just "dispatch-1"
  WsError -> Just "dispatch-1"
  _ -> Nothing

kindActionId :: WsMessageKind -> Maybe Text
kindActionId = \case
  WsProgress -> Just "action-1"
  WsActionResult -> Just "action-1"
  _ -> Nothing

deployJobEnvelope :: Maybe Text -> Maybe Text -> WsEnvelope
deployJobEnvelope dispatchId actionId =
  deployJobEnvelopeWithPayloadActions dispatchId actionId actionId

deployJobEnvelopeWithPayloadActions :: Maybe Text -> Maybe Text -> Maybe Text -> WsEnvelope
deployJobEnvelopeWithPayloadActions dispatchId intentActionId payloadActionId =
  mkEnvelope
    WsDeployJob
    dispatchId
    Nothing
    (A.object ["commitSha" A..= ("abc123" :: Text), "intent" A..= A.object ["schemaVersion" A..= (1 :: Int), "systemToplevel" A..= ("/nix/store/system" :: Text), "actions" A..= [A.object (["op" A..= ("activate" :: Text), "user" A..= ("alice" :: Text), "envStorePath" A..= ("/nix/store/user" :: Text)] <> maybe [] (\value -> ["actionId" A..= value]) intentActionId)]], "actions" A..= [A.object (["op" A..= ("activate" :: Text), "user" A..= ("alice" :: Text)] <> maybe [] (\value -> ["actionId" A..= value]) payloadActionId)]])

lookupTextField :: Text -> [Pair] -> Maybe Text
lookupTextField name pairs =
  case A.decode (A.encode (A.object pairs)) of
    Just (A.Object obj) ->
      case KM.lookup (K.fromText name) obj of
        Just (A.String value) -> Just value
        _ -> Nothing
    _ -> Nothing

writeIORefAppend :: IORef [BLC.ByteString] -> BLC.ByteString -> IO ()
writeIORefAppend ref value = do
  current <- readIORef ref
  writeIORef ref (current <> [value])

data TestForcedClose = TestForcedClose
  deriving (Eq, Show)

instance Exception TestForcedClose

data TestHeartbeatTimeout = TestHeartbeatTimeout
  deriving (Eq, Show)

instance Exception TestHeartbeatTimeout

data SentFrame = SentFrame
  { senderThread :: ThreadId
  , body :: BLC.ByteString
  }

data AttemptedSentFrame = AttemptedSentFrame
  { connectionAttempt :: Int
  , sentKind :: Maybe WsMessageKind
  }

validConfigValue :: A.Value
validConfigValue =
  A.object
    [ "providerApiBaseUrl" A..= ("  https://provider.test  " :: Text)
    , "nodeAuthTokenFile" A..= ("/tmp/token" :: FilePath)
    , "nodeName" A..= ("  node-a  " :: Text)
    , "stateFile" A..= ("/tmp/state.json" :: FilePath)
    , "reconnectSeconds" A..= (0 :: Int)
    ]

invalidProviderConfigValue :: A.Value
invalidProviderConfigValue =
  A.object
    [ "providerApiBaseUrl" A..= ("   " :: Text)
    , "nodeAuthTokenFile" A..= ("/tmp/token" :: FilePath)
    , "nodeName" A..= ("node-a" :: Text)
    , "stateFile" A..= ("/tmp/state.json" :: FilePath)
    ]

invalidNodeConfigValue :: A.Value
invalidNodeConfigValue =
  A.object
    [ "providerApiBaseUrl" A..= ("https://provider.test" :: Text)
    , "nodeAuthTokenFile" A..= ("/tmp/token" :: FilePath)
    , "nodeName" A..= ("   " :: Text)
    , "stateFile" A..= ("/tmp/state.json" :: FilePath)
    ]

type ReceiveQueue = TQueue (IO BLC.ByteString)

newReceiveQueue :: IO ReceiveQueue
newReceiveQueue = newTQueueIO

enqueueReceiveStep :: ReceiveQueue -> IO BLC.ByteString -> IO ()
enqueueReceiveStep queue step =
  atomically (writeTQueue queue step)

nextReceiveFrame :: ReceiveQueue -> IO BLC.ByteString
nextReceiveFrame queue = do
  step <- atomically (readTQueue queue)
  step

recordSessionHooks :: IO (SessionHooks, IO [Text], IO [Text])
recordSessionHooks = do
  startedRef <- newIORef ([] :: [Text])
  stoppedRef <- newIORef ([] :: [Text])
  let record ref name = modifyIORef' ref (<> [name])
      hooks =
        SessionHooks
          { workerStarted = record startedRef
          , workerStopped = record stoppedRef
          }
  pure (hooks, readIORef startedRef, readIORef stoppedRef)

assertSessionWorkersStopped :: IO [Text] -> IO [Text] -> String -> IO ()
assertSessionWorkersStopped readStarted readStopped message = do
  started <- readStarted
  stopped <- readStopped
  assert (sort started == ["dispatcher", "receiver", "sender"]) (message <> " (all workers should start)")
  assert (sort stopped == ["dispatcher", "receiver", "sender"]) (message <> " (all workers should stop)")

frameKind :: SentFrame -> Maybe WsMessageKind
frameKind sentFrame =
  case decodeWsEnvelopeMessage sentFrame.body of
    Just envelope -> Just envelope.kind
    Nothing -> Nothing

incrementCounter :: IORef Int -> IO Int
incrementCounter ref = do
  modifyIORef' ref (+ 1)
  readIORef ref

testLogger :: Logger
testLogger = mkJsonLogger (pure (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0))) (\_ -> pure ())

testWebSocketConfig :: WebSocketConfig
testWebSocketConfig =
  WebSocketConfig
    { url = "ws://provider.test/api/deploy-jobs/ws?node=node-a"
    , nodeName = "node-a"
    , authToken = "secret-token"
    , reconnectSeconds = 7
    , buildSessionId = pure "session-node-a-2026-04-03T13-00-00Z"
    , buildResumePayload = pure (buildWsResumePayload "node-a" (resumePayload (emptyStateFor "node-a")) "msg-resume-1" "2026-04-03T13:00:00Z")
    , onAuthenticated = \_ _ -> pure ()
    , onSessionActivity = \_ -> pure ()
    }

testAgentConfig :: AgentConfig
testAgentConfig =
  AgentConfig
    { providerApiBaseUrl = "https://provider.test"
    , nodeAuthTokenFile = "/tmp/hostenv-deploy-agent-token"
    , nodeName = "node-a"
    , stateFile = "/tmp/hostenv-deploy-agent-state.json"
    , actionTimeoutSeconds = 30
    , reconnectSeconds = 7
    , eventStderrMaxLines = defaultEventStderrMaxLines
    }

testPreparedRuntime :: PreparedRuntime
testPreparedRuntime =
  PreparedRuntime
    { webSocket = testWebSocketConfig
    , state = emptyStateFor "node-a"
    }

testExecutionDeps :: ExecutionDeps
testExecutionDeps =
  ExecutionDeps
    { logger = testLogger
    , emitEvent = \_ _ -> pure ()
    , fetchSnapshot = \_ -> pure (Left SnapshotMissing)
    , resolveUser = \_ -> pure Nothing
    , ensureUserSession = \_ -> pure Nothing
    , readState = pure (emptyStateFor "node-a")
    , writeState = \_ -> pure ()
    , processRunner = \_ -> pure ProcessResult { exitCode = 0, stdoutText = "", stderrText = "" }
    , pathExists = \_ -> pure False
    , listDirectory = \_ -> pure []
    , currentTime = pure (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0))
    }

withTempStateFile :: (FilePath -> IO a) -> IO a
withTempStateFile action = do
  tempDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tempDir "hostenv-deploy-agent-state.json"
  hClose handle
  result <- action path
  removeFile path
  pure result

mkExecutionDeps :: IORef AgentState -> IORef [Text] -> IORef [EventStatus] -> (IORef [EventStatus] -> Text -> EventStatus -> IO ()) -> ExecutionDeps
mkExecutionDeps stateRef commandsRef eventsRef emitEventImpl =
  ExecutionDeps
    { logger = testLogger
    , emitEvent = \_ NodeEvent { phase = mPhase, status = statusValue } -> emitEventImpl eventsRef (maybe "" id mPhase) statusValue
    , fetchSnapshot = \_ -> pure (Right emptySnapshotDocument)
    , resolveUser = \userName -> pure (Just UserIdentity { name = userName, uid = 42, home = "/srv/alice" })
    , ensureUserSession = \_ -> pure Nothing
    , readState = readIORef stateRef
    , writeState = writeIORef stateRef
    , processRunner = \spec -> do
        modifyIORef' commandsRef (<> [spec.description])
        pure ProcessResult { exitCode = 0, stdoutText = "", stderrText = "" }
    , pathExists = \_ -> pure True
    , listDirectory = \_ -> pure []
    , currentTime = pure (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0))
    }

recordEventOnly :: IORef [EventStatus] -> Text -> EventStatus -> IO ()
recordEventOnly eventsRef _phase statusValue =
  modifyIORef' eventsRef (<> [statusValue])

crashOnActionSuccess :: IORef [EventStatus] -> Text -> EventStatus -> IO ()
crashOnActionSuccess eventsRef phaseValue statusValue = do
  modifyIORef' eventsRef (<> [statusValue])
  if phaseValue == "deactivate" && statusValue == EventSuccess
    then throwIO TestForcedClose
    else pure ()

sideEffectAction :: DeployAction
sideEffectAction = mkAction "action-deactivate-alice" Deactivate "alice" []

sideEffectJob :: DeployJob
sideEffectJob = DeployJob "job-side-effect" "dispatch-side-effect" "abc123" Nothing (DeployIntent 1 Nothing Nothing [sideEffectAction])

emptySnapshotDocument :: SnapshotDocument
emptySnapshotDocument =
  let payload = A.object ["snapshots" A..= A.object []]
   in SnapshotDocument
        { snapshotMetadata = buildSnapshotMetadata payload
        , snapshotPayload = payload
        , snapshotBytes = A.encode payload
        }

testCapturedLogger :: IORef [BLC.ByteString] -> Logger
testCapturedLogger sinkRef =
  mkJsonLogger (pure (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0))) (writeIORefAppend sinkRef)

findLogEntry :: IORef [BLC.ByteString] -> Text -> IO A.Value
findLogEntry sinkRef eventName = do
  entries <- readIORef sinkRef
  let decoded = mapMaybeDecode entries
  case filter ((== Just eventName) . lookupTextFieldValue "event") decoded of
    entry : _ -> pure entry
    [] -> assert False ("expected log entry for event " <> show eventName) >> pure A.Null

mapMaybeDecode :: [BLC.ByteString] -> [A.Value]
mapMaybeDecode = foldr decodeOne []
  where
    decodeOne raw acc =
      case A.decode raw of
        Just value -> value : acc
        Nothing -> acc

lookupTextFieldValue :: Text -> A.Value -> Maybe Text
lookupTextFieldValue fieldName value =
  case value of
    A.Object obj ->
      case KM.lookup (K.fromText fieldName) obj of
        Just (A.String raw) -> Just raw
        _ -> Nothing
    _ -> Nothing

lookupIntField :: Text -> A.Value -> Maybe Int
lookupIntField fieldName value =
  case value of
    A.Object obj ->
      case KM.lookup (K.fromText fieldName) obj of
        Just (A.Number raw) -> Just (round raw)
        _ -> Nothing
    _ -> Nothing
