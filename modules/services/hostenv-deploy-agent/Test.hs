import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Hostenv.Provider.DeployAgent.Dispatcher (DispatchDecision(..), decideDispatch)
import Hostenv.Provider.DeployAgent.Executor.Classify
  ( ActionStep(..)
  , FailureClassification(..)
  , SystemStep(..)
  , buildCommandPayload
  , classifyActionFailure
  , classifySystemFailure
  )
import Hostenv.Provider.DeployAgent.Executor.Process (ProcessSpec(..))
import Hostenv.Provider.DeployAgent.Executor (SnapshotRequest(..))
import Hostenv.Provider.DeployAgent.Logging (logInfo, mkJsonLogger)
import Hostenv.Provider.DeployAgent.ProviderApi (snapshotUrl)
import Hostenv.Provider.DeployAgent.Protocol
  ( ActionOp(..)
  , DeployAction(..)
  , DeployIntent(..)
  , DeployJob(..)
  , EventStatus(..)
  , actionStorePath
  , buildWsAuthPayload
  , decodeDeployJobMessage
  , jobSignature
  )
import Hostenv.Provider.DeployAgent.State
  ( AgentState(..)
  , AppliedStorePath(..)
  , emptyState
  , finalizeJobState
  )
import Hostenv.Provider.DeployAgent.Transport.WebSocket (IncomingMessage(..), decodeIncomingMessage, deriveWebSocketUrl)
import Hostenv.Provider.DeployAgent.UserSession (UserIdentity(..), buildUserSession, runUserSpec)
import System.Exit (exitFailure)

assert :: Bool -> String -> IO ()
assert cond msg =
  if cond then pure () else putStrLn ("FAIL: " <> msg) >> exitFailure

main :: IO ()
main = do
  testWebSocketAuthPayload
  testDecodeDeployJobMessage
  testDecodeIncomingWebSocketMessage
  testJobSignature
  testStateCompatibility
  testDispatcher
  testSystemFailureClassification
  testActionFailureClassification
  testCommandPayloadShape
  testSnapshotUrl
  testUserSessionRunuserEnv
  testStructuredLogging
  putStrLn "ok"

testWebSocketAuthPayload :: IO ()
testWebSocketAuthPayload = do
  let payload = buildWsAuthPayload "node-a" "secret-token"
  assert (payload == A.object ["type" A..= ("auth" :: Text), "node" A..= ("node-a" :: Text), "token" A..= ("secret-token" :: Text)]) "ws auth payload should match hostenv-deploy-agent contract"
  assert (deriveWebSocketUrl "https://provider.test" "node-a" == Right "wss://provider.test/api/deploy-jobs/ws?node=node-a") "https api base should derive wss url"

testDecodeDeployJobMessage :: IO ()
testDecodeDeployJobMessage = do
  let raw =
        BLC.pack
          "{\"type\":\"deploy_job\",\"jobId\":\"job-1\",\"commitSha\":\"abc123\",\"intent\":{\"schemaVersion\":1,\"systemToplevel\":\"/nix/store/system\",\"actions\":[{\"op\":\"activate\",\"user\":\"alice\",\"envStorePath\":\"/nix/store/user\"}]}}"
  case decodeDeployJobMessage raw of
    Nothing -> assert False "deploy job websocket payload should decode"
    Just job -> do
      assert (job.jobId == "job-1") "decoded job should keep job id"
      case job.intent.actions of
        [action] -> assert (actionStorePath action == "/nix/store/user") "action store path should fall back to envStorePath"
        _ -> assert False "decoded job should contain exactly one action"

testDecodeIncomingWebSocketMessage :: IO ()
testDecodeIncomingWebSocketMessage = do
  let raw =
        BLC.pack
          "{\"type\":\"deploy_job\",\"jobId\":\"job-1\",\"commitSha\":\"abc123\",\"intent\":{\"schemaVersion\":1,\"actions\":[{\"op\":\"activate\",\"user\":\"alice\",\"storePath\":\"/nix/store/user\"}]}}"
  case decodeIncomingMessage raw of
    Just (IncomingDeployJob DeployJob { jobId = jobIdValue }) -> assert (jobIdValue == "job-1") "transport should surface deploy_job payloads"
    _ -> assert False "transport should decode deploy_job websocket messages"

testJobSignature :: IO ()
testJobSignature = do
  let action = mkAction Activate "alice" ["storePath" A..= ("/nix/store/user" :: Text)]
      intent = DeployIntent 1 (Just "/nix/store/system") Nothing [action]
      job = DeployJob "job-1" "abc123" Nothing intent
      signature = jobSignature job
  assert (T.length signature == 64) "job signature should be sha256 hex"

testStateCompatibility :: IO ()
testStateCompatibility = do
  let raw = BLC.pack "{\"lastAppliedJobId\":\"\",\"lastAppliedSignature\":\"\",\"lastCommitSha\":\"\",\"system\":{},\"users\":{},\"updatedAt\":\"\"}"
      decoded = A.decode raw :: Maybe AgentState
  case decoded of
    Nothing -> assert False "state.json compatibility payload should decode"
    Just state -> do
      assert (state.system == Nothing) "empty system object should decode as Nothing"
      let action = mkAction Reload "alice" ["envStorePath" A..= ("/nix/store/user" :: Text)]
          job = DeployJob "job-2" "def456" Nothing (DeployIntent 1 Nothing Nothing [action])
          finalState = finalizeJobState "2026-01-01T00:00:00Z" job "sig" emptyState
      assert (finalState.lastAppliedJobId == "job-2") "finalized state should store job id"
      assert (maybe False matchesStorePath (Map.lookup "alice" finalState.users)) "finalized state should record user store path"

testDispatcher :: IO ()
testDispatcher = do
  let action = mkAction Activate "alice" ["storePath" A..= ("/nix/store/user" :: Text)]
      job = DeployJob "job-1" "abc123" Nothing (DeployIntent 1 Nothing Nothing [action])
      duplicateState = emptyState { lastAppliedSignature = jobSignature job }
  assert (decideDispatch duplicateState job == DispatchSkipDuplicate) "dispatcher should skip duplicate signatures"
  case decideDispatch emptyState job of
    DispatchHandle _ _ -> pure ()
    _ -> assert False "dispatcher should handle unseen signatures"

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
      FailureClassification { status = timedOutStatus } = timedOut
      FailureClassification { message = missingMessage } = missingExec
  assert (timedOutStatus == EventTimedOut) "timeout should map to timed_out status"
  assert (missingMessage == ("Action executable not found during resolve-exec-activate-alice" :: Text)) "missing executable classification should preserve step label"

testCommandPayloadShape :: IO ()
testCommandPayloadShape = do
  let payload = buildCommandPayload 17 "trimmed stderr" (A.object ["step" A..= ("system-switch" :: Text)])
  assert (payload == A.object ["exitCode" A..= (17 :: Int), "stderrSummary" A..= ("trimmed stderr" :: Text), "step" A..= ("system-switch" :: Text)]) "command payload should merge extra object fields"

testSnapshotUrl :: IO ()
testSnapshotUrl = do
  let request = SnapshotRequest { jobId = "job-1", sourceNode = "node a", user = "alice@example.com" }
      url = snapshotUrl "https://provider.test" "node-b" request
  assert (url == "https://provider.test/api/deploy-jobs/job-1/backup-snapshot?node=node-b&source=node%20a&user=alice%40example.com") "snapshot URL should match provider backup-snapshot contract"

testUserSessionRunuserEnv :: IO ()
testUserSessionRunuserEnv = do
  let session = buildUserSession UserIdentity { name = "alice", uid = 42, home = "/srv/alice" }
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
  assert ("XDG_RUNTIME_DIR=/run/user/42" `elem` wrapped.arguments) "runuser environment should inject runtime dir"
  assert ("DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/42/bus" `elem` wrapped.arguments) "runuser environment should inject dbus address"

testStructuredLogging :: IO ()
testStructuredLogging = do
  sinkRef <- newIORef ([] :: [BLC.ByteString])
  let now = pure (UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0))
      logger = mkJsonLogger now (writeIORefAppend sinkRef)
  logInfo logger "executor" "running" ["jobId" A..= ("job-1" :: Text)]
  entries <- readIORef sinkRef
  case entries of
    [entry] ->
      case A.decode entry of
        Nothing -> assert False "structured log line should decode as json"
        Just value ->
          assert (value == expectedLogValue) "structured log entry should remain json-oriented"
    _ -> assert False "logger should emit exactly one entry"

expectedLogValue :: A.Value
expectedLogValue =
  A.object
    [ "timestamp" A..= ("2026-01-01T00:00:00Z" :: Text)
    , "level" A..= ("info" :: Text)
    , "component" A..= ("executor" :: Text)
    , "message" A..= ("running" :: Text)
    , "fields" A..= A.object ["jobId" A..= ("job-1" :: Text)]
    ]

mkAction :: ActionOp -> Text -> [Pair] -> DeployAction
mkAction op user fields =
  DeployAction
    { op
    , user
    , storePath = lookupTextField "storePath" fields
    , envStorePath = lookupTextField "envStorePath" fields
    , path = Nothing
    , fromNode = Nothing
    , toNode = Nothing
    , migrations = []
    , rawAction = A.object (["op" A..= op, "user" A..= user] <> fields)
    }

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

matchesStorePath :: AppliedStorePath -> Bool
matchesStorePath AppliedStorePath { storePath = appliedStorePath } = appliedStorePath == "/nix/store/user"
