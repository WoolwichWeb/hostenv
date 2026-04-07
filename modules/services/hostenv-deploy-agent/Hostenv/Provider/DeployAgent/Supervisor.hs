module Hostenv.Provider.DeployAgent.Supervisor
  ( PreparedRuntime(..)
  , SupervisorHooks(..)
  , runBootstrap
  , runSupervisorLoopWith
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Hostenv.Provider.DeployAgent.Config (AgentConfig(..), readNodeToken)
import Hostenv.Provider.DeployAgent.Dispatcher (SessionRuntime(..))
import Hostenv.Provider.DeployAgent.Executor
  ( ExecutionDeps(..)
  , JobResult(..)
  , executeJob
  )
import Hostenv.Provider.DeployAgent.Executor.Classify (FailureClassification(..))
import Hostenv.Provider.DeployAgent.Executor.Process
  ( ProcessResult(..)
  , runProcessIO
  )
import Hostenv.Provider.DeployAgent.Logging (Logger, logDebug, logInfo, logWarn, mkJsonLogger)
import qualified Hostenv.Provider.DeployAgent.ProviderApi as ProviderApi
import Hostenv.Provider.DeployAgent.Protocol (DeployJob(..), EventStatus(..), NodeEvent(..), WsEnvelope(..), buildWsNodeEventEnvelope, eventStatusText)
import Hostenv.Provider.DeployAgent.State (AgentState(..), ensureStateFile, loadStateFile, writeStateFile)
import Hostenv.Provider.DeployAgent.Systemd (SystemdRuntime(..), UserManagerBootstrap(..), bootstrapUserManager, mkSystemdRuntime)
import Hostenv.Provider.DeployAgent.Transport.WebSocket (WebSocketConfig(..), buildWebSocketConfig, runWebSocketSession)
import Hostenv.Provider.DeployAgent.UserSession (UserIdentity(..), UserSession(..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (doesPathExist, listDirectory)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError)
import System.Posix.Files (getFileStatus, isSocket)
import qualified System.Posix.User as PosixUser

data PreparedRuntime = PreparedRuntime
  { webSocket :: WebSocketConfig
  , state :: AgentState
  }

data SupervisorHooks = SupervisorHooks
  { runSession :: (SessionRuntime -> DeployJob -> IO ()) -> IO ()
  , sleepBeforeReconnect :: Int -> IO ()
  , shouldReconnect :: Int -> Either SomeException () -> IO Bool
  }

runBootstrap :: AgentConfig -> IO ()
runBootstrap cfg = do
  logger <- pure (stderrLogger getCurrentTime)
  systemdRuntime <- mkSystemdRuntime logger
  token <- readNodeToken cfg
  ensureStateFile cfg.nodeName cfg.stateFile
  initialState <- loadStateFile cfg.nodeName cfg.stateFile
  stateRef <- newIORef initialState
  case buildWebSocketConfig cfg token (readIORef stateRef) (\sessionId WsEnvelope { messageId = messageIdValue } -> systemdRuntime.signalReady sessionId messageIdValue) systemdRuntime.noteActivity of
    Left err -> do
      hPutStrLn stderr (T.unpack err)
      exitFailure
    Right wsConfig -> do
      manager <- newManager tlsManagerSettings
      let providerApi = ProviderApi.buildProviderApi logger manager cfg token
          runtime = PreparedRuntime { webSocket = wsConfig, state = initialState }
          deps = buildExecutionDeps logger cfg stateRef providerApi
          AgentState { updatedAt = stateUpdatedAt } = initialState
      logInfo logger "supervisor" "runtime_prepared"
        [ "websocket_url" A..= wsConfig.url
        , "node" A..= cfg.nodeName
        , "state_file" A..= cfg.stateFile
        , "state_updated_at" A..= stateUpdatedAt
        ]
      systemdRuntime.withWatchdogSupport (runSupervisorLoop logger deps cfg runtime)

buildExecutionDeps :: Logger -> AgentConfig -> IORef AgentState -> ProviderApi.ProviderApi -> ExecutionDeps
buildExecutionDeps logger cfg stateRef providerApi =
  ExecutionDeps
    { logger
    , emitEvent = \_ _ -> pure ()
    , fetchSnapshot = providerApi.fetchSnapshot
    , resolveUser = resolveUserIdentity
    , ensureUserSession = ensureUserSessionReady logger
    , readState = readIORef stateRef
    , writeState = persistState stateRef cfg.stateFile
    , processRunner = runProcessIO
    , pathExists = doesPathExist
    , listDirectory = listDirectory
    , currentTime = getCurrentTime
    }

runSupervisorLoop :: Logger -> ExecutionDeps -> AgentConfig -> PreparedRuntime -> IO ()
runSupervisorLoop logger deps cfg runtime =
  runSupervisorLoopWith logger deps cfg runtime (defaultSupervisorHooks logger runtime)

runSupervisorLoopWith :: Logger -> ExecutionDeps -> AgentConfig -> PreparedRuntime -> SupervisorHooks -> IO ()
runSupervisorLoopWith logger deps cfg _runtime hooks = go 1
  where
    go attempt = do
      result <- try (hooks.runSession handleJob) :: IO (Either SomeException ())
      logSessionEnd result
      reconnect <- hooks.shouldReconnect attempt result
      when reconnect do
        logInfo logger "supervisor" "session_reconnect_scheduled"
          [ "attempt" A..= attempt
          , "backoff_seconds" A..= cfg.reconnectSeconds
          ]
        hooks.sleepBeforeReconnect cfg.reconnectSeconds
        go (attempt + 1)

    handleJob sessionRuntime job = do
      let DeployJob { jobId = jobIdValue } = job
      let sessionDeps =
            deps
              { emitEvent = \eventJobId event -> do
                  deps.emitEvent eventJobId event
                  emitEventOverSession logger deps.currentTime sessionRuntime job eventJobId event
              }
      outcome <- executeJob sessionDeps cfg job
      case outcome of
        JobSucceeded _ ->
          logInfo logger "supervisor" "deploy_job_finished"
            [ "job_id" A..= jobIdValue
            , "dispatch_id" A..= job.dispatchId
            , "result" A..= ("success" :: T.Text)
            ]
        JobSkipped reason ->
          logInfo logger "supervisor" "deploy_job_skipped"
            [ "job_id" A..= jobIdValue
            , "dispatch_id" A..= job.dispatchId
            , "reason" A..= reason
            ]
        JobFailed FailureClassification { phase = failurePhase, message = failureMessage } ->
          logWarn logger "supervisor" "deploy_job_failed"
            [ "job_id" A..= jobIdValue
            , "dispatch_id" A..= job.dispatchId
            , "phase" A..= failurePhase
            , "error_class" A..= failurePhase
            , "failure_message" A..= failureMessage
            ]

    logSessionEnd result =
      case result of
        Left err ->
          logWarn logger "supervisor" "session_ended"
            [ "reason" A..= show err
            , "backoff_seconds" A..= cfg.reconnectSeconds
            , "error_class" A..= ("session_exception" :: T.Text)
            ]
        Right () ->
          logWarn logger "supervisor" "session_completed_unexpectedly"
            [ "backoff_seconds" A..= cfg.reconnectSeconds
            , "error_class" A..= ("unexpected_session_completion" :: T.Text)
            ]

defaultSupervisorHooks :: Logger -> PreparedRuntime -> SupervisorHooks
defaultSupervisorHooks logger runtime =
  SupervisorHooks
    { runSession = runWebSocketSession logger runtime.webSocket
    , sleepBeforeReconnect = \seconds -> threadDelay (seconds * 1000000)
    , shouldReconnect = \_ _ -> pure True
    }

emitEventOverSession :: Logger -> IO UTCTime -> SessionRuntime -> DeployJob -> T.Text -> NodeEvent -> IO ()
emitEventOverSession logger nowIO sessionRuntime job _jobId event = do
  let SessionRuntime { sessionId = sessionIdValue, sendJson = sendJsonValue } = sessionRuntime
  now <- nowIO
  let timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
      messageId = buildEventMessageId job event timestamp
      envelope = buildWsNodeEventEnvelope event.node job.jobId job.dispatchId messageId timestamp event
  logDebug logger "transport.websocket" "deploy_event_enqueued"
    [ "session_id" A..= sessionIdValue
    , "message_id" A..= messageId
    , "job_id" A..= job.jobId
    , "dispatch_id" A..= job.dispatchId
    , "action_id" A..= event.actionId
    , "kind" A..= eventKindText event
    , "phase" A..= event.phase
    , "status" A..= eventStatusText event.status
    ]
  sendJsonValue envelope

buildEventMessageId :: DeployJob -> NodeEvent -> T.Text -> T.Text
buildEventMessageId job event timestamp =
  T.map normalize (eventKindText event <> ":" <> event.node <> ":" <> job.jobId <> ":" <> timestamp)
  where
    normalize ':' = '-'
    normalize c = c

eventKindText :: NodeEvent -> T.Text
eventKindText event =
  case event.status of
    EventRunning -> "progress"
    EventQueued -> "progress"
    EventWaiting -> "progress"
    EventSuccess -> "action_result"
    EventFailed -> "action_result"
    EventTimedOut -> "action_result"

persistState :: IORef AgentState -> FilePath -> AgentState -> IO ()
persistState stateRef path state = do
  writeIORef stateRef state
  writeStateFile path state

resolveUserIdentity :: T.Text -> IO (Maybe UserIdentity)
resolveUserIdentity userName = do
  mEntry <-
    (Just <$> PosixUser.getUserEntryForName (T.unpack userName))
      `catchIOError` \_ -> pure Nothing
  pure $ case mEntry of
      Nothing -> Nothing
      Just entry ->
        Just
          UserIdentity
            { name = userName
            , uid = fromIntegral (PosixUser.userID entry)
            , home = PosixUser.homeDirectory entry
            }

ensureUserSessionReady :: Logger -> UserSession -> IO (Maybe ProcessResult)
ensureUserSessionReady logger session = do
  let UserManagerBootstrap { startCommand = startCommandSpec, socketPaths = socketPaths, maxAttempts = maxAttempts, retryDelaySeconds = retryDelaySeconds } = bootstrapUserManager session
      UserSession { identity = UserIdentity { name = userName }, runtimeDir = runtimeDirValue } = session
  startResult <- runProcessIO startCommandSpec
  if startResult.exitCode /= 0
    then pure (Just startResult)
    else waitForSockets socketPaths maxAttempts retryDelaySeconds userName runtimeDirValue
  where
    waitForSockets socketPaths attemptsRemaining retryDelaySeconds userName runtimeDirValue = do
      socketReady <- anySocket socketPaths
      if socketReady
        then do
          logInfo logger "supervisor" "user_manager_ready"
            [ "user" A..= userName
            , "runtime_dir" A..= runtimeDirValue
            ]
          pure Nothing
        else
          if attemptsRemaining <= 0
            then
              pure
                ( Just
                    ProcessResult
                      { exitCode = 1
                      , stdoutText = ""
                       , stderrText = "hostenv-deploy-agent: user manager sockets did not become ready"
                      }
                )
            else do
              threadDelay (1000000 * retryDelaySeconds)
              waitForSockets socketPaths (attemptsRemaining - 1) retryDelaySeconds userName runtimeDirValue

anySocket :: [FilePath] -> IO Bool
anySocket [] = pure False
anySocket (path : rest) = do
  isReady <- isSocketPath path
  if isReady then pure True else anySocket rest

isSocketPath :: FilePath -> IO Bool
isSocketPath path = do
  result <- try (isSocket <$> getFileStatus path) :: IO (Either IOException Bool)
  pure (either (const False) id result)

stderrLogger :: IO UTCTime -> Logger
stderrLogger now = mkJsonLogger now (BLC.hPutStr stderr)
