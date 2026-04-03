module Hostenv.Provider.DeployAgent.Supervisor
  ( PreparedRuntime(..)
  , runBootstrap
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, try)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T
import Hostenv.Provider.DeployAgent.Config (AgentConfig(..), readNodeToken)
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
import Hostenv.Provider.DeployAgent.Logging (Logger, logInfo, logWarn, mkJsonLogger)
import qualified Hostenv.Provider.DeployAgent.ProviderApi as ProviderApi
import Hostenv.Provider.DeployAgent.Protocol (DeployJob(..))
import Hostenv.Provider.DeployAgent.State (AgentState(..), ensureStateFile, loadStateFile, writeStateFile)
import Hostenv.Provider.DeployAgent.Systemd (UserManagerBootstrap(..), bootstrapUserManager)
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
  deriving (Eq, Show)

runBootstrap :: AgentConfig -> IO ()
runBootstrap cfg = do
  logger <- pure (stderrLogger getCurrentTime)
  token <- readNodeToken cfg
  ensureStateFile cfg.stateFile
  initialState <- loadStateFile cfg.stateFile
  case buildWebSocketConfig cfg token of
    Left err -> do
      hPutStrLn stderr (T.unpack err)
      exitFailure
    Right wsConfig -> do
      manager <- newManager tlsManagerSettings
      stateRef <- newIORef initialState
      let providerApi = ProviderApi.buildProviderApi logger manager cfg token
          runtime = PreparedRuntime { webSocket = wsConfig, state = initialState }
          deps = buildExecutionDeps logger cfg stateRef providerApi
          AgentState { updatedAt = stateUpdatedAt } = initialState
      logInfo logger "supervisor" "provider deploy agent runtime prepared" ["websocketUrl" A..= wsConfig.url, "node" A..= cfg.nodeName, "stateFile" A..= cfg.stateFile, "stateUpdatedAt" A..= stateUpdatedAt]
      runSupervisorLoop logger deps cfg runtime

buildExecutionDeps :: Logger -> AgentConfig -> IORef AgentState -> ProviderApi.ProviderApi -> ExecutionDeps
buildExecutionDeps logger cfg stateRef providerApi =
  ExecutionDeps
    { logger
    , emitEvent = providerApi.postEvent
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
runSupervisorLoop logger deps cfg runtime = do
  result <- try (runWebSocketSession logger runtime.webSocket handleJob) :: IO (Either SomeException ())
  case result of
    Left err ->
      logWarn logger "supervisor" "websocket session ended" ["reason" A..= show err, "reconnectSeconds" A..= cfg.reconnectSeconds]
    Right () ->
      logWarn logger "supervisor" "websocket session completed unexpectedly" ["reconnectSeconds" A..= cfg.reconnectSeconds]
  threadDelay (cfg.reconnectSeconds * 1000000)
  runSupervisorLoop logger deps cfg runtime
  where
    handleJob job = do
      let DeployJob { jobId = jobIdValue } = job
      outcome <- executeJob deps cfg job
      case outcome of
        JobSucceeded _ ->
          logInfo logger "supervisor" "deploy job finished" ["jobId" A..= jobIdValue, "result" A..= ("success" :: T.Text)]
        JobSkipped reason ->
          logInfo logger "supervisor" "deploy job skipped" ["jobId" A..= jobIdValue, "reason" A..= reason]
        JobFailed FailureClassification { phase = failurePhase, message = failureMessage } ->
          logWarn logger "supervisor" "deploy job failed" ["jobId" A..= jobIdValue, "phase" A..= failurePhase, "message" A..= failureMessage]

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
          logInfo logger "supervisor" "user manager ready" ["user" A..= userName, "runtimeDir" A..= runtimeDirValue]
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
