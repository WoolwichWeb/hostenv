module Hostenv.Provider.DeployAgent.Dispatcher
  ( DispatchDecision(..)
  , IncomingMessage(..)
  , SessionRuntime(..)
  , decideDispatch
  , runIncomingDispatcher
  ) where

import Control.Concurrent.STM (TQueue, atomically, readTQueue)
import Control.Monad (forever)
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import Hostenv.Provider.DeployAgent.Logging (Logger, logDebug, logInfo)
import Hostenv.Provider.DeployAgent.Protocol (DeployJob(..))
import Hostenv.Provider.DeployAgent.Protocol (WsEnvelope(..), WsMessageKind(..), renderWsMessageKind)
import Hostenv.Provider.DeployAgent.State (AgentState(..))

data DispatchDecision
  = DispatchHandle DeployJob
  deriving (Eq, Show)

data IncomingMessage
  = IncomingDeployJob WsEnvelope DeployJob
  | IncomingEnvelope WsEnvelope
  deriving (Eq, Show)

data SessionRuntime = SessionRuntime
  { sessionId :: Text
  , sendJson :: A.Value -> IO ()
  , sendResume :: IO ()
  , markAuthenticated :: WsEnvelope -> IO ()
  }

decideDispatch :: AgentState -> DeployJob -> DispatchDecision
decideDispatch _state job = DispatchHandle job

runIncomingDispatcher :: Logger -> TQueue IncomingMessage -> SessionRuntime -> (SessionRuntime -> DeployJob -> IO ()) -> IO ()
runIncomingDispatcher logger incomingQueue sessionRuntime onJob =
  forever do
    incoming <- atomically (readTQueue incomingQueue)
    case incoming of
      IncomingDeployJob envelope job -> do
        logInfo logger "transport.websocket" "deploy_job_received"
          [ "session_id" A..= sessionRuntime.sessionId
          , "message_id" A..= envelope.messageId
          , "job_id" A..= job.jobId
          , "dispatch_id" A..= job.dispatchId
          , "commit_sha" A..= job.commitSha
          ]
        onJob sessionRuntime job
      IncomingEnvelope envelope ->
        case envelope.kind of
          WsAuthOk ->
            do
              sessionRuntime.markAuthenticated envelope
              logInfo logger "transport.websocket" "session_authenticated"
                [ "session_id" A..= sessionRuntime.sessionId
                , "message_id" A..= envelope.messageId
                ]
              sessionRuntime.sendResume
          WsAuthError ->
            fail (T.unpack ("websocket auth rejected: " <> renderPayload envelope.payload))
          WsError ->
            fail (T.unpack ("websocket session received error envelope: " <> renderPayload envelope.payload))
          _ ->
            logDebug logger "transport.websocket" "websocket_message_ignored"
              [ "session_id" A..= sessionRuntime.sessionId
              , "message_id" A..= envelope.messageId
              , "kind" A..= renderWsMessageKind envelope.kind
              ]

renderPayload :: A.Value -> Text
renderPayload = T.pack . show
