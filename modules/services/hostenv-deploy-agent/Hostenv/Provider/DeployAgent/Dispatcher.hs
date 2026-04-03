module Hostenv.Provider.DeployAgent.Dispatcher
  ( DispatchDecision(..)
  , decideDispatch
  ) where

import Data.Text (Text)
import Hostenv.Provider.DeployAgent.Protocol (DeployJob, jobSignature)
import Hostenv.Provider.DeployAgent.State (AgentState(..))

data DispatchDecision
  = DispatchSkipDuplicate
  | DispatchHandle Text DeployJob
  deriving (Eq, Show)

decideDispatch :: AgentState -> DeployJob -> DispatchDecision
decideDispatch state job =
  let signature = jobSignature job
   in if signature == state.lastAppliedSignature
        then DispatchSkipDuplicate
        else DispatchHandle signature job
