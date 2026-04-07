module Hostenv.Provider.DeployAgent.Executor.Classify
  ( ActionStep(..)
  , FailureClassification(..)
  , SystemStep(..)
  , actionStepLabel
  , buildCommandPayload
  , classifyActionFailure
  , classifySystemFailure
  , stepLabel
  , systemStepLabel
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Hostenv.Provider.DeployAgent.Protocol (ActionOp, EventStatus(..), renderActionOp)

data SystemStep = SystemRealise | SystemSet | SystemSwitch
  deriving (Eq, Show)

data ActionStep
  = ValidateAction ActionOp Text
  | RealiseAction ActionOp Text
  | SetProfile Text
  | ResolveExecutable ActionOp Text
  | FetchSnapshot ActionOp Text
  | RunAction ActionOp Text
  | VerifyAction ActionOp Text
  deriving (Eq, Show)

data FailureClassification = FailureClassification
  { status :: EventStatus
  , phase :: Text
  , message :: Text
  , payload :: A.Value
  }
  deriving (Eq, Show)

buildCommandPayload :: Int -> Text -> A.Value -> A.Value
buildCommandPayload exitCode stderrSummary extra =
  case extra of
    A.Object extraFields ->
      A.Object
        ( KM.insert "stderrSummary" (A.String stderrSummary)
            (KM.insert "exitCode" (A.toJSON exitCode) extraFields)
        )
    _ ->
      A.object
        [ "exitCode" A..= exitCode
        , "stderrSummary" A..= stderrSummary
        , "details" A..= extra
        ]

classifySystemFailure :: SystemStep -> Int -> FailureClassification
classifySystemFailure step exitCode
  | exitCode `elem` [98, 99] =
      FailureClassification
        { status = EventFailed
        , phase = "intent"
        , message = "Superseded by newer job"
        , payload = A.object ["reason" A..= ("superseded" :: Text), "step" A..= systemStepLabel step]
        }
  | otherwise =
      FailureClassification
        { status = EventFailed
        , phase = "system"
        , message =
            case step of
              SystemRealise -> "Failed to realise system path"
              SystemSet -> "Failed to set system profile"
              SystemSwitch -> "System switch failed"
        , payload = A.object ["step" A..= systemStepLabel step]
        }

classifyActionFailure :: ActionOp -> ActionStep -> Int -> FailureClassification
classifyActionFailure op step exitCode
  | exitCode `elem` [98, 99] =
      FailureClassification
        { status = EventFailed
        , phase = "intent"
        , message = "Superseded by newer job during " <> label
        , payload = A.object ["reason" A..= ("superseded" :: Text), "op" A..= opName, "step" A..= label]
        }
  | exitCode == 124 =
      FailureClassification
        { status = EventTimedOut
        , phase = opName
        , message = "Action timed out during " <> label
        , payload = A.object ["op" A..= opName, "step" A..= label]
        }
  | exitCode == 4 =
      FailureClassification
        { status = EventFailed
        , phase = opName
        , message = "Restore snapshot unavailable during " <> label
        , payload = A.object ["op" A..= opName, "step" A..= label]
        }
  | exitCode == 3 =
      FailureClassification
        { status = EventFailed
        , phase = opName
        , message = "Action executable not found during " <> label
        , payload = A.object ["op" A..= opName, "step" A..= label]
        }
  | otherwise =
      FailureClassification
        { status = EventFailed
        , phase = opName
        , message = "Action failed during " <> label
        , payload = A.object ["op" A..= opName, "step" A..= label]
        }
  where
    opName = renderActionOp op
    label = actionStepLabel step

systemStepLabel :: SystemStep -> Text
systemStepLabel SystemRealise = "system-realise"
systemStepLabel SystemSet = "system-set"
systemStepLabel SystemSwitch = "system-switch"

actionStepLabel :: ActionStep -> Text
actionStepLabel = \case
  ValidateAction op user -> "validate-" <> renderActionOp op <> "-" <> user
  RealiseAction op user -> "realise-" <> renderActionOp op <> "-" <> user
  SetProfile user -> "set-profile-" <> user
  ResolveExecutable op user -> "resolve-exec-" <> renderActionOp op <> "-" <> user
  FetchSnapshot op user -> "fetch-snapshot-" <> renderActionOp op <> "-" <> user
  RunAction op user -> renderActionOp op <> "-" <> user
  VerifyAction op user -> "verify-" <> renderActionOp op <> "-" <> user

stepLabel :: Either SystemStep ActionStep -> Text
stepLabel = \case
  Left step -> systemStepLabel step
  Right step -> actionStepLabel step
