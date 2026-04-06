{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import Control.Monad (unless)
import "crypton" Crypto.Hash (SHA256)
import "crypton" Crypto.MAC.HMAC (HMAC (..), hmac)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Time (UTCTime(..), addUTCTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)
import Network.HTTP.Types.Status (mkStatus)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Exit (exitFailure)
import System.IO (hClose, openTempFile)

import qualified Data.Map.Strict as Map
import Hostenv.Provider.Config (AppConfig(..), DeployConfig(..), loadConfig)
import Hostenv.Provider.Crypto
import Hostenv.Provider.DB (DeployAction(DeployAction), DeployBackupSnapshotLookup(..), DeployBackupSnapshotMetadata(..), OAuthCredential(..), buildDeployBackupSnapshotMetadata, classifyDeployBackupSnapshotRows, deployActionId, selectDeployActionIndex)
import Hostenv.Provider.DeployApi (NodeEvent, NodeEventIntakeDecision(..), ProjectedNodeEvent(..), acceptsNodeEvents, backupSnapshotResponseValue, buildDeployJobEnvelope, classifyProjectedNodeEvent, currentDispatchIdFor, dispatchFingerprint, dispatchForNode, dispatchIdForEventValidation, dispatchStableId, extractBearer, isValidDeployWsAuth, normalizeStatus, projectNodeEvent, shouldDispatchJob, validateIntent)
import Hostenv.Provider.Gitlab
  ( GitlabCredentialContext(..)
  , GitlabError(..)
  , NixGitlabTokenType(..)
  , GitlabTokenResponse(..)
  , appendNixAccessTokenConfig
  , gitlabApiError
  , isReauthError
  , oauthCredentialFromTokenAt
  , renderAccessDeniedMessage
  , renderGitlabError
  , renderUserIdMismatchMessage
  )
import Hostenv.Provider.Logging (ProviderLogFields(..), ProviderSeverity(..), providerLogFields, providerLogValue)
import Hostenv.Provider.PrevNodeDiscovery
import Hostenv.Provider.Repo (RepoStatus(..), ensureGitConfig, ensureProviderRepo, isAuthFailure)
import Hostenv.Provider.Service
import Hostenv.Provider.Server (wsNodeEventMatchesAuthenticatedNode)
import Hostenv.Provider.Webhook (chooseFinalResult, persistIntentsActionsAndPushWith, shouldWaitForCallbacks)

assert :: Bool -> String -> IO ()
assert cond msg = unless cond $ do
  putStrLn ("FAIL: " <> msg)
  exitFailure

testTimestamp :: UTCTime
testTimestamp = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

assertStructuredLogShape :: A.Value -> [(String, Maybe A.Value)] -> IO ()
assertStructuredLogShape value expectations =
  case value of
    A.Object obj -> do
      let requiredFields =
            [ "event"
            , "severity"
            , "request_id"
            , "job_id"
            , "node"
            , "dispatch_id"
            , "action_id"
            , "phase"
            , "status"
            , "protocol_version"
            , "decision"
            , "reason"
            ]
      assert (all (\fieldName -> KM.member (K.fromString fieldName) obj) requiredFields) "structured deploy logs should include all required stable fields"
      assert (not (KM.member (K.fromString "headers") obj)) "structured deploy logs should not contain raw header dumps"
      assert (not (KM.member (K.fromString "token") obj)) "structured deploy logs should not contain token fields"
      mapM_
        (\(fieldName, expectedValue) -> assert (KM.lookup (K.fromString fieldName) obj == expectedValue) ("unexpected structured log field: " <> fieldName))
        expectations
    _ -> assert False "structured deploy log should encode to an object"

main :: IO ()
main = do
  testGitHubSig
  testGitLabToken
  testDeployApiBearerParsing
  testDeployApiStatusNormalization
  testDeployApiEventAcceptance
  testDeployApiIntentValidation
  testDeployApiIntentValidationEdgeCases
  testStructuredWsAuthLogShape
  testStructuredDispatchLogShape
  testStructuredNodeEventLogShape
  testStructuredWebhookBoundaryLogShape
  testDeployWsAuthFinalShape
  testWsNodeEventAuthenticatedNodeBoundary
  testDispatchForNodeMigrationSequencing
  testDeployDeliveryIdentity
  testNodeEventProtocolRoundTrip
  testEventProjectionBehavior
  testProjectedNodeEventIntake
  testBackupSnapshotLookupMissing
  testBackupSnapshotLookupMalformed
  testBackupSnapshotLookupChecksumMismatch
  testBackupSnapshotResponseShape
  testWebsocketDispatchFingerprinting
  testDispatchIdentityUsesDispatchedSubset
  testActionlessIntentKeepsDispatchIdentity
  testDispatchValidationPrefersPersistedSentDispatch
  testFinalIntentSuccessAcceptsPersistedDispatch
  testPlanParsing
  testNodeOrderWithMigrations
  testNodeOrderWithDnsSkipsNonMigratingEnvDiscovery
  testProjectHashSelection
  testPrevNodeDiscoveryResolution
  testCommandSequence
  testTemplateRender
  testGitCredentials
  testGitConfigAuthorIdentity
  testNixAccessTokenConfig
  testReadGitlabSecrets
  testGitlabOAuthCredentialMerge
  testGitlabApiErrorFormatting
  testGitlabAccessMessages
  testGitlabReauthClassification
  testRepoAuthFailureClassifier
  testWebhookResultPriority
  testShouldWaitForCallbacks
  testPersistIntentsActionsAndPushOrder
  testTokenEncryptionRoundtrip
  testTokenKeyLoading
  testLoadConfigJobDefaults
  testEnsureProviderRepoMissing
  testEnsureProviderRepoInvalidDir
  testDeployIntentStablePayload
  testNoIntentWhenUnchanged
  testReloadIntentSameNodeChange
  testMoveIntentOrdering
  putStrLn "ok"


testGitHubSig :: IO ()
testGitHubSig = do
  let secret = "secret"
  let body = BL.fromStrict "payload"
  let digest = hmac secret (BL.toStrict body) :: HMAC SHA256
  let expected = BAE.convertToBase BAE.Base16 (BA.convert digest :: BS.ByteString)
  let header = "sha256=" <> expected
  assert (verifyGitHubSignature secret body header) "github signature should validate"
  assert (not (verifyGitHubSignature secret body "sha256=deadbeef")) "github signature should reject invalid"


testGitLabToken :: IO ()
testGitLabToken = do
  assert (verifyGitLabToken "token" "token") "gitlab token should validate"
  assert (not (verifyGitLabToken "token" "bad")) "gitlab token should reject invalid"

testDeployApiBearerParsing :: IO ()
testDeployApiBearerParsing = do
  assert (extractBearer Nothing == Nothing) "extractBearer should reject missing auth header"
  assert (extractBearer (Just "Bearer token123") == Just "token123") "extractBearer should parse bearer token"
  assert (extractBearer (Just " bearer token456 ") == Just "token456") "extractBearer should normalize case and whitespace"
  assert (extractBearer (Just "Basic abc123") == Nothing) "extractBearer should reject non-bearer auth"

testDeployApiStatusNormalization :: IO ()
testDeployApiStatusNormalization = do
  assert (normalizeStatus "RUNNING" == "running") "normalizeStatus should lowercase valid statuses"
  assert (normalizeStatus " success " == "success") "normalizeStatus should trim whitespace"
  assert (normalizeStatus "invalid-status" == "") "normalizeStatus should reject invalid statuses"

testDeployApiEventAcceptance :: IO ()
testDeployApiEventAcceptance = do
  assert (acceptsNodeEvents "waiting") "acceptsNodeEvents should allow waiting jobs"
  assert (acceptsNodeEvents " WAITING ") "acceptsNodeEvents should normalize case and whitespace"
  assert (not (acceptsNodeEvents "running")) "acceptsNodeEvents should reject non-waiting statuses"
  assert (not (acceptsNodeEvents "failed")) "acceptsNodeEvents should reject terminal statuses"

testDeployApiIntentValidation :: IO ()
testDeployApiIntentValidation = do
  let validIntent =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..= [A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("activate" :: T.Text)]]
          ]
      invalidSchema =
        A.object
          [ "schemaVersion" A..= (2 :: Int)
          , "actions" A..= [A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("activate" :: T.Text)]]
          ]
      invalidUser =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..= [A.object ["user" A..= ("" :: T.Text), "op" A..= ("activate" :: T.Text)]]
          ]
      invalidOp =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..= [A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("migrate" :: T.Text)]]
          ]
  assert (validateIntent validIntent == Just validIntent) "validateIntent should accept valid payload"
  assert (validateIntent invalidSchema == Nothing) "validateIntent should reject unknown schema versions"
  assert (validateIntent invalidUser == Nothing) "validateIntent should reject blank user names"
  assert (validateIntent invalidOp == Nothing) "validateIntent should reject unknown action ops"

testDeployApiIntentValidationEdgeCases :: IO ()
testDeployApiIntentValidationEdgeCases = do
  let validIntent =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..=
              [ A.object
                  [ "user" A..= ("alice" :: T.Text)
                  , "op" A..= ("restore" :: T.Text)
                  , "fromNode" A..= ("node-a" :: T.Text)
                  , "toNode" A..= ("node-b" :: T.Text)
                  , "migrations" A..= ["db-migrate" :: T.Text, "search.v2" :: T.Text]
                  ]
              ]
          ]
      invalidFromNode =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..= [A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("restore" :: T.Text), "fromNode" A..= ("Node-A" :: T.Text)]]
          ]
      invalidMigration =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..= [A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("restore" :: T.Text), "migrations" A..= [A.Number 1]]]
          ]
  assert (validateIntent validIntent == Just validIntent) "validateIntent should accept safe node and migration identifiers"
  assert (validateIntent invalidFromNode == Nothing) "validateIntent should reject unsafe fromNode values"
  assert (validateIntent invalidMigration == Nothing) "validateIntent should reject non-text migration entries"

testStructuredWsAuthLogShape :: IO ()
testStructuredWsAuthLogShape =
  assertStructuredLogShape
    ( providerLogValue
        testTimestamp
        ( (providerLogFields "deploy_ws_auth" ProviderSeverityInfo)
            { entryRequestId = Just "req-1"
            , entryNode = Just "node-a"
            , entryProtocolVersion = Just 1
            , entryDecision = Just "accept"
            , entryReason = Just "authenticated"
            }
        )
        []
    )
    [ ("event", Just (A.String "deploy_ws_auth"))
    , ("severity", Just (A.String "info"))
    , ("request_id", Just (A.String "req-1"))
    , ("node", Just (A.String "node-a"))
    , ("protocol_version", Just (A.Number 1))
    , ("decision", Just (A.String "accept"))
    , ("reason", Just (A.String "authenticated"))
    , ("job_id", Just A.Null)
    ]

testStructuredDispatchLogShape :: IO ()
testStructuredDispatchLogShape =
  assertStructuredLogShape
    ( providerLogValue
        testTimestamp
        ( (providerLogFields "deploy_dispatch" ProviderSeverityInfo)
            { entryRequestId = Just "req-2"
            , entryJobId = Just "job-1"
            , entryNode = Just "node-b"
            , entryDispatchId = Just "dispatch-1"
            , entryProtocolVersion = Just 1
            , entryDecision = Just "skip"
            , entryReason = Just "unchanged_dispatch"
            }
        )
        ["action_count" A..= (0 :: Int)]
    )
    [ ("event", Just (A.String "deploy_dispatch"))
    , ("job_id", Just (A.String "job-1"))
    , ("dispatch_id", Just (A.String "dispatch-1"))
    , ("decision", Just (A.String "skip"))
    , ("reason", Just (A.String "unchanged_dispatch"))
    ]

testStructuredNodeEventLogShape :: IO ()
testStructuredNodeEventLogShape =
  assertStructuredLogShape
    ( providerLogValue
        testTimestamp
        ( (providerLogFields "deploy_node_event_intake" ProviderSeverityWarn)
            { entryJobId = Just "job-1"
            , entryNode = Just "node-b"
            , entryDispatchId = Just "dispatch-1"
            , entryActionId = Just "job-1:node-b:0"
            , entryPhase = Just "restore"
            , entryStatus = Just "running"
            , entryProtocolVersion = Just 1
            , entryDecision = Just "ignore"
            , entryReason = Just "stale_dispatch"
            }
        )
        ["action_row_update_count" A..= (0 :: Int)]
    )
    [ ("event", Just (A.String "deploy_node_event_intake"))
    , ("action_id", Just (A.String "job-1:node-b:0"))
    , ("phase", Just (A.String "restore"))
    , ("status", Just (A.String "running"))
    , ("decision", Just (A.String "ignore"))
    , ("reason", Just (A.String "stale_dispatch"))
    ]

testStructuredWebhookBoundaryLogShape :: IO ()
testStructuredWebhookBoundaryLogShape =
  assertStructuredLogShape
    ( providerLogValue
        testTimestamp
        ( (providerLogFields "webhook_persist_intents" ProviderSeverityInfo)
            { entryJobId = Just "job-1"
            , entryPhase = Just "persist_intents"
            , entryDecision = Just "accept"
            , entryReason = Just "persisted"
            }
        )
        [ "commit_sha" A..= ("abc123" :: T.Text)
        , "node_count" A..= (2 :: Int)
        ]
    )
    [ ("event", Just (A.String "webhook_persist_intents"))
    , ("job_id", Just (A.String "job-1"))
    , ("phase", Just (A.String "persist_intents"))
    , ("decision", Just (A.String "accept"))
    , ("reason", Just (A.String "persisted"))
    , ("request_id", Just A.Null)
    ]

testDeployWsAuthFinalShape :: IO ()
testDeployWsAuthFinalShape = do
  let cfg =
        (mkRepoConfig "/tmp/hostenv-provider-auth")
          { appDeploy =
              DeployConfig
                { enable = True
                , nodeAuthTokens = Map.fromList [("node-a", "secret-token")]
                }
          }
      legacyPayload =
        A.encode
          (A.object ["node" A..= ("node-a" :: T.Text), "token" A..= ("secret-token" :: T.Text)])
      finalPayload =
        A.encode
          ( A.object
              [ "version" A..= (1 :: Int)
              , "kind" A..= ("auth" :: T.Text)
              , "messageId" A..= ("msg-auth-1" :: T.Text)
              , "timestamp" A..= ("2026-04-03T13:00:00Z" :: T.Text)
              , "node" A..= ("node-a" :: T.Text)
              , "payload" A..=
                  A.object
                    [ "token" A..= ("secret-token" :: T.Text)
                    ]
              ]
          )
      missingTokenPayload =
        A.encode
          ( A.object
              [ "version" A..= (1 :: Int)
              , "kind" A..= ("auth" :: T.Text)
              , "messageId" A..= ("msg-auth-2" :: T.Text)
              , "timestamp" A..= ("2026-04-03T13:00:00Z" :: T.Text)
              , "node" A..= ("node-a" :: T.Text)
              , "payload" A..= A.object []
              ]
          )
  assert (not (isValidDeployWsAuth cfg "node-a" legacyPayload)) "websocket auth should reject legacy auth payloads"
  assert (isValidDeployWsAuth cfg "node-a" finalPayload) "websocket auth should accept only the final protocol envelope"
  assert (not (isValidDeployWsAuth cfg "node-a" missingTokenPayload)) "websocket auth should require the final auth token field"
  assert (not (isValidDeployWsAuth cfg "node-b" finalPayload)) "websocket auth should still reject mismatched node auth"

testWsNodeEventAuthenticatedNodeBoundary :: IO ()
testWsNodeEventAuthenticatedNodeBoundary = do
  let matchingEventValue =
        A.object
          [ "version" A..= (1 :: Int)
          , "kind" A..= ("progress" :: T.Text)
          , "messageId" A..= ("msg-progress-1" :: T.Text)
          , "timestamp" A..= ("2026-04-03T13:00:00Z" :: T.Text)
          , "jobId" A..= ("job-1" :: T.Text)
          , "dispatchId" A..= ("dispatch-1" :: T.Text)
          , "actionId" A..= Just ("job-1:node-a:0" :: T.Text)
          , "node" A..= ("node-a" :: T.Text)
          , "payload" A..=
              A.object
                [ "status" A..= ("running" :: T.Text)
                , "phase" A..= ("restore" :: T.Text)
                ]
          ]
      mismatchedEventValue =
        A.object
          [ "version" A..= (1 :: Int)
          , "kind" A..= ("progress" :: T.Text)
          , "messageId" A..= ("msg-progress-2" :: T.Text)
          , "timestamp" A..= ("2026-04-03T13:00:01Z" :: T.Text)
          , "jobId" A..= ("job-1" :: T.Text)
          , "dispatchId" A..= ("dispatch-1" :: T.Text)
          , "actionId" A..= Just ("job-1:node-a:0" :: T.Text)
          , "node" A..= ("node-b" :: T.Text)
          , "payload" A..=
              A.object
                [ "status" A..= ("running" :: T.Text)
                , "phase" A..= ("restore" :: T.Text)
                ]
          ]
  case (A.eitherDecode (A.encode matchingEventValue), A.eitherDecode (A.encode mismatchedEventValue)) of
    (Right matchingEvent, Right mismatchedEvent) -> do
      assert (wsNodeEventMatchesAuthenticatedNode "node-a" (matchingEvent :: NodeEvent)) "websocket node events should accept the authenticated node"
      assert (not (wsNodeEventMatchesAuthenticatedNode "node-a" (mismatchedEvent :: NodeEvent))) "websocket node events should reject cross-node submissions"
    (Left err, _) -> assert False ("failed to decode matching websocket node event fixture: " <> err)
    (_, Left err) -> assert False ("failed to decode mismatched websocket node event fixture: " <> err)

testDispatchForNodeMigrationSequencing :: IO ()
testDispatchForNodeMigrationSequencing = do
  let sourceIntent =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..=
              [ A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("backup" :: T.Text), "toNode" A..= ("node-b" :: T.Text)]
              , A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("deactivate" :: T.Text), "toNode" A..= ("node-b" :: T.Text)]
              ]
          ]
      destinationIntent =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..=
              [ A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("restore" :: T.Text), "fromNode" A..= ("node-a" :: T.Text)]
              , A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("activate" :: T.Text), "fromNode" A..= ("node-a" :: T.Text)]
              ]
          ]
      t0 = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
      mkAction nodeName actionIndexVal opVal statusVal =
        DeployAction
          "job-1"
          nodeName
          actionIndexVal
          opVal
          "alice"
          statusVal
          Nothing
          Nothing
          Nothing
          t0
      sourceQueued = [mkAction "node-a" 0 "backup" "queued", mkAction "node-a" 1 "deactivate" "queued"]
      destinationQueued = [mkAction "node-b" 0 "restore" "queued", mkAction "node-b" 1 "activate" "queued"]
      allQueued = sourceQueued <> destinationQueued
      opNames = map getOp
      getOp (DeployAction _ _ _ opVal _ _ _ _ _ _) = opVal

  case dispatchForNode sourceIntent allQueued "node-a" of
    Nothing -> assert False "dispatchForNode should return filtered source payload"
    Just (_, actions) ->
      assert
        (opNames actions == ["backup" :: T.Text])
        "source node should dispatch backup before deactivate"

  case dispatchForNode destinationIntent allQueued "node-b" of
    Nothing -> assert False "dispatchForNode should return filtered destination payload"
    Just (_, actions) ->
      assert
        (null actions)
        "destination restore should wait for source backup success"

  let backupSucceeded = mkAction "node-a" 0 "backup" "success" : tail sourceQueued
      afterBackup = backupSucceeded <> destinationQueued
  case dispatchForNode destinationIntent afterBackup "node-b" of
    Nothing -> assert False "dispatchForNode should return destination payload after backup success"
    Just (_, actions) ->
      assert
        (opNames actions == ["restore" :: T.Text])
        "destination should dispatch restore before activate"

  let restoreSucceeded = [mkAction "node-b" 0 "restore" "success", mkAction "node-b" 1 "activate" "queued"]
      afterRestore = backupSucceeded <> restoreSucceeded
  case dispatchForNode sourceIntent afterRestore "node-a" of
    Nothing -> assert False "dispatchForNode should return source payload after restore success"
    Just (_, actions) ->
      assert
        (opNames actions == ["deactivate" :: T.Text])
        "source should dispatch deactivate after destination restore success"

  case dispatchForNode destinationIntent afterRestore "node-b" of
    Nothing -> assert False "dispatchForNode should return destination payload after restore success"
    Just (_, actions) ->
      assert
        (opNames actions == ["activate" :: T.Text])
        "destination activate should run only after restore success"

testWebsocketDispatchFingerprinting :: IO ()
testWebsocketDispatchFingerprinting = do
  let t0 = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
      mkAction nodeName actionIndexVal opVal statusVal =
        DeployAction
          "job-1"
          nodeName
          actionIndexVal
          opVal
          "alice"
          statusVal
          Nothing
          Nothing
          Nothing
          t0
      actions =
        [ mkAction "node-b" 0 "restore" "queued"
        , mkAction "node-b" 1 "activate" "queued"
        ]
      intentA =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..=
              [ A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("restore" :: T.Text)]
              , A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("activate" :: T.Text)]
              ]
          ]
      fingerprintA = dispatchFingerprint "job-1" intentA actions
      actionsUpdated = [mkAction "node-b" 0 "restore" "success", mkAction "node-b" 1 "activate" "queued"]
      intentSystemOnly =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "systemPath" A..= ("/nix/store/system-only" :: T.Text)
          , "actions" A..= ([] :: [A.Value])
          ]
      fingerprintB = dispatchFingerprint "job-1" intentA actionsUpdated
      systemFingerprint = dispatchFingerprint "job-2" intentSystemOnly []
  assert (shouldDispatchJob intentA actions actions Nothing fingerprintA) "websocket dispatch should run on first seen payload"
  assert (not (shouldDispatchJob intentA actions actions (Just fingerprintA) fingerprintA)) "websocket dispatch should skip duplicate payload fingerprints"
  assert (shouldDispatchJob intentA actionsUpdated actionsUpdated (Just fingerprintA) fingerprintB) "websocket dispatch should emit when same job id has newly executable actions"
  assert (shouldDispatchJob intentSystemOnly [] [] Nothing systemFingerprint) "websocket dispatch should also emit system-only payloads"

testDeployDeliveryIdentity :: IO ()
testDeployDeliveryIdentity = do
  let t0 = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
      mkAction statusVal =
        DeployAction
          "job-1"
          "node-b"
          0
          "restore"
          "alice"
          statusVal
          Nothing
          Nothing
          Nothing
          t0
      intent =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..=
              [ A.object
                  [ "user" A..= ("alice" :: T.Text)
                  , "op" A..= ("restore" :: T.Text)
                  ]
              ]
          ]
      queuedAction = mkAction "queued"
      runningAction = mkAction "running"
      stableQueued = dispatchStableId "job-1" "node-b" intent [queuedAction]
      stableRunning = dispatchStableId "job-1" "node-b" intent [runningAction]
      fingerprintQueued = dispatchFingerprint "job-1" intent [queuedAction]
      fingerprintRunning = dispatchFingerprint "job-1" intent [runningAction]
      deployJobEnvelope = buildDeployJobEnvelope t0 "msg-dispatch-1" "job-1" "abc123" "node-b" stableQueued intent [queuedAction]
  assert (stableQueued == stableRunning) "dispatchStableId should stay fixed across action status changes"
  assert (fingerprintQueued /= fingerprintRunning) "dispatchFingerprint should continue tracking mutable delivery changes"
  assert
    ( currentDispatchIdFor "job-1" "node-b" intent [queuedAction]
        == currentDispatchIdFor "job-1" "node-b" intent [runningAction]
    )
    "current dispatch identity should stay fixed across action status changes"
  case deployJobEnvelope of
    A.Object obj -> do
      assert (KM.lookup (K.fromString "kind") obj == Just (A.String "deploy_job")) "deploy job should use the final protocol kind field"
      assert (KM.lookup (K.fromString "dispatchId") obj == Just (A.String stableQueued)) "deploy job should include dispatch identity at the envelope level"
      assert (not (KM.member (K.fromString "type") obj)) "deploy job should not emit the legacy type field"
    _ -> assert False "deploy job envelope should encode to a JSON object"
  case A.toJSON queuedAction of
    A.Object obj ->
      assert
        (KM.lookup (K.fromString "actionId") obj == Just (A.String (deployActionId queuedAction)))
        "deploy actions should include a stable actionId field"
    _ -> assert False "deploy action should encode to a JSON object"

testDispatchIdentityUsesDispatchedSubset :: IO ()
testDispatchIdentityUsesDispatchedSubset = do
  let t0 = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
      action0 =
        DeployAction
          "job-1"
          "node-b"
          0
          "activate"
          "alice"
          "queued"
          Nothing
          Nothing
          Nothing
          t0
      action1 =
        DeployAction
          "job-1"
          "node-b"
          1
          "reload"
          "alice"
          "queued"
          Nothing
          Nothing
          Nothing
          t0
      allActions = [action0, action1]
      intent =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "actions" A..=
              [ A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("activate" :: T.Text)]
              , A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("reload" :: T.Text)]
              ]
          ]
      currentDispatchId = currentDispatchIdFor "job-1" "node-b" intent allActions
  case dispatchForNode intent allActions "node-b" of
    Nothing -> assert False "dispatchForNode should produce a dispatchable subset"
    Just (filteredIntent, filteredActions) -> do
      let filteredDispatchId = dispatchStableId "job-1" "node-b" filteredIntent filteredActions
          fullNodeDispatchId = dispatchStableId "job-1" "node-b" intent allActions
      assert (filteredActions == [action0]) "only the first queued action should be dispatchable initially"
      assert (currentDispatchId == Just filteredDispatchId) "currentDispatchIdFor should match the dispatched subset identity"
      assert (currentDispatchId /= Just fullNodeDispatchId) "current dispatch identity should not hash queued but not yet dispatched actions"

testActionlessIntentKeepsDispatchIdentity :: IO ()
testActionlessIntentKeepsDispatchIdentity = do
  let intent =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "systemPath" A..= ("/nix/store/system-only" :: T.Text)
          , "actions" A..= ([] :: [A.Value])
          ]
      expectedDispatchId = dispatchStableId "job-1" "node-b" intent []
      currentDispatchId = currentDispatchIdFor "job-1" "node-b" intent []
      intakeDecision = classifyProjectedNodeEvent False currentDispatchId Nothing expectedDispatchId "success"
  assert (currentDispatchId == Just expectedDispatchId) "actionless deploy intents should still keep a current dispatch identity"
  assert (intakeDecision == AcceptProjectedNodeEvent) "actionless deploy callbacks should be accepted when they report the current dispatch id"

testDispatchValidationPrefersPersistedSentDispatch :: IO ()
testDispatchValidationPrefersPersistedSentDispatch = do
  let t0 = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
      queuedAction :: DeployAction
      queuedAction =
        DeployAction
          "job-1"
          "node-b"
          0
          "activate"
          "alice"
          "queued"
          Nothing
          Nothing
          Nothing
          t0
      successfulAction :: DeployAction
      successfulAction =
        DeployAction
          "job-1"
          "node-b"
          0
          "activate"
          "alice"
          "success"
          Nothing
          Nothing
          Nothing
          t0
      mixedIntent =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "systemPath" A..= ("/nix/store/system-profile" :: T.Text)
          , "actions" A..=
              [ A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("activate" :: T.Text)]
              ]
          ]
      sentDispatchId = dispatchStableId "job-1" "node-b" mixedIntent [queuedAction]
      recomputedDispatchId = currentDispatchIdFor "job-1" "node-b" mixedIntent [successfulAction]
      expectedDispatchId = dispatchIdForEventValidation (Just sentDispatchId) recomputedDispatchId
  assert (recomputedDispatchId /= Just sentDispatchId) "recomputed dispatch identity should shrink after the only action succeeds"
  assert (expectedDispatchId == Just sentDispatchId) "event validation should prefer the persisted sent dispatch id over a recomputed subset id"
  assert (dispatchIdForEventValidation Nothing recomputedDispatchId == recomputedDispatchId) "event validation should fall back to recomputed dispatch ids when no sent dispatch id exists"

testFinalIntentSuccessAcceptsPersistedDispatch :: IO ()
testFinalIntentSuccessAcceptsPersistedDispatch = do
  let t0 = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
      queuedAction :: DeployAction
      queuedAction =
        DeployAction
          "job-1"
          "node-b"
          0
          "activate"
          "alice"
          "queued"
          Nothing
          Nothing
          Nothing
          t0
      successfulAction :: DeployAction
      successfulAction =
        DeployAction
          "job-1"
          "node-b"
          0
          "activate"
          "alice"
          "success"
          Nothing
          Nothing
          Nothing
          t0
      mixedIntent =
        A.object
          [ "schemaVersion" A..= (1 :: Int)
          , "systemPath" A..= ("/nix/store/system-profile" :: T.Text)
          , "actions" A..=
              [ A.object ["user" A..= ("alice" :: T.Text), "op" A..= ("activate" :: T.Text)]
              ]
          ]
      sentDispatchId = dispatchStableId "job-1" "node-b" mixedIntent [queuedAction]
      recomputedDispatchId = currentDispatchIdFor "job-1" "node-b" mixedIntent [successfulAction]
      expectedDispatchId = dispatchIdForEventValidation (Just sentDispatchId) recomputedDispatchId
      intakeDecision = classifyProjectedNodeEvent False expectedDispatchId Nothing sentDispatchId "success"
  assert (intakeDecision == AcceptProjectedNodeEvent) "final intent success should still be accepted for the original sent dispatch after action success"

testNodeEventProtocolRoundTrip :: IO ()
testNodeEventProtocolRoundTrip = do
  mapM_ roundTrip ["progress", "action_result"]
  where
    roundTrip kindValue = do
      let eventValue =
            A.object
              [ "version" A..= (1 :: Int)
              , "kind" A..= kindValue
              , "messageId" A..= ("msg-progress-1" :: T.Text)
              , "timestamp" A..= ("2026-04-03T13:00:00Z" :: T.Text)
              , "jobId" A..= ("job-1" :: T.Text)
              , "dispatchId" A..= ("dispatch-1" :: T.Text)
              , "actionId" A..= Just ("job-1/node-b/1" :: T.Text)
              , "node" A..= ("node-b" :: T.Text)
              , "payload" A..=
                  A.object
                    [ "status" A..= ("running" :: T.Text)
                    , "phase" A..= ("restore" :: T.Text)
                    , "message" A..= ("restoring backup" :: T.Text)
                    ]
              ]
      case A.eitherDecode (A.encode eventValue) of
        Left err -> assert False ("node event round-trip failed: " <> err)
        Right decoded -> do
          let projected = projectNodeEvent decoded
          assert (A.toJSON (decoded :: NodeEvent) == eventValue) ("node events should round-trip for kind " <> T.unpack kindValue)
          assert (projected.projectedDispatchId == Just "dispatch-1") "round-tripped node events should preserve dispatch identity"
          assert (projected.projectedActionId == Just "job-1/node-b/1") "round-tripped node events should preserve action identity"

testEventProjectionBehavior :: IO ()
testEventProjectionBehavior = do
  let t0 = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
      action0 =
        DeployAction
          "job-1"
          "node-b"
          0
          "restore"
          "alice"
          "queued"
          Nothing
          Nothing
          Nothing
          t0
      action1 =
        DeployAction
          "job-1"
          "node-b"
          1
          "restore"
          "alice"
          "queued"
          Nothing
          Nothing
          Nothing
          t0
      richerEventValue =
        A.object
          [ "version" A..= (1 :: Int)
          , "kind" A..= ("progress" :: T.Text)
          , "messageId" A..= ("msg-progress-1" :: T.Text)
          , "timestamp" A..= ("2026-04-03T13:00:00Z" :: T.Text)
          , "jobId" A..= ("job-1" :: T.Text)
          , "dispatchId" A..= ("dispatch-1" :: T.Text)
          , "actionId" A..= deployActionId action1
          , "node" A..= ("node-b" :: T.Text)
          , "payload" A..=
              A.object
                [ "status" A..= ("running" :: T.Text)
                , "phase" A..= ("restore" :: T.Text)
                , "message" A..= ("restoring backup" :: T.Text)
                ]
          ]
      missingActionIdValue =
        A.object
          [ "version" A..= (1 :: Int)
          , "kind" A..= ("progress" :: T.Text)
          , "messageId" A..= ("msg-progress-2" :: T.Text)
          , "timestamp" A..= ("2026-04-03T13:00:00Z" :: T.Text)
          , "jobId" A..= ("job-1" :: T.Text)
          , "dispatchId" A..= ("dispatch-1" :: T.Text)
          , "node" A..= ("node-b" :: T.Text)
          , "payload" A..=
              A.object
                [ "status" A..= ("running" :: T.Text)
                , "phase" A..= ("restore" :: T.Text)
                ]
          ]
  case A.fromJSON richerEventValue of
    A.Error err -> assert False ("failed to decode richer node event: " <> err)
    A.Success event -> do
      let ProjectedNodeEvent projectedNodeValue projectedStatusValue projectedPhaseValue projectedMessageValue projectedActionIdValue projectedDispatchIdValue = projectNodeEvent (event :: NodeEvent)
      assert (projectedNodeValue == "node-b") "event projection should preserve the node identity"
      assert (projectedStatusValue == "running") "event projection should accept payload-only status fields"
      assert (projectedPhaseValue == Just "restore") "event projection should accept payload-only phase fields"
      assert (projectedMessageValue == Just "restoring backup") "event projection should accept payload-only message fields"
      assert (projectedDispatchIdValue == Just "dispatch-1") "event projection should retain dispatch identity"
      assert (projectedActionIdValue == Just (deployActionId action1)) "event projection should retain action identity"
      assert
        (selectDeployActionIndex "job-1" "node-b" "activate" ["success"] projectedActionIdValue [action0, action1] == Just 1)
        "event projection should target the identified action row by actionId only"
  case A.fromJSON missingActionIdValue of
    A.Error err -> assert False ("failed to decode missing-action-id node event: " <> err)
    A.Success event -> do
      let projected = projectNodeEvent (event :: NodeEvent)
      assert (projected.projectedActionId == Nothing) "event projection should expose missing action identity"
      assert
        (selectDeployActionIndex "job-1" "node-b" "restore" ["queued", "waiting", "running"] projected.projectedActionId [action0, action1] == Nothing)
        "event projection should reject action updates that omit actionId"

testProjectedNodeEventIntake :: IO ()
testProjectedNodeEventIntake = do
  let t0 = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
      queuedAction =
        DeployAction
          "job-1"
          "node-b"
          1
          "restore"
          "alice"
          "queued"
          Nothing
          Nothing
          Nothing
          t0
      runningAction =
        DeployAction
          "job-1"
          "node-b"
          1
          "restore"
          "alice"
          "running"
          Nothing
          Nothing
          Nothing
          t0
  assert
    ( classifyProjectedNodeEvent True (Just "dispatch-1") (Just runningAction) "dispatch-1" "running"
        == IgnoreDuplicateProjectedNodeEvent
    )
    "same-dispatch same-action same-status events should be accepted as no-op duplicates"
  assert
    ( classifyProjectedNodeEvent True (Just "dispatch-2") (Just queuedAction) "dispatch-1" "running"
        == IgnoreStaleProjectedNodeEvent
    )
    "stale dispatch replays should be ignored safely"
  assert
    ( classifyProjectedNodeEvent True (Just "dispatch-1") Nothing "dispatch-1" "running"
        == RejectUnknownProjectedNodeEvent
    )
    "unknown action identities should still be rejected"
  assert
    ( classifyProjectedNodeEvent True (Just "dispatch-1") (Just queuedAction) "dispatch-1" "running"
        == AcceptProjectedNodeEvent
    )
    "new same-dispatch action status transitions should still be accepted"

testBackupSnapshotLookupMissing :: IO ()
testBackupSnapshotLookupMissing =
  assert
    (classifyDeployBackupSnapshotRows [] == DeployBackupSnapshotMissing)
    "snapshot lookup should surface missing snapshots explicitly"

testBackupSnapshotLookupMalformed :: IO ()
testBackupSnapshotLookupMalformed =
  assert
    (classifyDeployBackupSnapshotRows [A.object ["snapshotMetadata" A..= buildDeployBackupSnapshotMetadata (A.object ["snapshots" A..= A.object []])]] == DeployBackupSnapshotMalformed)
    "snapshot lookup should reject malformed stored payloads"

testBackupSnapshotLookupChecksumMismatch :: IO ()
testBackupSnapshotLookupChecksumMismatch =
  assert
    (classifyDeployBackupSnapshotRows [storedPayload] == DeployBackupSnapshotMalformed)
    "snapshot lookup should reject checksum mismatches"
  where
    payload = A.object ["snapshots" A..= A.object ["db" A..= ("snapshot-1" :: T.Text)]]
    metadata = buildDeployBackupSnapshotMetadata payload
    storedPayload =
      A.object
        [ "user" A..= ("alice" :: T.Text)
        , "op" A..= ("backup" :: T.Text)
        , "snapshotPayload" A..= payload
        , "snapshotMetadata" A..=
            A.object
              [ "size" A..= metadata.snapshotSize
              , "checksum" A..= ("deadbeef" :: T.Text)
              , "contentType" A..= metadata.snapshotContentType
              , "schemaVersion" A..= metadata.snapshotSchemaVersion
              ]
        ]

testBackupSnapshotResponseShape :: IO ()
testBackupSnapshotResponseShape =
  case classifyDeployBackupSnapshotRows [storedPayload] of
    DeployBackupSnapshotAvailable snapshot ->
      assert
        ( backupSnapshotResponseValue "job-1" "node-b" "node-a" "alice" snapshot
            == A.object
              [ "jobId" A..= ("job-1" :: T.Text)
              , "node" A..= ("node-b" :: T.Text)
              , "sourceNode" A..= ("node-a" :: T.Text)
              , "user" A..= ("alice" :: T.Text)
              , "metadata" A..= metadata
              , "payload" A..= payload
              ]
        )
        "snapshot handler should return explicit metadata alongside payload"
    _ -> assert False "valid stored snapshot should decode"
  where
    payload = A.object ["snapshots" A..= A.object ["db" A..= ("snapshot-1" :: T.Text)]]
    metadata = buildDeployBackupSnapshotMetadata payload
    storedPayload =
      A.object
        [ "user" A..= ("alice" :: T.Text)
        , "op" A..= ("backup" :: T.Text)
        , "snapshotPayload" A..= payload
        , "snapshotMetadata" A..= metadata
        ]

testPlanParsing :: IO ()
testPlanParsing = do
  let planJson =
        BLC.pack
          "{\"environments\":{\"env-a\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-main\"},\"node\":\"node-a\"},\"env-b\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-dev\"},\"node\":\"node-b\"},\"env-c\":{\"hostenv\":{\"organisation\":\"other\",\"project\":\"x\",\"projectNameHash\":\"hash-x\"},\"node\":\"node-c\"}}}"
  case projectForHash "hash-main" planJson of
    Left err -> assert False ("projectForHash failed: " <> show err)
    Right ref -> do
      assert (ref.prOrg == "acme" && ref.prProject == "site") "projectForHash should resolve org/project"
      case nodesForProject ref.prOrg ref.prProject planJson of
        Left err -> assert False ("nodesForProject failed: " <> show err)
        Right nodes -> assert (nodes == ["node-a", "node-b"]) "plan parsing should return matching nodes"

testNodeOrderWithMigrations :: IO ()
testNodeOrderWithMigrations = do
  let planJson =
        BLC.pack
          "{\"environments\":{\"env-a\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-main\"},\"node\":\"node-b\",\"previousNode\":\"node-a\"},\"env-b\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-dev\"},\"node\":\"node-a\"}}}"
  case nodesForProject "acme" "site" planJson of
    Left err -> assert False ("nodesForProject failed: " <> show err)
    Right nodes ->
      assert (nodes == ["node-b", "node-a"]) "migration ordering should deploy destination before source"

testNodeOrderWithDnsSkipsNonMigratingEnvDiscovery :: IO ()
testNodeOrderWithDnsSkipsNonMigratingEnvDiscovery = do
  let planJson =
        BLC.pack
          "{\"hostenvHostname\":\"hosting.test\",\"nodes\":{\"node-a\":{},\"node-b\":{},\"node-c\":{}},\"environments\":{\"env-a\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-main\"},\"node\":\"node-c\"},\"env-b\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-dev\"},\"node\":\"node-b\",\"migrations\":[\"db\"]}}}"
  callsRef <- newIORef ([] :: [(T.Text, T.Text)])
  let pointsTo vhost expectedHost = do
        modifyIORef' callsRef (<> [(vhost, expectedHost)])
        if vhost == "env-a.hosting.test"
          then pure (expectedHost == "node-a.hosting.test" || expectedHost == "node-b.hosting.test")
          else pure False
  result <- nodesForProjectWithDnsWith pointsTo "acme" "site" planJson
  case result of
    Left err -> assert False ("nodesForProjectWithDnsWith failed unexpectedly: " <> show err)
    Right nodes -> do
      assert (nodes == ["node-b", "node-c"]) "dns node ordering should still include matching nodes"
      calls <- readIORef callsRef
      assert
        (not (any (\(vhost, _) -> vhost == "env-a.hosting.test") calls))
        "non-migrating env should skip DNS previous-node discovery"


testCommandSequence :: IO ()
testCommandSequence = do
  let planJson =
        BLC.pack
          "{\"environments\":{\"env-a\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-main\"},\"node\":\"node-a\"},\"env-b\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-dev\"},\"node\":\"node-b\"}}}"
  ref <- newIORef ([] :: [CommandSpec])
  stageRef <- newIORef ([] :: [WebhookStage])
  let runner spec = do
        modifyIORef' ref (<> [spec])
        if spec.cmdName == "git"
          && spec.cmdArgs == ["status", "--porcelain", "--", "generated", "flake.nix", "flake.lock"]
          then pure (Right (CommandOutput " M generated/plan.json\n" ""))
          else pure (Right (CommandOutput "" ""))
  let notifyStage stage = modifyIORef' stageRef (<> [stage])
  let cfg = WebhookConfig "/tmp/provider" "/tmp/provider/generated/plan.json"
  let proj = ProjectRef "acme" "site"
  result <- runWebhookWith notifyStage runner (pure planJson) cfg proj
  case result of
    Left err -> assert False ("webhook run failed: " <> show err)
    Right res -> do
      assert (res.updateStatus == WebhookUpdateCommitted) "webhook run should commit when changes exist"
      assert (res.nodes == ["node-a", "node-b"]) "webhook run should preserve ordered nodes"
      assert (length res.intents == 2) "webhook run should emit one intent per matching node"
      let firstActions = case res.intents of
            (intent:_) -> intent.actions
            _ -> []
      assert (any (\action -> action.op == "activate") firstActions) "new deployments should include activate intent actions"
      cmds <- readIORef ref
      let expected =
            [ CommandSpec "git" ["pull", "--rebase", "--autostash"] "/tmp/provider"
            , CommandSpec "nix" ["flake", "update", "hostenv", "acme__site"] "/tmp/provider"
            , CommandSpec "nix" ["run", ".#hostenv-provider", "--", "plan"] "/tmp/provider"
            , CommandSpec "nix" ["run", ".#hostenv-provider", "--", "dns-gate"] "/tmp/provider"
            , CommandSpec "git" ["add", "-A", "generated"] "/tmp/provider"
            , CommandSpec "git" ["add", "-A", "flake.nix", "flake.lock"] "/tmp/provider"
            , CommandSpec "git" ["status", "--porcelain", "--", "generated", "flake.nix", "flake.lock"] "/tmp/provider"
            , CommandSpec "git" ["rev-parse", "HEAD"] "/tmp/provider"
            ]

      assert (cmds == expected) "webhook command sequence should match"
      stages <- readIORef stageRef
      let expectedStages =
            [ StageSyncRepo
            , StageUpdateFlake
            , StagePlan
            , StageDnsGate
            , StageLoadPlan
            , StageResolveNodes
            , StageDeriveIntents
            , StageWriteIntent
            , StageFinalizeRepo
            ]
      assert (stages == expectedStages) "webhook run should report stage transitions in order"


recordRunner :: IORef [CommandSpec] -> CommandRunner
recordRunner ref spec = do
  modifyIORef' ref (<> [spec])
  pure (Right (CommandOutput "" ""))


testProjectHashSelection :: IO ()
testProjectHashSelection = do
  let planJson =
        BLC.pack
          "{\"environments\":{\"main\":{\"type\":\"production\",\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-prod\"}},\"dev\":{\"type\":\"development\",\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-dev\"}}}}"
  case projectHashFor "acme" "site" planJson of
    Left err -> assert False ("projectHashFor failed: " <> show err)
    Right hash -> assert (hash == "hash-prod") "projectHashFor should prefer production"

  let planJson2 =
        BLC.pack
          "{\"environments\":{\"main\":{\"type\":\"development\",\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-main\"}},\"feature\":{\"type\":\"development\",\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"projectNameHash\":\"hash-feature\"}}}}"
  case projectHashFor "acme" "site" planJson2 of
    Left err -> assert False ("projectHashFor failed: " <> show err)
    Right hash -> assert (hash == "hash-main") "projectHashFor should prefer main when no production"

testPrevNodeDiscoveryResolution :: IO ()
testPrevNodeDiscoveryResolution = do
  assert
    (canonicalHostInDomain "env-main-a1b2c3" "hosting.test" == "env-main-a1b2c3.hosting.test")
    "canonical host should append hostenv domain"
  assert
    (canonicalHostInDomain "env-main-a1b2c3.hosting.test" "hosting.test" == "env-main-a1b2c3.hosting.test")
    "canonical host should not duplicate suffix"

  assert
    (resolvePrevNodeFromMatches "node-b" [] == PrevNodeSkip)
    "empty matches should skip discovery"
  assert
    (resolvePrevNodeFromMatches "node-b" ["node-a"] == PrevNodeResolved "node-a")
    "single non-current match should resolve previous node"
  assert
    (resolvePrevNodeFromMatches "node-b" ["node-b"] == PrevNodeSkip)
    "single current-node match should skip discovery"
  assert
    (resolvePrevNodeFromMatches "node-b" ["node-a", "node-b"] == PrevNodeSkip)
    "ambiguous matches including current node should skip discovery"
  assert
    ( resolvePrevNodeFromMatches "node-c" ["node-a", "node-b"]
        == PrevNodeAmbiguousFatal ["node-a", "node-b"]
    )
    "ambiguous matches excluding current node should be fatal"


testTemplateRender :: IO ()
testTemplateRender = do
  let template = "inputs = {\\n{{HOSTENV_PROJECT_INPUTS}}\\n};"
  let inputs = renderProjectInputs [("acme__site", "gitlab:acme/site?dir=.hostenv")]
  case renderFlakeTemplate template inputs of
    Left err -> assert False ("renderFlakeTemplate failed: " <> show err)
    Right out -> assert (T.isInfixOf "acme__site.url" out) "template should include rendered inputs"


testGitCredentials :: IO ()
testGitCredentials = do
  let rendered = renderGitCredentials [("https://gitlab.com/acme/site.git", "token123")]
  assert (T.isInfixOf "oauth2:token123@gitlab.com" rendered) "credentials should inject token"

testGitConfigAuthorIdentity :: IO ()
testGitConfigAuthorIdentity = do
  let gitConfigPath = "/tmp/test-gitconfig"
  let credsPath = "/tmp/test-creds"
  let cfg = mkRepoConfig "/tmp/test-data"
  let cfgWithPaths = cfg { appGitConfigPath = gitConfigPath, appGitCredentialsPath = credsPath }
  ensureGitConfig cfgWithPaths
  content <- readFile gitConfigPath
  let textContent = T.pack content
  assert (T.isInfixOf "[user]" textContent) "git config should include user section"
  assert (T.isInfixOf "email = hostenv-provider@localhost" textContent) "git config should set email"
  assert (T.isInfixOf "name = hostenv-provider" textContent) "git config should set name"

testNixAccessTokenConfig :: IO ()
testNixAccessTokenConfig = do
  let oauthLine = appendNixAccessTokenConfig Nothing "gitlab.com" NixGitlabOAuth2 "oauth-token"
  assert
    (oauthLine == "access-tokens = gitlab.com=OAuth2:oauth-token")
    "nix access token config should encode OAuth2 token type"
  let merged = appendNixAccessTokenConfig (Just "extra-experimental-features = nix-command flakes") "gitlab.com" NixGitlabPAT "pat-token"
  assert
    (merged == "extra-experimental-features = nix-command flakes\naccess-tokens = gitlab.com=PAT:pat-token")
    "nix access token config should append PAT token line"


testReadGitlabSecrets :: IO ()
testReadGitlabSecrets = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tmpDir "gitlab-secrets"
  hClose handle
  writeFile path "client_id=abc123\n# comment\nclient_secret=def456\n"
  secrets <- readGitlabSecrets path
  removeFile path
  assert (secrets.gitlabClientId == "abc123") "readGitlabSecrets should parse client_id"
  assert (secrets.gitlabClientSecret == "def456") "readGitlabSecrets should parse client_secret"

testGitlabOAuthCredentialMerge :: IO ()
testGitlabOAuthCredentialMerge = do
  let now = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
      previous :: OAuthCredential
      previous =
        OAuthCredential
          "gitlab.com"
          "old-access-token"
          (Just "old-refresh-token")
          (Just "Bearer")
          "api"
          (Just (addUTCTime 120 now))
      refreshedToken :: GitlabTokenResponse
      refreshedToken =
        GitlabTokenResponse
          "new-access-token"
          Nothing
          Nothing
          Nothing
          (Just 120)
      merged = oauthCredentialFromTokenAt now "GitLab.Com" (Just previous) refreshedToken
  assert (merged.host == "gitlab.com") "oauth merge should normalize host to lowercase"
  assert (merged.accessToken == "new-access-token") "oauth merge should replace access token"
  assert (merged.refreshToken == Just "old-refresh-token") "oauth merge should preserve refresh token when omitted"
  assert (merged.tokenType == Just "Bearer") "oauth merge should preserve token type when omitted"
  assert (merged.scopes == "api") "oauth merge should preserve scopes when omitted"
  assert (merged.expiresAt == Just (addUTCTime 60 now)) "oauth merge should apply 60 second skew to expiry"

  let updatedToken :: GitlabTokenResponse
      updatedToken =
        GitlabTokenResponse
          "newer-access-token"
          (Just "new-refresh-token")
          (Just "Token")
          (Just "read_api")
          (Just 30)
      updated = oauthCredentialFromTokenAt now "gitlab.com" (Just previous) updatedToken
  assert (updated.refreshToken == Just "new-refresh-token") "oauth merge should use new refresh token when provided"
  assert (updated.tokenType == Just "Token") "oauth merge should use new token type when provided"
  assert (updated.scopes == "read_api") "oauth merge should use new scopes when provided"
  assert (updated.expiresAt == Just now) "oauth merge should clamp negative skewed expiry to now"

testGitlabApiErrorFormatting :: IO ()
testGitlabApiErrorFormatting = do
  let longBody = BLC.replicate 800 'x'
      err = gitlabApiError "GitLab project lookup failed" (mkStatus 422 "unprocessable") longBody
  case err of
    GitlabHttpError { operation = op, status = code, responseBody = mBody } -> do
      assert (op == "GitLab project lookup failed") "gitlabApiError should preserve operation text"
      assert (code == 422) "gitlabApiError should preserve status code"
      case mBody of
        Nothing -> assert False "gitlabApiError should include non-empty response body snippet"
        Just snippet -> assert (T.length snippet == 500) "gitlabApiError should truncate response snippets to 500 chars"
    _ -> assert False "gitlabApiError should construct GitlabHttpError"

  let rendered = renderGitlabError err
  assert ("HTTP 422" `T.isInfixOf` rendered) "renderGitlabError should include status code"
  assert ("GitLab response:" `T.isInfixOf` rendered) "renderGitlabError should include response snippet marker"

  let emptyBodyErr = gitlabApiError "GitLab project lookup failed" (mkStatus 500 "internal") BL.empty
  case emptyBodyErr of
    GitlabHttpError { responseBody = Nothing } -> pure ()
    _ -> assert False "gitlabApiError should omit empty response body snippets"

testGitlabAccessMessages :: IO ()
testGitlabAccessMessages = do
  let accessMsg = renderAccessDeniedMessage "liammcdermott"
  assert ("has not been granted access to this Hostenv provider project." `T.isInfixOf` accessMsg) "access denied message should explain the cause"
  assert ("allEnvironments.users.<name>.gitlabUsername" `T.isInfixOf` accessMsg) "access denied message should mention allEnvironments mapping"
  assert ("environments.<name>.users.<name>.gitlabUsername" `T.isInfixOf` accessMsg) "access denied message should mention environment mapping"
  let mismatchMsg = renderUserIdMismatchMessage "liammcdermott" 101 202
  assert ("liammcdermott" `T.isInfixOf` mismatchMsg) "mismatch message should include username"
  assert ("(101)" `T.isInfixOf` mismatchMsg) "mismatch message should include GitLab user id"
  assert ("(202)" `T.isInfixOf` mismatchMsg) "mismatch message should include Hostenv user id"

testGitlabReauthClassification :: IO ()
testGitlabReauthClassification = do
  assert (isReauthError (GitlabAuthMissingCredential UserCredentialContext)) "missing user credential should require reauth"
  assert (isReauthError (GitlabAuthExpiredNoRefresh ProjectCredentialContext)) "expired project credential without refresh should require reauth"
  assert (isReauthError (GitlabRefreshFailed UserCredentialContext "refresh failed")) "refresh failure should require reauth"
  assert (isReauthError (GitlabHttpError "lookup" 401 Nothing)) "HTTP 401 should require reauth"
  assert (isReauthError (GitlabHttpError "lookup" 403 Nothing)) "HTTP 403 should require reauth"
  assert (not (isReauthError (GitlabHttpError "lookup" 500 Nothing))) "HTTP 500 should not be treated as reauth"
  assert (not (isReauthError (GitlabInvariantError "db failure"))) "invariant failures should not be treated as reauth"
  let projectRefreshMsg = renderGitlabError (GitlabRefreshFailed ProjectCredentialContext "refresh failed")
  assert ("re-add the project" `T.isInfixOf` projectRefreshMsg) "project refresh failure message should include remediation guidance"

testRepoAuthFailureClassifier :: IO ()
testRepoAuthFailureClassifier = do
  assert (isAuthFailure "HTTP Basic: Access denied") "auth classifier should catch HTTP basic denial"
  assert (isAuthFailure "Authentication failed for repository") "auth classifier should catch authentication failure"
  assert (isAuthFailure "could not read username for 'https://gitlab.com'") "auth classifier should catch missing username errors"
  assert (not (isAuthFailure "fatal: unable to access: TLS timeout")) "auth classifier should ignore non-auth transport failures"

testWebhookResultPriority :: IO ()
testWebhookResultPriority = do
  let primary = WebhookPlanError StagePlan "primary failed"
      okResult = WebhookResult [] [] "sha" WebhookUpdateNoop
  case chooseFinalResult (Left primary) (Just "revoke failed") of
    Left (Left err) -> assert (err == primary) "primary webhook failure should take precedence over revoke failures"
    _ -> assert False "expected primary webhook failure to win"

  case chooseFinalResult (Right okResult) (Just "revoke failed") of
    Left (Right revokeErr) -> assert (revokeErr == "revoke failed") "revoke failure should surface when primary succeeds"
    _ -> assert False "expected revoke error when primary succeeded"

  case chooseFinalResult (Right okResult) Nothing of
    Right finalResult -> assert (finalResult == okResult) "successful webhook result should pass through when revoke succeeds"
    _ -> assert False "expected successful result when there is no revoke error"

testShouldWaitForCallbacks :: IO ()
testShouldWaitForCallbacks = do
  let oneIntent = [NodeIntent "node-a" [NodeAction "activate" "alice" Nothing Nothing []]]
  assert (shouldWaitForCallbacks WebhookUpdateCommitted True oneIntent) "waiting should require committed update, deploy callbacks enabled, and non-empty intents"
  assert (not (shouldWaitForCallbacks WebhookUpdateNoop True oneIntent)) "waiting should be skipped when commit is not pushed"
  assert (not (shouldWaitForCallbacks WebhookUpdateCommitted False oneIntent)) "waiting should be skipped when deploy callbacks are disabled"
  assert (not (shouldWaitForCallbacks WebhookUpdateCommitted True [])) "waiting should be skipped when no node intents are generated"

testPersistIntentsActionsAndPushOrder :: IO ()
testPersistIntentsActionsAndPushOrder = do
  stepsRef <- newIORef ([] :: [T.Text])
  let record step = modifyIORef' stepsRef (<> [step])
  result <-
    persistIntentsActionsAndPushWith
      (record "intents")
      (record "actions")
      (record "events")
      (record "push" >> pure (Right ()))
  assert (result == Right ()) "persist helper should return push result"
  steps <- readIORef stepsRef
  assert (steps == ["intents", "actions", "events", "push"]) "persist helper should run persistence before push"

testTokenEncryptionRoundtrip :: IO ()
testTokenEncryptionRoundtrip = do
  let cipher = TokenCipher "test-key" ("0123456789abcdef0123456789abcdef" :: BS.ByteString)
  encryptedResult <- encryptToken cipher "token-value"
  case encryptedResult of
    Left err -> assert False ("encryptToken failed: " <> T.unpack err)
    Right encrypted ->
      case decryptToken cipher encrypted.encryptedTokenNonce encrypted.encryptedTokenCiphertext encrypted.encryptedTokenKeyId of
        Left err -> assert False ("decryptToken failed: " <> T.unpack err)
        Right value -> assert (value == "token-value") "encrypt/decrypt should roundtrip"

testTokenKeyLoading :: IO ()
testTokenKeyLoading = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tmpDir "token-key"
  hClose handle
  writeFile path "key=0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\n"
  cipherResult <- loadTokenCipher path
  removeFile path
  case cipherResult of
    Left err -> assert False ("loadTokenCipher failed: " <> T.unpack err)
    Right cipher ->
      assert (BS.length cipher.tokenCipherKey == 32) "token key file should load 32-byte key"

testLoadConfigJobDefaults :: IO ()
testLoadConfigJobDefaults = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tmpDir "provider-config-"
  hClose handle
  BL.writeFile path $
    A.encode $
      A.object
        [ "dataDir" A..= ("/tmp/hostenv-provider" :: T.Text)
        , "flakeRoot" A..= ("." :: T.Text)
        , "listenSocket" A..= ("/tmp/hostenv-provider.sock" :: T.Text)
        , "webhookHost" A..= ("example.invalid" :: T.Text)
        , "uiBasePath" A..= ("/dashboard" :: T.Text)
        , "uiBaseUrl" A..= ("https://example.invalid" :: T.Text)
        , "dbUri" A..= ("host=/tmp dbname=hostenv-provider user=test" :: T.Text)
        , "gitlab" A..= A.object [ "enable" A..= False ]
        , "seedUsers" A..= ([] :: [A.Value])
        , "gitConfigFile" A..= ("/tmp/gitconfig" :: T.Text)
        , "gitCredentialsFile" A..= ("/tmp/git-credentials" :: T.Text)
        , "flakeTemplate" A..= ("flake.template.nix" :: T.Text)
        , "deploy" A..= A.object [ "enable" A..= False ]
        ]
  cfg <- loadConfig path
  removeFile path
  assert (cfg.appJobsRetentionDays == 30) "loadConfig should default jobs.retentionDays"
  assert (cfg.appJobsCleanupIntervalMins == 1440) "loadConfig should default jobs.cleanupIntervalMins"
  assert (cfg.appJobsWaitTimeoutMins == 120) "loadConfig should default jobs.waitTimeoutMins"
  assert (cfg.appJobsWaitInterval == 60) "loadConfig should default jobs.waitInterval"

testEnsureProviderRepoMissing :: IO ()
testEnsureProviderRepoMissing = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tmpDir "provider-repo-missing-"
  hClose handle
  removeFile path
  let cfg = mkRepoConfig path
  result <- ensureProviderRepo cfg
  case result of
    Right RepoMissing -> pure ()
    other -> assert False ("expected RepoMissing, got " <> show other)

testEnsureProviderRepoInvalidDir :: IO ()
testEnsureProviderRepoInvalidDir = do
  tmpDir <- getTemporaryDirectory
  (path, handle) <- openTempFile tmpDir "provider-repo-invalid-"
  hClose handle
  removeFile path
  createDirectoryIfMissing True (path <> "/git/provider.git")
  let cfg = mkRepoConfig path
  result <- ensureProviderRepo cfg
  removePathForcibly path
  case result of
    Left msg -> assert ("provider bare repository is invalid" `T.isInfixOf` msg) "invalid provider bare repo should report validation error"
    other -> assert False ("expected bare repo validation error, got " <> show other)


testDeployIntentStablePayload :: IO ()
testDeployIntentStablePayload = do
  let ref = ProjectRef "acme" "site"
      intent = NodeIntent "node-a" [NodeAction "activate" "alice" Nothing Nothing []]
      payload = renderDeployIntentDocument ref [intent]
      missingKey key =
        case payload of
          A.Object obj -> not (KM.member (K.fromString key) obj)
          _ -> False
  assert (missingKey "jobId") "deploy intent should not include jobId"
  assert (missingKey "generatedAt") "deploy intent should not include generatedAt"


testNoIntentWhenUnchanged :: IO ()
testNoIntentWhenUnchanged = do

  tmpDir <- getTemporaryDirectory
  let workDir = tmpDir <> "/hostenv-provider-no-intent"
  let generatedDir = workDir <> "/generated"
  let previousPlanPath = generatedDir <> "/plan.json"
  createDirectoryIfMissing True generatedDir
  let planJson =
        BLC.pack
          "{\"hostenvHostname\":\"hosting.test\",\"nodes\":{\"node-a\":{}},\"environments\":{\"env-a\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"userName\":\"env-a\",\"projectNameHash\":\"hash-main\"},\"node\":\"node-a\"}}}"
  BLC.writeFile previousPlanPath planJson
  cmdRef <- newIORef ([] :: [CommandSpec])
  let runner = recordRunner cmdRef
  let cfg = WebhookConfig workDir previousPlanPath
  let proj = ProjectRef "acme" "site"
  result <- runWebhookWith (\_ -> pure ()) runner (pure planJson) cfg proj
  removePathForcibly workDir
  case result of
    Left err -> assert False ("unchanged webhook run failed: " <> show err)
    Right webhookResult -> do
      assert (webhookResult.intents == []) "unchanged plans should not emit deploy intents"

testReloadIntentSameNodeChange :: IO ()
testReloadIntentSameNodeChange = do
  tmpDir <- getTemporaryDirectory
  let workDir = tmpDir <> "/hostenv-provider-reload-intent"
  let generatedDir = workDir <> "/generated"
  let previousPlanPath = generatedDir <> "/plan.json"
  createDirectoryIfMissing True generatedDir
  let previousPlan =
        BLC.pack
          "{\"hostenvHostname\":\"hosting.test\",\"nodes\":{\"node-a\":{}},\"environments\":{\"env-a\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"userName\":\"env-a\",\"projectNameHash\":\"hash-main\"},\"node\":\"node-a\",\"version\":1}}}"
  let currentPlan =
        BLC.pack
          "{\"hostenvHostname\":\"hosting.test\",\"nodes\":{\"node-a\":{}},\"environments\":{\"env-a\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"userName\":\"env-a\",\"projectNameHash\":\"hash-main\"},\"node\":\"node-a\",\"version\":2}}}"
  BLC.writeFile previousPlanPath previousPlan
  cmdRef <- newIORef ([] :: [CommandSpec])
  let runner = recordRunner cmdRef
  let cfg = WebhookConfig workDir previousPlanPath
  let proj = ProjectRef "acme" "site"
  result <- runWebhookWith (\_ -> pure ()) runner (pure currentPlan) cfg proj
  removePathForcibly workDir
  case result of
    Left err -> assert False ("reload webhook run failed: " <> show err)
    Right webhookResult -> do
      let intentsByNode =
            Map.fromList
              [ (intent.node, map (.op) intent.actions)
              | intent <- webhookResult.intents
              ]
      assert
        (Map.findWithDefault [] "node-a" intentsByNode == ["reload"])
        "same-node payload changes should emit reload action"

testMoveIntentOrdering :: IO ()
testMoveIntentOrdering = do
  tmpDir <- getTemporaryDirectory
  let workDir = tmpDir <> "/hostenv-provider-move-order"
  let generatedDir = workDir <> "/generated"
  let previousPlanPath = generatedDir <> "/plan.json"
  createDirectoryIfMissing True generatedDir
  let previousPlan =
        BLC.pack
          "{\"hostenvHostname\":\"hosting.test\",\"nodes\":{\"node-a\":{},\"node-b\":{}},\"environments\":{\"env-a\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"userName\":\"env-a\",\"projectNameHash\":\"hash-main\"},\"node\":\"node-a\"}}}"
  let currentPlan =
        BLC.pack
          "{\"hostenvHostname\":\"hosting.test\",\"nodes\":{\"node-a\":{},\"node-b\":{}},\"environments\":{\"env-a\":{\"hostenv\":{\"organisation\":\"acme\",\"project\":\"site\",\"userName\":\"env-a\",\"projectNameHash\":\"hash-main\"},\"node\":\"node-b\",\"previousNode\":\"node-a\",\"migrations\":[\"db-migrate\"]}}}"
  BLC.writeFile previousPlanPath previousPlan
  cmdRef <- newIORef ([] :: [CommandSpec])
  let runner = recordRunner cmdRef
  let cfg = WebhookConfig workDir previousPlanPath
  let proj = ProjectRef "acme" "site"
  result <- runWebhookWith (\_ -> pure ()) runner (pure currentPlan) cfg proj
  removePathForcibly workDir
  case result of
    Left err -> assert False ("move webhook run failed: " <> show err)
    Right webhookResult -> do
      let intentsByNode =
            Map.fromList
              [ (intent.node, map (.op) intent.actions)
              | intent <- webhookResult.intents
              ]
      assert
        (Map.findWithDefault [] "node-b" intentsByNode == ["restore", "activate"])
        "destination node should run restore before activate"
      assert
        (Map.findWithDefault [] "node-a" intentsByNode == ["backup", "deactivate"])
        "source node should run backup before deactivate"

mkRepoConfig :: FilePath -> AppConfig
mkRepoConfig dataDir =
  AppConfig
    { appDataDir = dataDir
    , appFlakeRoot = "."
    , appListenSocket = "/tmp/provider.sock"
    , appWebhookSecretFile = Nothing
    , appWebhookSecretsDir = Nothing
    , appWebhookConfig = WebhookConfig dataDir (dataDir <> "/generated/plan.json")
    , appUiBasePath = "/dashboard"
    , appUiBaseUrl = "https://example.invalid"
    , appWebhookHost = "example.invalid"
    , appDbConnString = Nothing
    , appGitlabSecrets = Nothing
    , appGitlabHosts = ["gitlab.com"]
    , appGitlabTokenCipher = Nothing
    , appGitlabDeployTokenTtlMinutes = 15
    , appGitConfigPath = dataDir <> "/gitconfig"
    , appGitCredentialsPath = dataDir <> "/git-credentials"
    , appFlakeTemplate = "flake.template.nix"
    , appSeedUsers = []
    , appJobsRetentionDays = 30
    , appJobsCleanupIntervalMins = 1440
    , appJobsWaitTimeoutMins = 120
    , appJobsWaitInterval = 60
    , appDeploy =
        DeployConfig
          { enable = True
          , nodeAuthTokens = Map.empty
          }
    , appHttpManager = Nothing
    }
