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
import Hostenv.Provider.Config (AppConfig(..), CominConfig(..), loadConfig)
import Hostenv.Provider.Crypto
import Hostenv.Provider.DB (OAuthCredential(..))
import Hostenv.Provider.DeployApi (acceptsNodeEvents, extractBearer, normalizeStatus, validateIntent)
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
import Hostenv.Provider.PrevNodeDiscovery
import Hostenv.Provider.Repo (RepoStatus(..), ensureProviderRepo, isAuthFailure)
import Hostenv.Provider.Service
import Hostenv.Provider.Webhook (chooseFinalResult, persistIntentsActionsAndPushWith, shouldWaitForCallbacks)

assert :: Bool -> String -> IO ()
assert cond msg = unless cond $ do
  putStrLn ("FAIL: " <> msg)
  exitFailure

main :: IO ()
main = do
  testGitHubSig
  testGitLabToken
  testDeployApiBearerParsing
  testDeployApiStatusNormalization
  testDeployApiEventAcceptance
  testDeployApiIntentValidation
  testPlanParsing
  testNodeOrderWithMigrations
  testNodeOrderWithDnsSkipsNonMigratingEnvDiscovery
  testProjectHashSelection
  testPrevNodeDiscoveryResolution
  testCommandSequence
  testTemplateRender
  testGitCredentials
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
            , CommandSpec "nix" ["flake", "update", "acme__site"] "/tmp/provider"
            , CommandSpec "nix" ["run", ".#hostenv-provider", "--", "plan"] "/tmp/provider"
            , CommandSpec "nix" ["run", ".#hostenv-provider", "--", "dns-gate"] "/tmp/provider"
            , CommandSpec "git" ["add", "-A", "generated"] "/tmp/provider"
            , CommandSpec "git" ["add", "-A", "flake.nix", "flake.lock"] "/tmp/provider"
            , CommandSpec "git" ["status", "--porcelain", "--", "generated", "flake.nix", "flake.lock"] "/tmp/provider"
            , CommandSpec "git" ["commit", "-m", "hostenv-provider: deploy intent for acme/site"] "/tmp/provider"
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
  assert (shouldWaitForCallbacks WebhookUpdateCommitted True oneIntent) "waiting should require committed update, comin enabled, and non-empty intents"
  assert (not (shouldWaitForCallbacks WebhookUpdateNoop True oneIntent)) "waiting should be skipped when commit is not pushed"
  assert (not (shouldWaitForCallbacks WebhookUpdateCommitted False oneIntent)) "waiting should be skipped when comin is disabled"
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
        , "comin" A..= A.object [ "enable" A..= False ]
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
  createDirectory path
  let cfg = mkRepoConfig path
  result <- ensureProviderRepo cfg
  removePathForcibly path
  case result of
    Left msg -> assert ("not a git checkout" `T.isInfixOf` msg) "invalid repo dir should report git checkout error"
    other -> assert False ("expected checkout error, got " <> show other)


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
    , appComin =
        CominConfig
          { enable = True
          , branch = "main"
          , pollIntervalSeconds = 30
          , nodeAuthTokens = Map.empty
          }
    , appHttpManager = Nothing
    }
