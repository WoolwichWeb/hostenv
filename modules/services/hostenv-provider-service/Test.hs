{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import Control.Monad (unless)
import "crypton" Crypto.Hash (SHA256)
import "crypton" Crypto.MAC.HMAC (HMAC (..), hmac)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (exitFailure)
import System.IO (hClose, openTempFile)

import Hostenv.Provider.Crypto
import Hostenv.Provider.PrevNodeDiscovery
import Hostenv.Provider.Service

assert :: Bool -> String -> IO ()
assert cond msg = unless cond $ do
  putStrLn ("FAIL: " <> msg)
  exitFailure

main :: IO ()
main = do
  testGitHubSig
  testGitLabToken
  testPlanParsing
  testNodeOrderWithMigrations
  testNodeOrderWithDnsSkipsNonMigratingEnvDiscovery
  testProjectHashSelection
  testPrevNodeDiscoveryResolution
  testCommandSequence
  testTemplateRender
  testGitCredentials
  testReadGitlabSecrets
  testTokenEncryptionRoundtrip
  testTokenKeyLoading
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
  let runner = recordRunner ref
  let cfg = WebhookConfig "/tmp/provider" "/tmp/provider/generated/plan.json"
  let proj = ProjectRef "acme" "site"
  result <- runWebhookWith runner (pure planJson) cfg proj
  case result of
    Left err -> assert False ("webhook run failed: " <> show err)
    Right res -> do
      assert (res.webhookOk) "webhook run should succeed"
      cmds <- readIORef ref
      let expected =
            [ CommandSpec "nix" ["flake", "update", "acme__site"] "/tmp/provider"
            , CommandSpec "nix" ["run", ".#hostenv-provider", "--", "plan"] "/tmp/provider"
            , CommandSpec "nix" ["run", ".#hostenv-provider", "--", "dns-gate"] "/tmp/provider"
            , CommandSpec "nix" ["run", ".#hostenv-provider", "--", "deploy", "--node", "node-a"] "/tmp/provider"
            , CommandSpec "nix" ["run", ".#hostenv-provider", "--", "deploy", "--node", "node-b"] "/tmp/provider"
            ]
      assert (cmds == expected) "webhook command sequence should match"


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
