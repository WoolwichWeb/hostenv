{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.DB
  ( User(..)
  , SessionInfo(..)
  , ProviderAccountMatch(..)
  , ProjectRow(..)
  , OAuthCredential(..)
  , DeployCredential(..)
  , DeployEvent(..)
  , withDb
  , ensureSchema
  , syncUsers
  , syncUsersConn
  , loadProjects
  , upsertOAuthCredential
  , loadLatestOAuthCredential
  , upsertProjectOAuthCredential
  , lookupProviderAccount
  , setProviderUserId
  , loadDeployCredentialByHash
  , saveDeployIntents
  , loadDeployIntentByJob
  , loadDeployIntentBySha
  , loadDeployIntentNodes
  , deployIntentExists
  , saveDeployActions
  , applyDeployActionEvent
  , DeployAction(..)
  , loadDeployActions
  , loadDeployActionsByNode
  , DeployStatus(..)
  , loadDeployStatuses
  , loadDeployBackupSnapshot
  , loadDeployStatusByNode
  , loadDeployEventsSince
  , appendDeployEvent
  , getSessionInfo
  , createSession
  , renderSessionCookie
  , logoutCookie
  ) where

import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Binary (..), Connection, In (..), Only (..), Query, close, connectPostgreSQL, execute, execute_, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Data.Maybe (listToMaybe, mapMaybe)
import Network.Wai (Request)
import qualified Network.Wai as Wai
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, renderSetCookie, sameSiteLax)

import Hostenv.Provider.Config (AppConfig(..), ProviderAccountConfig(..), UserConfig(..))
import Hostenv.Provider.Crypto (TokenCipher(..), decryptToken, encryptToken, encryptedTokenCiphertext, encryptedTokenKeyId, encryptedTokenNonce)
import Hostenv.Provider.Util (randomToken)

sessionCookieName :: BS.ByteString
sessionCookieName = "hostenv_session"


data User = User
  { id :: Int
  , username :: Text
  , email :: Maybe Text
  , role :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

data ProviderAccountMatch = ProviderAccountMatch
  { rowId :: Int
  , userId :: Int
  , externalUserId :: Maybe Int64
  } deriving (Eq, Show)


data SessionInfo = SessionInfo
  { id :: Text
  , csrf :: Text
  , user :: User
  }


data ProjectRow = ProjectRow
  { id :: Int
  , org :: Text
  , name :: Text
  , gitHost :: Text
  , repoId :: Int64
  , repoUrl :: Text
  , repoPath :: Text
  , flakeInput :: Text
  , hash :: Maybe Text
  }

instance FromRow ProjectRow where
  fromRow = ProjectRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data OAuthCredential = OAuthCredential
  { host :: Text
  , accessToken :: Text
  , refreshToken :: Maybe Text
  , tokenType :: Maybe Text
  , scopes :: Text
  , expiresAt :: Maybe UTCTime
  } deriving (Eq, Show)

data DeployCredential = DeployCredential
  { projectId :: Int
  , repoId :: Int64
  , host :: Text
  , accessToken :: Text
  , refreshToken :: Maybe Text
  , tokenType :: Maybe Text
  , scopes :: Text
  , expiresAt :: Maybe UTCTime
  } deriving (Eq, Show)

data DeployEvent = DeployEvent
  { id :: Int64
  , node :: Text
  , status :: Text
  , phase :: Maybe Text
  , message :: Maybe Text
  , payload :: A.Value
  , createdAt :: UTCTime
  } deriving (Eq, Show)

instance FromRow DeployEvent where
  fromRow =
    DeployEvent
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance A.ToJSON DeployEvent where
  toJSON event =
    A.object
      [ "id" A..= event.id
      , "node" A..= event.node
      , "status" A..= event.status
      , "phase" A..= event.phase
      , "message" A..= event.message
      , "payload" A..= event.payload
      , "createdAt" A..= event.createdAt
      ]

data DeployStatus = DeployStatus
  { eventId :: Int64
  , node :: Text
  , status :: Text
  , phase :: Maybe Text
  , message :: Maybe Text
  , createdAt :: UTCTime
  } deriving (Eq, Show)

instance FromRow DeployStatus where
  fromRow =
    DeployStatus
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance A.ToJSON DeployStatus where
  toJSON s =
    A.object
      [ "eventId" A..= s.eventId
      , "node" A..= s.node
      , "status" A..= s.status
      , "phase" A..= s.phase
      , "message" A..= s.message
      , "createdAt" A..= s.createdAt
      ]

data DeployAction = DeployAction
  { node :: Text
  , actionIndex :: Int
  , op :: Text
  , user :: Text
  , status :: Text
  , message :: Maybe Text
  , startedAt :: Maybe UTCTime
  , finishedAt :: Maybe UTCTime
  , updatedAt :: UTCTime
  } deriving (Eq, Show)

instance FromRow DeployAction where
  fromRow =
    DeployAction
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance A.ToJSON DeployAction where
  toJSON action =
    A.object
      [ "node" A..= action.node
      , "actionIndex" A..= action.actionIndex
      , "op" A..= action.op
      , "user" A..= action.user
      , "status" A..= action.status
      , "message" A..= action.message
      , "startedAt" A..= action.startedAt
      , "finishedAt" A..= action.finishedAt
      , "updatedAt" A..= action.updatedAt
      ]

data StoredOAuthCredentialRow = StoredOAuthCredentialRow
  { host :: Text
  , accessEncrypted :: BS.ByteString
  , accessNonce :: BS.ByteString
  , refreshEncrypted :: Maybe BS.ByteString
  , refreshNonce :: Maybe BS.ByteString
  , keyId :: Text
  , tokenType :: Maybe Text
  , scopes :: Text
  , expiresAt :: Maybe UTCTime
  }

instance FromRow StoredOAuthCredentialRow where
  fromRow =
    StoredOAuthCredentialRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field


withDb :: AppConfig -> (Connection -> IO a) -> IO a
withDb cfg action =
  let AppConfig { appDbConnString = dbConn } = cfg
   in case dbConn of
    Nothing -> error "Database not configured"
    Just connStr -> do
      conn <- connectPostgreSQL connStr
      result <- action conn
      close conn
      pure result

ensureSchema :: AppConfig -> IO ()
ensureSchema cfg = withDb cfg $ \conn -> do
  legacyRows <- query_ conn "SELECT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = 'public' AND table_name = 'users' AND column_name = 'gitlab_user_id')"
  let resetLegacy = case legacyRows of
        (Only True:_) -> True
        _ -> False
  when resetLegacy $ do
    let resetQueries :: [Query]
        resetQueries =
          [ "DROP TABLE IF EXISTS gitlab_tokens;"
          , "DROP TABLE IF EXISTS project_oauth_credentials;"
          , "DROP TABLE IF EXISTS oauth_credentials;"
          , "DROP TABLE IF EXISTS sessions;"
          , "DROP TABLE IF EXISTS provider_users;"
          , "DROP TABLE IF EXISTS users;"
          , "DROP TABLE IF EXISTS projects;"
          , "DROP TABLE IF EXISTS oauth_states;"
          , "DROP TABLE IF EXISTS webhooks;"
          , "DROP TYPE IF EXISTS provider_kind;"
          , "DROP TYPE IF EXISTS hostenv_role;"
          ]
    forM_ resetQueries (execute_ conn)

  -- No backward compatibility for old token storage.
  _ <- execute_ conn "DROP TABLE IF EXISTS gitlab_tokens;"
  pure ()

  let migrations :: [Query]
      migrations =
        [ "DO $$ BEGIN CREATE TYPE hostenv_role AS ENUM ('admin','user'); EXCEPTION WHEN duplicate_object THEN null; END $$;"
        , "DO $$ BEGIN CREATE TYPE provider_kind AS ENUM ('gitlab'); EXCEPTION WHEN duplicate_object THEN null; END $$;"
        , "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, config_username TEXT UNIQUE NOT NULL, email TEXT, role hostenv_role NOT NULL DEFAULT 'admin', created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now());"
        , "CREATE TABLE IF NOT EXISTS provider_users (id SERIAL PRIMARY KEY, user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE, provider provider_kind NOT NULL, provider_host TEXT NOT NULL, provider_username TEXT NOT NULL, provider_user_id BIGINT, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE (provider, provider_host, provider_username));"
        , "CREATE UNIQUE INDEX IF NOT EXISTS provider_users_provider_host_user_id_unique ON provider_users (provider, provider_host, provider_user_id) WHERE provider_user_id IS NOT NULL;"
        , "CREATE TABLE IF NOT EXISTS sessions (id TEXT PRIMARY KEY, user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE, csrf_token TEXT NOT NULL, expires_at TIMESTAMPTZ NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now());"
        , "CREATE TABLE IF NOT EXISTS oauth_states (state TEXT PRIMARY KEY, git_host TEXT NOT NULL, expires_at TIMESTAMPTZ NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now());"
        , "CREATE TABLE IF NOT EXISTS projects (id SERIAL PRIMARY KEY, org TEXT NOT NULL, project TEXT NOT NULL, git_host TEXT NOT NULL, repo_id BIGINT NOT NULL, repo_url TEXT NOT NULL, repo_path TEXT NOT NULL, flake_input TEXT NOT NULL, default_env_hash TEXT, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE (git_host, repo_id), UNIQUE (org, project));"
        , "CREATE TABLE IF NOT EXISTS oauth_credentials (id SERIAL PRIMARY KEY, user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE, provider provider_kind NOT NULL, provider_host TEXT NOT NULL, access_token_encrypted BYTEA NOT NULL, access_token_nonce BYTEA NOT NULL, refresh_token_encrypted BYTEA, refresh_token_nonce BYTEA, token_key_id TEXT NOT NULL, token_type TEXT, scopes TEXT NOT NULL, expires_at TIMESTAMPTZ, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE (user_id, provider, provider_host));"
        , "CREATE TABLE IF NOT EXISTS project_oauth_credentials (id SERIAL PRIMARY KEY, project_id INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE, provider provider_kind NOT NULL, provider_host TEXT NOT NULL, access_token_encrypted BYTEA NOT NULL, access_token_nonce BYTEA NOT NULL, refresh_token_encrypted BYTEA, refresh_token_nonce BYTEA, token_key_id TEXT NOT NULL, token_type TEXT, scopes TEXT NOT NULL, expires_at TIMESTAMPTZ, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE (project_id, provider, provider_host));"
        , "CREATE TABLE IF NOT EXISTS webhooks (id SERIAL PRIMARY KEY, project_id INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE, webhook_id BIGINT, webhook_url TEXT, secret TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE (project_id));"
        , "CREATE TABLE IF NOT EXISTS deploy_intents (job_id TEXT NOT NULL, commit_sha TEXT NOT NULL, node TEXT NOT NULL, intent JSONB NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), PRIMARY KEY (job_id, node));"
        , "CREATE INDEX IF NOT EXISTS deploy_intents_commit_node_idx ON deploy_intents (commit_sha, node, created_at DESC);"
        , "CREATE TABLE IF NOT EXISTS deploy_actions (job_id TEXT NOT NULL, node TEXT NOT NULL, action_idx INTEGER NOT NULL, op TEXT NOT NULL, user_name TEXT NOT NULL, action JSONB NOT NULL, status TEXT NOT NULL, message TEXT, started_at TIMESTAMPTZ, finished_at TIMESTAMPTZ, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), PRIMARY KEY (job_id, node, action_idx));"
        , "CREATE INDEX IF NOT EXISTS deploy_actions_job_node_idx ON deploy_actions (job_id, node, action_idx);"
        , "CREATE TABLE IF NOT EXISTS deploy_node_events (id BIGSERIAL PRIMARY KEY, job_id TEXT NOT NULL, node TEXT NOT NULL, status TEXT NOT NULL, phase TEXT, message TEXT, payload JSONB NOT NULL DEFAULT '{}'::jsonb, created_at TIMESTAMPTZ NOT NULL DEFAULT now());"
        , "CREATE INDEX IF NOT EXISTS deploy_node_events_job_idx ON deploy_node_events (job_id, created_at DESC);"
        ]
  forM_ migrations (execute_ conn)

normalizeIdentity :: Text -> Text
normalizeIdentity = T.toLower . T.strip

normalizeRole :: Text -> Text
normalizeRole roleText =
  if normalizeIdentity roleText == "user" then "user" else "admin"

-- | Syncs users with those specified in provider Hostenv project
-- Nix configuration.
syncUsers :: AppConfig -> IO ()
syncUsers cfg = withDb cfg (syncUsersConn cfg)

-- | Syncs users with those specified in provider Hostenv project
-- Nix configuration.
syncUsersConn :: AppConfig -> Connection -> IO ()
syncUsersConn cfg conn = do
  userPairs <- mapM (upsertUser conn) cfg.appSeedUsers
  let desiredUsers = Map.fromList userPairs

  existingUsers <- query_ conn "SELECT id, config_username FROM users" :: IO [(Int, Text)]
  forM_ existingUsers $ \(existingUserId, existingConfigUsername) ->
    unless (Map.member existingConfigUsername desiredUsers) $ do
      _ <- execute conn "DELETE FROM users WHERE id = ?" (Only existingUserId)
      pure ()

  let desiredAccounts =
        [ (providerAccount acct, userId, acct.userId)
        | seedUser <- cfg.appSeedUsers
        , let configUsername = normalizeIdentity seedUser.configUsername
        , Just userId <- [Map.lookup configUsername desiredUsers]
        , acct <- seedUser.providerAccounts
        ]
      desiredAccountKeys = Set.fromList (map (\(key, _, _) -> key) desiredAccounts)

  forM_ desiredAccounts $ \((providerKind, providerHost, providerUsername), userId, providerUserId) -> do
    _ <- execute conn
      "INSERT INTO provider_users (user_id, provider, provider_host, provider_username, provider_user_id) VALUES (?, ?::provider_kind, ?, ?, ?) ON CONFLICT (provider, provider_host, provider_username) DO UPDATE SET user_id = EXCLUDED.user_id, provider_user_id = COALESCE(provider_users.provider_user_id, EXCLUDED.provider_user_id), updated_at = now()"
      (userId, providerKind, providerHost, providerUsername, providerUserId)
    pure ()

  existingAccounts <- query_ conn "SELECT id, provider::text, provider_host, provider_username FROM provider_users" :: IO [(Int, Text, Text, Text)]
  forM_ existingAccounts $ \(providerUserRowId, providerKind, providerHost, providerUsername) -> do
    let key = (normalizeIdentity providerKind, normalizeIdentity providerHost, normalizeIdentity providerUsername)
    unless (Set.member key desiredAccountKeys) $ do
      _ <- execute conn "DELETE FROM provider_users WHERE id = ?" (Only providerUserRowId)
      pure ()
  where
    providerAccount :: ProviderAccountConfig -> (Text, Text, Text)
    providerAccount acct =
      ( normalizeIdentity acct.provider
      , normalizeIdentity acct.host
      , normalizeIdentity acct.username
      )

upsertUser :: Connection -> UserConfig -> IO (Text, Int)
upsertUser conn seedUser = do
  let configUsername = normalizeIdentity seedUser.configUsername
      emailValue = case fmap T.strip seedUser.email of
        Just "" -> Nothing
        other -> other
      roleValue = normalizeRole seedUser.role
  rows <- query conn
    "INSERT INTO users (config_username, email, role) VALUES (?, ?, ?::hostenv_role) ON CONFLICT (config_username) DO UPDATE SET email = EXCLUDED.email, role = EXCLUDED.role, updated_at = now() RETURNING id"
    (configUsername, emailValue, roleValue)
  case rows of
    (Only userIdVal:_) -> pure (configUsername, userIdVal)
    _ -> error "seed user upsert failed to return id"

loadProjects :: Connection -> IO [ProjectRow]
loadProjects conn =
  query_ conn "SELECT id, org, project, git_host, repo_id, repo_url, repo_path, flake_input, default_env_hash FROM projects ORDER BY org, project"

upsertOAuthCredential :: AppConfig -> Connection -> Int -> OAuthCredential -> IO (Either Text ())
upsertOAuthCredential cfg conn userIdVal cred =
  upsertCredentialRow
    cfg
    conn
    "INSERT INTO oauth_credentials (user_id, provider, provider_host, access_token_encrypted, access_token_nonce, refresh_token_encrypted, refresh_token_nonce, token_key_id, token_type, scopes, expires_at) VALUES (?, 'gitlab'::provider_kind, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (user_id, provider, provider_host) DO UPDATE SET access_token_encrypted = EXCLUDED.access_token_encrypted, access_token_nonce = EXCLUDED.access_token_nonce, refresh_token_encrypted = EXCLUDED.refresh_token_encrypted, refresh_token_nonce = EXCLUDED.refresh_token_nonce, token_key_id = EXCLUDED.token_key_id, token_type = EXCLUDED.token_type, scopes = EXCLUDED.scopes, expires_at = EXCLUDED.expires_at, updated_at = now()"
    userIdVal
    cred

upsertProjectOAuthCredential :: AppConfig -> Connection -> Int -> OAuthCredential -> IO (Either Text ())
upsertProjectOAuthCredential cfg conn projectIdVal cred =
  upsertCredentialRow
    cfg
    conn
    "INSERT INTO project_oauth_credentials (project_id, provider, provider_host, access_token_encrypted, access_token_nonce, refresh_token_encrypted, refresh_token_nonce, token_key_id, token_type, scopes, expires_at) VALUES (?, 'gitlab'::provider_kind, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (project_id, provider, provider_host) DO UPDATE SET access_token_encrypted = EXCLUDED.access_token_encrypted, access_token_nonce = EXCLUDED.access_token_nonce, refresh_token_encrypted = EXCLUDED.refresh_token_encrypted, refresh_token_nonce = EXCLUDED.refresh_token_nonce, token_key_id = EXCLUDED.token_key_id, token_type = EXCLUDED.token_type, scopes = EXCLUDED.scopes, expires_at = EXCLUDED.expires_at, updated_at = now()"
    projectIdVal
    cred

upsertCredentialRow
  :: AppConfig
  -> Connection
  -> Query
  -> Int
  -> OAuthCredential
  -> IO (Either Text ())
upsertCredentialRow cfg conn queryText key cred =
  case cfg.appGitlabTokenCipher of
    Nothing -> pure (Left "GitLab token encryption key not configured")
    Just cipher -> do
      accessResult <- encryptToken cipher cred.accessToken
      case accessResult of
        Left err -> pure (Left err)
        Right accessEnc -> do
          refreshResult <- encryptOptionalToken cipher cred.refreshToken
          case refreshResult of
            Left err -> pure (Left err)
            Right (refreshEncrypted, refreshNonce) -> do
              _ <- execute conn
                queryText
                ( key
                , normalizeIdentity cred.host
                , Binary (encryptedTokenCiphertext accessEnc)
                , Binary (encryptedTokenNonce accessEnc)
                , fmap Binary refreshEncrypted
                , fmap Binary refreshNonce
                , encryptedTokenKeyId accessEnc
                , cred.tokenType
                , cred.scopes
                , cred.expiresAt
                )
              pure (Right ())

encryptOptionalToken :: TokenCipher -> Maybe Text -> IO (Either Text (Maybe BS.ByteString, Maybe BS.ByteString))
encryptOptionalToken _ Nothing = pure (Right (Nothing, Nothing))
encryptOptionalToken cipher (Just token) = do
  encrypted <- encryptToken cipher token
  case encrypted of
    Left err -> pure (Left err)
    Right enc -> pure (Right (Just (encryptedTokenCiphertext enc), Just (encryptedTokenNonce enc)))

loadLatestOAuthCredential :: AppConfig -> Connection -> Int -> IO (Either Text (Maybe OAuthCredential))
loadLatestOAuthCredential cfg conn userIdVal = do
  rows <- query conn
    "SELECT provider_host, access_token_encrypted, access_token_nonce, refresh_token_encrypted, refresh_token_nonce, token_key_id, token_type, scopes, expires_at FROM oauth_credentials WHERE user_id = ? AND provider = 'gitlab'::provider_kind ORDER BY updated_at DESC LIMIT 1"
    (Only userIdVal)
  pure (decodeOAuthRows cfg rows)

loadDeployCredentialByHash :: AppConfig -> Connection -> Text -> IO (Either Text (Maybe DeployCredential))
loadDeployCredentialByHash cfg conn hash = do
  rows <- query conn
    "SELECT projects.id, projects.repo_id, project_oauth_credentials.provider_host, project_oauth_credentials.access_token_encrypted, project_oauth_credentials.access_token_nonce, project_oauth_credentials.refresh_token_encrypted, project_oauth_credentials.refresh_token_nonce, project_oauth_credentials.token_key_id, project_oauth_credentials.token_type, project_oauth_credentials.scopes, project_oauth_credentials.expires_at FROM projects JOIN LATERAL (SELECT provider_host, access_token_encrypted, access_token_nonce, refresh_token_encrypted, refresh_token_nonce, token_key_id, token_type, scopes, expires_at FROM project_oauth_credentials WHERE project_id = projects.id AND provider = 'gitlab'::provider_kind ORDER BY updated_at DESC LIMIT 1) AS project_oauth_credentials ON TRUE WHERE projects.default_env_hash = ? LIMIT 1"
    (Only hash)
  case rows of
    [] -> pure (Right Nothing)
    ((projectIdVal, repoIdVal, providerHost, accessEncrypted, accessNonce, refreshEncrypted, refreshNonce, tokenKeyId, tokenType, scopes, expiresAt):_) -> do
      let stored =
            StoredOAuthCredentialRow
              { host = providerHost
              , accessEncrypted = accessEncrypted
              , accessNonce = accessNonce
              , refreshEncrypted = refreshEncrypted
              , refreshNonce = refreshNonce
              , keyId = tokenKeyId
              , tokenType = tokenType
              , scopes = scopes
              , expiresAt = expiresAt
              }
      case decodeOAuthCredential cfg stored of
        Left err -> pure (Left err)
        Right cred ->
          pure
            ( Right
                ( Just
                    DeployCredential
                      { projectId = projectIdVal
                      , repoId = repoIdVal
                      , host = cred.host
                      , accessToken = cred.accessToken
                      , refreshToken = cred.refreshToken
                      , tokenType = cred.tokenType
                      , scopes = cred.scopes
                      , expiresAt = cred.expiresAt
                      }
                )
            )

saveDeployIntents :: AppConfig -> Text -> Text -> [(Text, A.Value)] -> IO ()
saveDeployIntents cfg jobId commitSha intents =
  withDb cfg $ \conn ->
    forM_ intents $ \(node, intent) -> do
      _ <- execute conn
        "INSERT INTO deploy_intents (job_id, commit_sha, node, intent, created_at) VALUES (?, ?, ?, ?::jsonb, now()) ON CONFLICT (job_id, node) DO UPDATE SET commit_sha = EXCLUDED.commit_sha, intent = EXCLUDED.intent, created_at = now()"
        (jobId, commitSha, node, encodeJsonText intent)
      pure ()

saveDeployActions :: AppConfig -> Text -> [(Text, A.Value)] -> IO ()
saveDeployActions cfg jobId intents =
  withDb cfg $ \conn -> do
    _ <- execute conn "DELETE FROM deploy_actions WHERE job_id = ?" (Only jobId)
    forM_ intents $ \(nodeName, intentValue) ->
      forM_ (extractActions intentValue) $ \(actionIndex, op, userName, actionValue) -> do
        _ <- execute conn
          "INSERT INTO deploy_actions (job_id, node, action_idx, op, user_name, action, status, message, started_at, finished_at, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?::jsonb, 'queued', ?, NULL, NULL, now(), now())"
          (jobId, nodeName, actionIndex, op, userName, encodeJsonText actionValue, Just ("Queued action " <> op <> " for " <> userName))
        pure ()
  where
    extractActions :: A.Value -> [(Int, Text, Text, A.Value)]
    extractActions value =
      case value of
        A.Object obj ->
          case KM.lookup (K.fromString "actions") obj of
            Just (A.Array arr) ->
              mapMaybe extractAction (zip [0 ..] (toList arr))
            _ -> []
        _ -> []

    extractAction :: (Int, A.Value) -> Maybe (Int, Text, Text, A.Value)
    extractAction (idx, actionValue) =
      case actionValue of
        A.Object actionObj ->
          case (KM.lookup (K.fromString "op") actionObj, KM.lookup (K.fromString "user") actionObj) of
            (Just (A.String op), Just (A.String userName))
              | T.strip op /= "" && T.strip userName /= "" -> Just (idx, T.toLower (T.strip op), T.strip userName, actionValue)
            _ -> Nothing
        _ -> Nothing

loadDeployActions :: AppConfig -> Text -> IO [DeployAction]
loadDeployActions cfg jobId =
  withDb cfg $ \conn ->
    query conn
      "SELECT node, action_idx, op, user_name, status, message, started_at, finished_at, updated_at FROM deploy_actions WHERE job_id = ? ORDER BY node, action_idx"
      (Only jobId)

loadDeployActionsByNode :: AppConfig -> Text -> Text -> IO [DeployAction]
loadDeployActionsByNode cfg jobId nodeName =
  withDb cfg $ \conn ->
    query conn
      "SELECT node, action_idx, op, user_name, status, message, started_at, finished_at, updated_at FROM deploy_actions WHERE job_id = ? AND node = ? ORDER BY action_idx"
      (jobId, nodeName)

applyDeployActionEvent :: AppConfig -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> IO ()
applyDeployActionEvent cfg jobId nodeName rawStatus rawPhase mMessage =
  withDb cfg $ \conn -> do
    let status = T.toLower (T.strip rawStatus)
        phase = fmap (T.toLower . T.strip) rawPhase
        message = fmap T.strip mMessage >>= \msg -> if msg == "" then Nothing else Just msg
    case phase of
      Nothing -> pure ()
      Just "" -> pure ()
      Just "intent" ->
        case status of
          "success" -> do
            _ <- execute conn
              "UPDATE deploy_actions SET status = 'success', message = COALESCE(?, message), started_at = COALESCE(started_at, now()), finished_at = COALESCE(finished_at, now()), updated_at = now() WHERE job_id = ? AND node = ? AND status IN ('queued','waiting','running')"
              (message, jobId, nodeName)
            pure ()
          "failed" -> finalizeRemaining conn "failed" message
          "timed_out" -> finalizeRemaining conn "timed_out" message
          _ -> pure ()
      Just op ->
        case status of
          "waiting" -> transitionOne conn op (In ["queued" :: Text, "running"]) "waiting" True False message
          "running" -> transitionOne conn op (In ["queued" :: Text, "waiting"]) "running" True False message
          "success" -> transitionOne conn op (In ["running" :: Text, "waiting", "queued"]) "success" True True message
          "failed" -> failAndFinalize conn op "failed" message
          "timed_out" -> failAndFinalize conn op "timed_out" message
          _ -> pure ()
  where
    finalizeRemaining :: Connection -> Text -> Maybe Text -> IO ()
    finalizeRemaining conn finalStatus message = do
      _ <- execute conn
        "UPDATE deploy_actions SET status = ?, message = COALESCE(?, message), started_at = COALESCE(started_at, now()), finished_at = COALESCE(finished_at, now()), updated_at = now() WHERE job_id = ? AND node = ? AND status IN ('queued','waiting','running')"
        (finalStatus, message, jobId, nodeName)
      pure ()

    transitionOne :: Connection -> Text -> In [Text] -> Text -> Bool -> Bool -> Maybe Text -> IO ()
    transitionOne conn op eligibleStatuses newStatus setStarted setFinished message = do
      candidates <- query conn
        "SELECT action_idx FROM deploy_actions WHERE job_id = ? AND node = ? AND op = ? AND status IN ? ORDER BY action_idx LIMIT 1"
        (jobId, nodeName, op, eligibleStatuses) :: IO [Only Int]
      case candidates of
        (Only actionIndex:_) -> do
          _ <- execute conn
            "UPDATE deploy_actions SET status = ?, message = COALESCE(?, message), started_at = CASE WHEN ? THEN COALESCE(started_at, now()) ELSE started_at END, finished_at = CASE WHEN ? THEN now() ELSE finished_at END, updated_at = now() WHERE job_id = ? AND node = ? AND action_idx = ?"
            (newStatus, message, setStarted, setFinished, jobId, nodeName, actionIndex)
          pure ()
        _ -> pure ()

    failAndFinalize :: Connection -> Text -> Text -> Maybe Text -> IO ()
    failAndFinalize conn op finalStatus message = do
      transitionOne conn op (In ["running" :: Text, "waiting", "queued"]) finalStatus True True message
      finalizeRemaining conn finalStatus message

-- | Load deployment intentions from a node reference.
-- For example: backend01, webserver13, and so on.
loadDeployIntentByJob :: AppConfig -> Text -> Text -> IO (Maybe A.Value)
loadDeployIntentByJob cfg jobId node =
  withDb cfg $ \conn -> do
    rows <- query conn
      "SELECT intent::text FROM deploy_intents WHERE job_id = ? AND node = ? ORDER BY created_at DESC LIMIT 1"
      (jobId, node)
    pure (listToMaybe rows >>= decodeJsonOnly)

-- | Load deployment intentions from a git commit ref.
loadDeployIntentBySha :: AppConfig -> Text -> Text -> IO (Maybe (Text, A.Value))
loadDeployIntentBySha cfg commitSha node =
  withDb cfg $ \conn -> do
    rows <- query conn
      "SELECT job_id, intent::text FROM deploy_intents WHERE commit_sha = ? AND node = ? ORDER BY created_at DESC LIMIT 1"
      (commitSha, node)
    pure $
      listToMaybe rows >>= \(jobId, payloadText) ->
        (\payload -> (jobId, payload)) <$> decodeJsonText payloadText

loadDeployIntentNodes :: AppConfig -> Text -> IO [Text]
loadDeployIntentNodes cfg jobId =
  withDb cfg $ \conn -> do
    rows <- query conn
      "SELECT node FROM deploy_intents WHERE job_id = ? ORDER BY node"
      (Only jobId)
    pure (map fromOnly rows)

-- | Returns whether deployment intentions were recorded for the given job id.
deployIntentExists :: AppConfig -> Text -> Text -> IO Bool
deployIntentExists cfg jobId node =
  withDb cfg $ \conn -> do
    rows <- query conn
      "SELECT EXISTS (SELECT 1 FROM deploy_intents WHERE job_id = ? AND node = ?)"
      (jobId, node)
    pure
      ( case rows of
          (Only existsVal:_) -> existsVal
          _ -> False
      )

loadDeployEventsSince :: AppConfig -> Text -> Int64 -> IO [DeployEvent]
loadDeployEventsSince cfg jobId afterId =
  withDb cfg $ \conn ->
    query conn
      "SELECT id, node, status, phase, message, payload, created_at FROM deploy_node_events WHERE job_id = ? AND id > ? ORDER BY id"
      (jobId, afterId)

loadDeployStatuses :: AppConfig -> Text -> IO [DeployStatus]
loadDeployStatuses cfg jobId =
  withDb cfg $ \conn ->
    query conn
      "SELECT id, node, status, phase, message, created_at FROM (SELECT DISTINCT ON (node) id, node, status, phase, message, created_at FROM deploy_node_events WHERE job_id = ? ORDER BY node, id DESC) latest ORDER BY node"
      (Only jobId)

loadDeployBackupSnapshot :: AppConfig -> Text -> Text -> Text -> IO (Maybe A.Value)
loadDeployBackupSnapshot cfg jobId sourceNode userName =
  withDb cfg $ \conn -> do
    rows <- query conn
      "SELECT payload FROM deploy_node_events WHERE job_id = ? AND node = ? AND phase = 'backup' AND status = 'success' AND payload ->> 'user' = ? ORDER BY id DESC LIMIT 1"
      (jobId, sourceNode, userName)
    pure (listToMaybe (map fromOnly rows))

loadDeployStatusByNode :: AppConfig -> Text -> Text -> IO (Maybe DeployStatus)
loadDeployStatusByNode cfg jobId nodeName =
  withDb cfg $ \conn -> do
    rows <- query conn
      "SELECT id, node, status, phase, message, created_at FROM deploy_node_events WHERE job_id = ? AND node = ? ORDER BY id DESC LIMIT 1"
      (jobId, nodeName)
    pure (listToMaybe rows)

appendDeployEvent :: AppConfig -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe A.Value -> IO DeployEvent
appendDeployEvent cfg jobId node status phase message payload =
  withDb cfg $ \conn -> do
    rows <- query conn
      "INSERT INTO deploy_node_events (job_id, node, status, phase, message, payload) VALUES (?, ?, ?, ?, ?, ?::jsonb) RETURNING id, node, status, phase, message, payload, created_at"
      (jobId, node, status, phase, message, encodeJsonText (maybe (A.object []) id payload))
    case rows of
      (event:_) -> pure event
      _ -> error "appendDeployEvent failed to return inserted row"

decodeOAuthRows :: AppConfig -> [StoredOAuthCredentialRow] -> Either Text (Maybe OAuthCredential)
decodeOAuthRows _ [] = Right Nothing
decodeOAuthRows cfg (row:_) = Just <$> decodeOAuthCredential cfg row

encodeJsonText :: A.Value -> Text
encodeJsonText = TE.decodeUtf8 . BL.toStrict . A.encode

decodeJsonText :: Text -> Maybe A.Value
decodeJsonText text =
  case A.eitherDecode' (BL.fromStrict (TE.encodeUtf8 text)) of
    Left _ -> Nothing
    Right value -> Just value

decodeJsonOnly :: Only Text -> Maybe A.Value
decodeJsonOnly (Only text) = decodeJsonText text

decodeOAuthCredential :: AppConfig -> StoredOAuthCredentialRow -> Either Text OAuthCredential
decodeOAuthCredential cfg row =
  case cfg.appGitlabTokenCipher of
    Nothing -> Left "GitLab token encryption key not configured"
    Just cipher -> do
      access <- decryptToken cipher row.accessNonce row.accessEncrypted row.keyId
      refresh <- decodeOptionalRefresh cipher row
      Right
        OAuthCredential
          { host = row.host
          , accessToken = access
          , refreshToken = refresh
          , tokenType = row.tokenType
          , scopes = row.scopes
          , expiresAt = row.expiresAt
          }

decodeOptionalRefresh :: TokenCipher -> StoredOAuthCredentialRow -> Either Text (Maybe Text)
decodeOptionalRefresh cipher row =
  case (row.refreshEncrypted, row.refreshNonce) of
    (Nothing, Nothing) -> Right Nothing
    (Just encrypted, Just nonce) -> Just <$> decryptToken cipher nonce encrypted row.keyId
    _ -> Left "OAuth refresh token row is malformed"

lookupProviderAccount :: Connection -> Text -> Text -> Text -> IO (Maybe ProviderAccountMatch)
lookupProviderAccount conn providerKind providerHost providerUsername = do
  rows <- query conn
    "SELECT provider_users.id, users.id, provider_users.provider_user_id FROM provider_users JOIN users ON users.id = provider_users.user_id WHERE provider_users.provider = ?::provider_kind AND provider_users.provider_host = ? AND provider_users.provider_username = ? LIMIT 1"
    (normalizeIdentity providerKind, normalizeIdentity providerHost, normalizeIdentity providerUsername)
  case rows of
    [] -> pure Nothing
    ((providerRowId, userIdVal, providerUserId):_) ->
      pure
        ( Just
            ProviderAccountMatch
              { rowId = providerRowId
              , userId = userIdVal
              , externalUserId = providerUserId
              }
        )

setProviderUserId :: Connection -> Int -> Int64 -> IO ()
setProviderUserId conn providerRowId providerUserId = do
  _ <- execute conn
    "UPDATE provider_users SET provider_user_id = ?, updated_at = now() WHERE id = ?"
    (providerUserId, providerRowId)
  pure ()

getSessionInfo :: AppConfig -> Request -> IO (Maybe SessionInfo)
getSessionInfo cfg req = do
  let cookieHeader = lookup "Cookie" (Wai.requestHeaders req)
  let mSessionId = cookieHeader >>= lookup sessionCookieName . parseCookies
  case mSessionId of
    Nothing -> pure Nothing
    Just raw -> do
      let sid = TE.decodeUtf8 raw
      withDb cfg $ \conn -> do
        now <- getCurrentTime
        rows <- query conn
          "SELECT sessions.id, sessions.csrf_token, sessions.expires_at, users.id, users.config_username, users.email, users.role::text FROM sessions JOIN users ON users.id = sessions.user_id WHERE sessions.id = ?"
          (Only sid)
        case rows of
          [] -> pure Nothing
          ((sessId, csrfToken, expiresAt, uId, uConfigUsername, uEmail, uRole):_) ->
            if expiresAt <= now
              then do
                _ <- execute conn "DELETE FROM sessions WHERE id = ?" (Only sessId)
                pure Nothing
              else do
                let user = User uId uConfigUsername uEmail uRole
                pure (Just (SessionInfo sessId csrfToken user))

renderSessionCookie :: SessionInfo -> BS.ByteString
renderSessionCookie sess =
  let cookie =
        defaultSetCookie
          { setCookieName = sessionCookieName
          , setCookieValue = TE.encodeUtf8 sess.id
          , setCookiePath = Just "/"
          , setCookieHttpOnly = True
          , setCookieSecure = True
          , setCookieSameSite = Just sameSiteLax
          }
   in renderSetCookieBytes cookie

logoutCookie :: BS.ByteString
logoutCookie =
  renderSetCookieBytes
    defaultSetCookie
      { setCookieName = sessionCookieName
      , setCookieValue = ""
      , setCookiePath = Just "/"
      , setCookieMaxAge = Just 0
      }

renderSetCookieBytes :: SetCookie -> BS.ByteString
renderSetCookieBytes = BL.toStrict . toLazyByteString . renderSetCookie

createSession :: Connection -> Int -> IO SessionInfo
createSession conn userIdVal = do
  sid <- randomToken 24
  csrf <- randomToken 16
  now <- getCurrentTime
  let expires = addUTCTime (60 * 60 * 24 * 7) now
  _ <- execute conn
    "INSERT INTO sessions (id, user_id, csrf_token, expires_at) VALUES (?, ?, ?, ?)"
    (sid, userIdVal, csrf, expires)
  rows <- query conn "SELECT id, config_username, email, role::text FROM users WHERE id = ?" (Only userIdVal)
  case rows of
    (user:_) -> pure (SessionInfo sid csrf user)
    _ -> error "session user not found"
