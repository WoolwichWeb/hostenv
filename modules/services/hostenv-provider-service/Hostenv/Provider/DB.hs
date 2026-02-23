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
  , getSessionInfo
  , createSession
  , renderSessionCookie
  , logoutCookie
  ) where

import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Binary (..), Connection, Only (..), Query, close, connectPostgreSQL, execute, execute_, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
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
        ]
  forM_ migrations (execute_ conn)

normalizeIdentity :: Text -> Text
normalizeIdentity = T.toLower . T.strip

normalizeRole :: Text -> Text
normalizeRole roleText =
  if normalizeIdentity roleText == "user" then "user" else "admin"

syncUsers :: AppConfig -> IO ()
syncUsers cfg = withDb cfg (syncUsersConn cfg)

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

decodeOAuthRows :: AppConfig -> [StoredOAuthCredentialRow] -> Either Text (Maybe OAuthCredential)
decodeOAuthRows _ [] = Right Nothing
decodeOAuthRows cfg (row:_) = Just <$> decodeOAuthCredential cfg row

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
