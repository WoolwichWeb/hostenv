{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.DB
  ( User(..)
  , SessionInfo(..)
  , ProjectRow(..)
  , GitlabAccessToken(..)
  , ProjectDeployCredential(..)
  , withDb
  , ensureSchema
  , loadProjects
  , saveGitlabToken
  , loadLatestUserGitlabToken
  , loadLatestProjectGitlabToken
  , loadProjectDeployCredentialByHash
  , getSessionInfo
  , createSession
  , renderSessionCookie
  , logoutCookie
  ) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Binary (..), Connection, Only (..), Query, close, connectPostgreSQL, execute, execute_, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Network.Wai (Request)
import qualified Network.Wai as Wai
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, renderSetCookie, sameSiteLax)

import Hostenv.Provider.Config (AppConfig(..))
import Hostenv.Provider.Crypto (TokenCipher(..), decryptToken, encryptToken, encryptedTokenCiphertext, encryptedTokenKeyId, encryptedTokenNonce)
import Hostenv.Provider.Util (randomToken)

sessionCookieName :: BS.ByteString
sessionCookieName = "hostenv_session"


data User = User
  { userId :: Int
  , userGitlabId :: Int64
  , userUsername :: Text
  , userName :: Text
  , userRole :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field


data SessionInfo = SessionInfo
  { sessionId :: Text
  , sessionCsrf :: Text
  , sessionUser :: User
  }


data ProjectRow = ProjectRow
  { projectId :: Int
  , projectOrg :: Text
  , projectName :: Text
  , projectGitHost :: Text
  , projectRepoId :: Int64
  , projectRepoUrl :: Text
  , projectRepoPath :: Text
  , projectFlakeInput :: Text
  , projectHash :: Maybe Text
  }

instance FromRow ProjectRow where
  fromRow = ProjectRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data GitlabAccessToken = GitlabAccessToken
  { gitlabTokenHost :: Text
  , gitlabTokenValue :: Text
  , gitlabTokenScopes :: Text
  } deriving (Eq, Show)

data ProjectDeployCredential = ProjectDeployCredential
  { deployCredProjectId :: Int
  , deployCredRepoId :: Int64
  , deployCredHost :: Text
  , deployCredToken :: Text
  , deployCredScopes :: Text
  } deriving (Eq, Show)

data StoredTokenRow = StoredTokenRow
  { storedTokenId :: Int
  , storedTokenHost :: Text
  , storedTokenPlain :: Maybe Text
  , storedTokenEncrypted :: Maybe BS.ByteString
  , storedTokenNonce :: Maybe BS.ByteString
  , storedTokenKeyId :: Maybe Text
  , storedTokenScopes :: Text
  }

instance FromRow StoredTokenRow where
  fromRow =
    StoredTokenRow
      <$> field
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
  let migrations :: [Query]
      migrations =
        [ "DO $$ BEGIN CREATE TYPE hostenv_role AS ENUM ('admin','user'); EXCEPTION WHEN duplicate_object THEN null; END $$;"
        , "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, gitlab_user_id BIGINT UNIQUE NOT NULL, username TEXT NOT NULL, name TEXT NOT NULL, role hostenv_role NOT NULL DEFAULT 'user', created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now());"
        , "CREATE TABLE IF NOT EXISTS sessions (id TEXT PRIMARY KEY, user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE, csrf_token TEXT NOT NULL, expires_at TIMESTAMPTZ NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now());"
        , "CREATE TABLE IF NOT EXISTS oauth_states (state TEXT PRIMARY KEY, git_host TEXT NOT NULL, expires_at TIMESTAMPTZ NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now());"
        , "CREATE TABLE IF NOT EXISTS projects (id SERIAL PRIMARY KEY, org TEXT NOT NULL, project TEXT NOT NULL, git_host TEXT NOT NULL, repo_id BIGINT NOT NULL, repo_url TEXT NOT NULL, repo_path TEXT NOT NULL, flake_input TEXT NOT NULL, default_env_hash TEXT, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE (git_host, repo_id), UNIQUE (org, project));"
        , "CREATE TABLE IF NOT EXISTS gitlab_tokens (id SERIAL PRIMARY KEY, user_id INTEGER REFERENCES users(id) ON DELETE SET NULL, project_id INTEGER REFERENCES projects(id) ON DELETE CASCADE, git_host TEXT NOT NULL, token TEXT NOT NULL, scopes TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now());"
        , "ALTER TABLE gitlab_tokens ALTER COLUMN token DROP NOT NULL;"
        , "ALTER TABLE gitlab_tokens ADD COLUMN IF NOT EXISTS token_encrypted BYTEA;"
        , "ALTER TABLE gitlab_tokens ADD COLUMN IF NOT EXISTS token_nonce BYTEA;"
        , "ALTER TABLE gitlab_tokens ADD COLUMN IF NOT EXISTS token_key_id TEXT;"
        , "CREATE TABLE IF NOT EXISTS webhooks (id SERIAL PRIMARY KEY, project_id INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE, webhook_id BIGINT, webhook_url TEXT, secret TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE (project_id));"
        ]
  forM_ migrations (execute_ conn)
  migrateLegacyTokens cfg conn

loadProjects :: Connection -> IO [ProjectRow]
loadProjects conn =
  query_ conn "SELECT id, org, project, git_host, repo_id, repo_url, repo_path, flake_input, default_env_hash FROM projects ORDER BY org, project"

saveGitlabToken :: AppConfig -> Connection -> Maybe Int -> Maybe Int -> Text -> Text -> Text -> IO (Either Text ())
saveGitlabToken cfg conn mUserId mProjectId host token scopes =
  case cfg.appGitlabTokenCipher of
    Nothing -> pure (Left "GitLab token encryption key not configured")
    Just cipher -> do
      encrypted <- encryptToken cipher token
      case encrypted of
        Left err -> pure (Left err)
        Right enc -> do
          _ <- execute conn
            "INSERT INTO gitlab_tokens (user_id, project_id, git_host, token, token_encrypted, token_nonce, token_key_id, scopes) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
            ( mUserId
            , mProjectId
            , host
            , (Nothing :: Maybe Text)
            , Binary (encryptedTokenCiphertext enc)
            , Binary (encryptedTokenNonce enc)
            , encryptedTokenKeyId enc
            , scopes
            )
          pure (Right ())

loadLatestUserGitlabToken :: AppConfig -> Connection -> Int -> IO (Either Text (Maybe GitlabAccessToken))
loadLatestUserGitlabToken cfg conn userIdVal = do
  rows <- query conn
    "SELECT id, git_host, token, token_encrypted, token_nonce, token_key_id, scopes FROM gitlab_tokens WHERE user_id = ? AND project_id IS NULL ORDER BY created_at DESC LIMIT 1"
    (Only userIdVal)
  decodeTokenRows cfg conn rows

loadLatestProjectGitlabToken :: AppConfig -> Connection -> Int -> IO (Either Text (Maybe GitlabAccessToken))
loadLatestProjectGitlabToken cfg conn projectIdVal = do
  rows <- query conn
    "SELECT id, git_host, token, token_encrypted, token_nonce, token_key_id, scopes FROM gitlab_tokens WHERE project_id = ? ORDER BY created_at DESC LIMIT 1"
    (Only projectIdVal)
  decodeTokenRows cfg conn rows

loadProjectDeployCredentialByHash :: AppConfig -> Connection -> Text -> IO (Either Text (Maybe ProjectDeployCredential))
loadProjectDeployCredentialByHash cfg conn hash = do
  rows <- query conn
    "SELECT projects.id, projects.repo_id, gitlab_tokens.id, gitlab_tokens.git_host, gitlab_tokens.token, gitlab_tokens.token_encrypted, gitlab_tokens.token_nonce, gitlab_tokens.token_key_id, gitlab_tokens.scopes FROM projects JOIN LATERAL (SELECT id, git_host, token, token_encrypted, token_nonce, token_key_id, scopes FROM gitlab_tokens WHERE project_id = projects.id ORDER BY created_at DESC LIMIT 1) AS gitlab_tokens ON TRUE WHERE projects.default_env_hash = ? LIMIT 1"
    (Only hash)
  case rows of
    [] -> pure (Right Nothing)
    ((projectIdVal, repoIdVal, tokenRowId, host, plain, encrypted, nonce, keyId, scopes):_) -> do
      tokenResult <- resolveTokenValue cfg conn tokenRowId plain encrypted nonce keyId
      case tokenResult of
        Left err -> pure (Left err)
        Right token ->
          pure
            ( Right
                ( Just
                    ProjectDeployCredential
                      { deployCredProjectId = projectIdVal
                      , deployCredRepoId = repoIdVal
                      , deployCredHost = host
                      , deployCredToken = token
                      , deployCredScopes = scopes
                      }
                )
            )

decodeTokenRows :: AppConfig -> Connection -> [StoredTokenRow] -> IO (Either Text (Maybe GitlabAccessToken))
decodeTokenRows _cfg _conn [] = pure (Right Nothing)
decodeTokenRows cfg conn (row:_) = do
  tokenResult <- resolveTokenValue cfg conn row.storedTokenId row.storedTokenPlain row.storedTokenEncrypted row.storedTokenNonce row.storedTokenKeyId
  case tokenResult of
    Left err -> pure (Left err)
    Right token ->
      pure
        ( Right
            ( Just
                GitlabAccessToken
                  { gitlabTokenHost = row.storedTokenHost
                  , gitlabTokenValue = token
                  , gitlabTokenScopes = row.storedTokenScopes
                  }
            )
        )

resolveTokenValue
  :: AppConfig
  -> Connection
  -> Int
  -> Maybe Text
  -> Maybe BS.ByteString
  -> Maybe BS.ByteString
  -> Maybe Text
  -> IO (Either Text Text)
resolveTokenValue cfg conn tokenRowId mPlain mEncrypted mNonce mKeyId =
  case (mEncrypted, mNonce, mKeyId, mPlain) of
    (Just encrypted, Just nonce, Just keyId, _) ->
      case cfg.appGitlabTokenCipher of
        Nothing -> pure (Left "GitLab token encryption key not configured")
        Just cipher -> pure (decryptToken cipher nonce encrypted keyId)
    (Nothing, Nothing, Nothing, Just plain) -> do
      migrateResult <- migratePlaintextRow cfg conn tokenRowId plain
      case migrateResult of
        Left _ -> pure ()
        Right _ -> pure ()
      pure (Right plain)
    (Nothing, Nothing, Nothing, Nothing) -> pure (Left "GitLab token is missing")
    _ -> pure (Left "GitLab token row is malformed")

migratePlaintextRow :: AppConfig -> Connection -> Int -> Text -> IO (Either Text ())
migratePlaintextRow cfg conn tokenRowId plain =
  case cfg.appGitlabTokenCipher of
    Nothing -> pure (Left "GitLab token encryption key not configured")
    Just cipher -> do
      encrypted <- encryptToken cipher plain
      case encrypted of
        Left err -> pure (Left err)
        Right enc -> do
          _ <- execute conn
            "UPDATE gitlab_tokens SET token = NULL, token_encrypted = ?, token_nonce = ?, token_key_id = ?, updated_at = now() WHERE id = ?"
            (Binary (encryptedTokenCiphertext enc), Binary (encryptedTokenNonce enc), encryptedTokenKeyId enc, tokenRowId)
          pure (Right ())

migrateLegacyTokens :: AppConfig -> Connection -> IO ()
migrateLegacyTokens cfg conn =
  case cfg.appGitlabTokenCipher of
    Nothing -> pure ()
    Just cipher -> do
      rows <- query_ conn "SELECT id, token FROM gitlab_tokens WHERE token IS NOT NULL AND token_encrypted IS NULL"
      forM_ rows $ \(tokenId, tokenValue) -> do
        encrypted <- encryptToken cipher tokenValue
        case encrypted of
          Left _ -> pure ()
          Right enc -> do
            _ <- execute conn
              "UPDATE gitlab_tokens SET token = NULL, token_encrypted = ?, token_nonce = ?, token_key_id = ?, updated_at = now() WHERE id = ?"
              (Binary (encryptedTokenCiphertext enc), Binary (encryptedTokenNonce enc), encryptedTokenKeyId enc, tokenId :: Int)
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
          "SELECT sessions.id, sessions.csrf_token, sessions.expires_at, users.id, users.gitlab_user_id, users.username, users.name, users.role FROM sessions JOIN users ON users.id = sessions.user_id WHERE sessions.id = ?"
          (Only sid)
        case rows of
          [] -> pure Nothing
          ((sessId, csrfToken, expiresAt, uId, uGitlab, uUsername, uName, uRole):_) ->
            if expiresAt <= now
              then do
                _ <- execute conn "DELETE FROM sessions WHERE id = ?" (Only sessId)
                pure Nothing
              else do
                let user = User uId uGitlab uUsername uName uRole
                pure (Just (SessionInfo sessId csrfToken user))

renderSessionCookie :: SessionInfo -> BS.ByteString
renderSessionCookie sess =
  let cookie =
        defaultSetCookie
          { setCookieName = sessionCookieName
          , setCookieValue = TE.encodeUtf8 sess.sessionId
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
  rows <- query conn "SELECT id, gitlab_user_id, username, name, role FROM users WHERE id = ?" (Only userIdVal)
  case rows of
    (user:_) -> pure (SessionInfo sid csrf user)
    _ -> error "session user not found"
