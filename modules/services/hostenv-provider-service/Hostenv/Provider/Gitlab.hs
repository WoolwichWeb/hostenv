{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Gitlab
  ( GitlabTokenResponse(..)
  , GitlabUser(..)
  , GitlabProject(..)
  , GitlabHook(..)
  , GitlabDeployToken(..)
  , requireSecrets
  , selectGitlabHost
  , createOauthState
  , consumeOauthState
  , oauthRedirectUri
  , exchangeOAuthCode
  , fetchGitlabUser
  , loadUserProjects
  , fetchGitlabProjects
  , fetchGitlabProject
  , createGitlabWebhook
  , updateGitlabWebhook
  , createProjectDeployToken
  , revokeProjectDeployToken
  , appendNixAccessTokenConfig
  , UpsertUserSessionError(..)
  , upsertUserSession
  ) where

import Data.Aeson (FromJSON (..), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (addUTCTime, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple (Only (..), execute, query)
import Network.HTTP.Client (Manager, Request (..), RequestBody (..), httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types (Status, methodPost, statusCode)
import Network.HTTP.Types.URI (renderSimpleQuery)
import qualified Network.Wai as Wai

import Hostenv.Provider.Config (AppConfig(..), uiPath)
import Hostenv.Provider.DB
  ( GitlabAccessToken(..)
  , ProviderAccountMatch(..)
  , SessionInfo(..)
  , User(..)
  , createSession
  , loadLatestUserGitlabToken
  , lookupProviderAccount
  , saveGitlabToken
  , setProviderUserId
  , syncUsersConn
  , withDb
  )
import Hostenv.Provider.Service (GitlabSecrets(..))
import Hostenv.Provider.Util (randomToken)


requireSecrets :: AppConfig -> Either Text GitlabSecrets
requireSecrets cfg =
  let AppConfig { appGitlabSecrets = secrets } = cfg
   in case secrets of
    Just s -> Right s
    Nothing -> Left "GitLab OAuth is not configured"

requireManager :: AppConfig -> IO Manager
requireManager cfg =
  let AppConfig { appHttpManager = manager } = cfg
   in case manager of
    Just mgr -> pure mgr
    Nothing -> error "HTTP manager not configured"

selectGitlabHost :: AppConfig -> Wai.Request -> Maybe Text
selectGitlabHost cfg req =
  case lookup "host" (Wai.queryString req) >>= id of
    Nothing ->
      let AppConfig { appGitlabHosts = hosts } = cfg
       in listToMaybe hosts
    Just hostRaw ->
      let host = TE.decodeUtf8 hostRaw
          AppConfig { appGitlabHosts = hosts } = cfg
       in if host `elem` hosts then Just host else Nothing

createOauthState :: AppConfig -> Text -> IO Text
createOauthState cfg host = do
  state <- randomToken 18
  now <- getCurrentTime
  let expires = addUTCTime (60 * 10) now
  withDb cfg $ \conn -> do
    _ <- execute conn "INSERT INTO oauth_states (state, git_host, expires_at) VALUES (?, ?, ?)" (state, host, expires)
    pure state

consumeOauthState :: AppConfig -> Text -> IO (Maybe Text)
consumeOauthState cfg state = withDb cfg $ \conn -> do
  now <- getCurrentTime
  rows <- query conn "SELECT git_host, expires_at FROM oauth_states WHERE state = ?" (Only state)
  case rows of
    [] -> pure Nothing
    ((host, expiresAt):_) ->
      if expiresAt <= now
        then do
          _ <- execute conn "DELETE FROM oauth_states WHERE state = ?" (Only state)
          pure Nothing
        else do
          _ <- execute conn "DELETE FROM oauth_states WHERE state = ?" (Only state)
          pure (Just host)

oauthRedirectUri :: AppConfig -> Text
oauthRedirectUri cfg =
  let AppConfig { appUiBaseUrl = baseUrl } = cfg
   in baseUrl <> uiPath cfg "/oauth/gitlab/callback"

exchangeOAuthCode :: AppConfig -> Text -> Text -> IO (Either Text GitlabTokenResponse)
exchangeOAuthCode cfg host code = do
  case requireSecrets cfg of
    Left err -> pure (Left err)
    Right secrets -> do
      let GitlabSecrets { gitlabClientId = clientId, gitlabClientSecret = clientSecret } = secrets
      manager <- requireManager cfg
      initialReq <- parseRequest (T.unpack ("https://" <> host <> "/oauth/token"))
      let params =
            [ ("client_id", TE.encodeUtf8 clientId)
            , ("client_secret", TE.encodeUtf8 clientSecret)
            , ("code", TE.encodeUtf8 code)
            , ("grant_type", "authorization_code")
            , ("redirect_uri", TE.encodeUtf8 (oauthRedirectUri cfg))
            ]
      let req = initialReq
            { method = methodPost
            , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
            , requestBody = RequestBodyLBS (BL.fromStrict (renderSimpleQuery False params))
            }
      resp <- httpLbs req manager
      if isSuccessStatus (responseStatus resp)
        then case A.eitherDecode' (responseBody resp) of
          Left err -> pure (Left (T.pack err))
          Right token -> pure (Right token)
        else pure (Left "GitLab OAuth token exchange failed")

fetchGitlabUser :: AppConfig -> Text -> Text -> IO (Either Text GitlabUser)
fetchGitlabUser cfg host token = do
  manager <- requireManager cfg
  req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/user"))
  let req' = req { requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> token))] }
  resp <- httpLbs req' manager
  if isSuccessStatus (responseStatus resp)
    then case A.eitherDecode' (responseBody resp) of
      Left err -> pure (Left (T.pack err))
      Right user -> pure (Right user)
    else pure (Left "GitLab user lookup failed")

loadUserProjects :: AppConfig -> SessionInfo -> IO (Either Text [GitlabProject])
loadUserProjects cfg sess = withDb cfg $ \conn -> do
  let userIdVal = sess.user.id
  tokenResult <- loadLatestUserGitlabToken cfg conn userIdVal
  case tokenResult of
    Left err -> pure (Left err)
    Right Nothing -> pure (Left "No GitLab token available for this user")
    Right (Just tokenInfo) -> fetchGitlabProjects cfg tokenInfo.host tokenInfo.value

fetchGitlabProjects :: AppConfig -> Text -> Text -> IO (Either Text [GitlabProject])
fetchGitlabProjects cfg host token = do
  manager <- requireManager cfg
  req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/projects?membership=true&per_page=100&simple=true"))
  let req' = req { requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> token))] }
  resp <- httpLbs req' manager
  if isSuccessStatus (responseStatus resp)
    then case A.eitherDecode' (responseBody resp) of
      Left err -> pure (Left (T.pack err))
      Right projects -> pure (Right projects)
    else pure (Left "GitLab project listing failed")

fetchGitlabProject :: AppConfig -> Text -> Text -> Int64 -> IO (Either Text GitlabProject)
fetchGitlabProject cfg host token repoId = do
  manager <- requireManager cfg
  req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/projects/" <> T.pack (show repoId)))
  let req' = req { requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> token))] }
  resp <- httpLbs req' manager
  if isSuccessStatus (responseStatus resp)
    then case A.eitherDecode' (responseBody resp) of
      Left err -> pure (Left (T.pack err))
      Right project -> pure (Right project)
    else pure (Left "GitLab project lookup failed")

createGitlabWebhook :: AppConfig -> Text -> Text -> Int64 -> Text -> Text -> IO (Either Text GitlabHook)
createGitlabWebhook cfg host token repoId url secret = do
  manager <- requireManager cfg
  req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/projects/" <> T.pack (show repoId) <> "/hooks"))
  let params =
        [ ("url", TE.encodeUtf8 url)
        , ("token", TE.encodeUtf8 secret)
        , ("push_events", "true")
        , ("tag_push_events", "true")
        , ("enable_ssl_verification", "true")
        ]
  let req' = req
        { method = methodPost
        , requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> token)), ("Content-Type", "application/x-www-form-urlencoded")]
        , requestBody = RequestBodyLBS (BL.fromStrict (renderSimpleQuery False params))
        }
  resp <- httpLbs req' manager
  if isSuccessStatus (responseStatus resp)
    then case A.eitherDecode' (responseBody resp) of
      Left err -> pure (Left (T.pack err))
      Right hook -> pure (Right hook)
    else pure (Left "GitLab webhook creation failed")

updateGitlabWebhook :: AppConfig -> Text -> Text -> Int64 -> Int64 -> Text -> Text -> IO (Either Text GitlabHook)
updateGitlabWebhook cfg host token repoId hookId url secret = do
  manager <- requireManager cfg
  req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/projects/" <> T.pack (show repoId) <> "/hooks/" <> T.pack (show hookId)))
  let params =
        [ ("url", TE.encodeUtf8 url)
        , ("token", TE.encodeUtf8 secret)
        , ("push_events", "true")
        , ("tag_push_events", "true")
        , ("enable_ssl_verification", "true")
        ]
  let req' = req
        { method = "PUT"
        , requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> token)), ("Content-Type", "application/x-www-form-urlencoded")]
        , requestBody = RequestBodyLBS (BL.fromStrict (renderSimpleQuery False params))
        }
  resp <- httpLbs req' manager
  if isSuccessStatus (responseStatus resp)
    then case A.eitherDecode' (responseBody resp) of
      Left err -> pure (Left (T.pack err))
      Right hook -> pure (Right hook)
    else pure (Left "GitLab webhook update failed")

createProjectDeployToken :: AppConfig -> Text -> Text -> Int64 -> IO (Either Text GitlabDeployToken)
createProjectDeployToken cfg host oauthToken repoId = do
  manager <- requireManager cfg
  now <- getCurrentTime
  suffix <- randomToken 10
  let expiryDay = utctDay (addUTCTime (fromIntegral (cfg.appGitlabDeployTokenTtlMinutes * 60)) now)
      expiryDate = T.pack (formatTime defaultTimeLocale "%Y-%m-%d" expiryDay)
      tokenName = "hostenv-deploy-" <> suffix
  req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/projects/" <> T.pack (show repoId) <> "/access_tokens"))
  let params =
        [ ("name", TE.encodeUtf8 tokenName)
        , ("scopes[]", "read_repository")
        , ("access_level", "30")
        , ("expires_at", TE.encodeUtf8 expiryDate)
        ]
      req' =
        req
          { method = methodPost
          , requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> oauthToken)), ("Content-Type", "application/x-www-form-urlencoded")]
          , requestBody = RequestBodyLBS (BL.fromStrict (renderSimpleQuery False params))
          }
  resp <- httpLbs req' manager
  if isSuccessStatus (responseStatus resp)
    then case A.eitherDecode' (responseBody resp) of
      Left err -> pure (Left (T.pack err))
      Right deployToken -> pure (Right deployToken)
    else pure (Left "GitLab project deploy token creation failed")

revokeProjectDeployToken :: AppConfig -> Text -> Text -> Int64 -> Int64 -> IO (Either Text ())
revokeProjectDeployToken cfg host oauthToken repoId deployTokenId = do
  manager <- requireManager cfg
  req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/projects/" <> T.pack (show repoId) <> "/access_tokens/" <> T.pack (show deployTokenId)))
  let req' = req { method = "DELETE", requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> oauthToken))] }
  resp <- httpLbs req' manager
  let code = statusCode (responseStatus resp)
  if isSuccessStatus (responseStatus resp) || code == 404
    then pure (Right ())
    else pure (Left "GitLab project deploy token revocation failed")

appendNixAccessTokenConfig :: Maybe Text -> Text -> Text -> Text
appendNixAccessTokenConfig mExisting host token =
  let line = "access-tokens = " <> host <> "=" <> token
   in case mExisting of
    Nothing -> line
    Just existing ->
      if T.strip existing == ""
        then line
        else existing <> "\n" <> line


-- GitLab JSON types

data GitlabTokenResponse = GitlabTokenResponse
  { tokenAccessToken :: Text
  , tokenType :: Text
  , tokenScope :: Text
  } deriving (Eq, Show)

instance FromJSON GitlabTokenResponse where
  parseJSON = A.withObject "GitlabTokenResponse" $ \o ->
    GitlabTokenResponse
      <$> o .: "access_token"
      <*> o .: "token_type"
      <*> o .: "scope"


data GitlabUser = GitlabUser
  { glUserId :: Int64
  , glUserUsername :: Text
  , glUserName :: Text
  } deriving (Eq, Show)

instance FromJSON GitlabUser where
  parseJSON = A.withObject "GitlabUser" $ \o ->
    GitlabUser
      <$> o .: "id"
      <*> o .: "username"
      <*> o .: "name"


data GitlabProject = GitlabProject
  { glProjectId :: Int64
  , glProjectPath :: Text
  , glProjectHttpUrl :: Text
  , glProjectName :: Text
  } deriving (Eq, Show)

instance FromJSON GitlabProject where
  parseJSON = A.withObject "GitlabProject" $ \o ->
    GitlabProject
      <$> o .: "id"
      <*> o .: "path_with_namespace"
      <*> o .: "http_url_to_repo"
      <*> o .: "name"


data GitlabHook = GitlabHook
  { glHookId :: Int64
  , glHookUrl :: Text
  } deriving (Eq, Show)

instance FromJSON GitlabHook where
  parseJSON = A.withObject "GitlabHook" $ \o ->
    GitlabHook
      <$> o .: "id"
      <*> o .: "url"

data GitlabDeployToken = GitlabDeployToken
  { deployTokenId :: Int64
  , deployTokenValue :: Text
  } deriving (Eq, Show)

instance FromJSON GitlabDeployToken where
  parseJSON = A.withObject "GitlabDeployToken" $ \o ->
    GitlabDeployToken
      <$> o .: "id"
      <*> o .: "token"

data UpsertUserSessionError
  = AccessDenied Text
  | InternalError Text
  deriving (Eq, Show)

upsertUserSession :: AppConfig -> Text -> GitlabUser -> GitlabTokenResponse -> IO (Either UpsertUserSessionError SessionInfo)
upsertUserSession cfg host glUser token = withDb cfg $ \conn -> do
  syncUsersConn cfg conn
  let normalizedHost = T.toLower host
      normalizedUsername = T.toLower glUser.glUserUsername
  mapped <- lookupProviderAccount conn "gitlab" normalizedHost normalizedUsername
  case mapped of
    Nothing ->
      pure (Left (AccessDenied "GitLab account is not seeded in this hostenv project"))
    Just providerMatch ->
      case providerMatch.externalUserId of
        Just existingId
          | existingId /= glUser.glUserId ->
              pure (Left (AccessDenied "Seeded GitLab account mapping does not match provider user id"))
        _ -> do
          case providerMatch.externalUserId of
            Nothing ->
              setProviderUserId conn providerMatch.rowId glUser.glUserId
            _ ->
              pure ()
          let userIdVal = providerMatch.userId
          saveResult <- saveGitlabToken cfg conn (Just userIdVal) Nothing host token.tokenAccessToken token.tokenScope
          case saveResult of
            Left err -> pure (Left (InternalError err))
            Right _ -> Right <$> createSession conn userIdVal

isSuccessStatus :: Status -> Bool
isSuccessStatus st = statusCode st >= 200 && statusCode st < 300
