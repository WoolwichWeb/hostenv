{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Gitlab
  ( GitlabTokenResponse(..)
  , GitlabUser(..)
  , GitlabProject(..)
  , GitlabHook(..)
  , GitlabDeployToken(..)
  , NixGitlabTokenType(..)
  , GitlabCredentialContext(..)
  , GitlabError(..)
  , renderGitlabError
  , isReauthError
  , isAccessDeniedError
  , requireSecrets
  , selectGitlabHost
  , createOauthState
  , consumeOauthState
  , oauthRedirectUri
  , exchangeOAuthCode
  , fetchGitlabUser
  , loadUserOAuthCredential
  , loadUserProjects
  , fetchGitlabProjects
  , fetchGitlabProject
  , createGitlabWebhook
  , updateGitlabWebhook
  , createProjectDeployToken
  , revokeProjectToken
  , appendNixAccessTokenConfig
  , ensureProjectCredential
  , oauthCredentialFromTokenAt
  , gitlabApiError
  , renderAccessDeniedMessage
  , renderUserIdMismatchMessage
  , upsertUserSession
  ) where

import Data.Aeson (FromJSON (..), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime, addUTCTime, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, query)
import Network.HTTP.Client (Manager, Request (..), RequestBody (..), httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types (Status, methodPost, statusCode)
import Network.HTTP.Types.URI (renderSimpleQuery)
import qualified Network.Wai as Wai

import Hostenv.Provider.Config (AppConfig(..), uiPath)
import Hostenv.Provider.DB
  ( OAuthCredential(..)
  , DeployCredential(..)
  , ProviderAccountMatch(..)
  , SessionInfo(..)
  , User(..)
  , createSession
  , loadLatestOAuthCredential
  , lookupProviderAccount
  , setProviderUserId
  , syncUsersConn
  , upsertOAuthCredential
  , upsertProjectOAuthCredential
  , withDb
  )
import Hostenv.Provider.Service (GitlabSecrets(..))
import Hostenv.Provider.Util (randomToken)


data GitlabCredentialContext
  = UserCredentialContext
  | ProjectCredentialContext
  deriving (Eq, Show)

data NixGitlabTokenType
  = NixGitlabOAuth2
  | NixGitlabPAT
  deriving (Eq, Show)

data GitlabError
  = GitlabConfigError Text
  | GitlabHttpError
      { operation :: Text
      , status :: Int
      , responseBody :: Maybe Text
      }
  | GitlabDecodeError
      { operation :: Text
      , detail :: Text
      }
  | GitlabAuthMissingCredential
      { context :: GitlabCredentialContext
      }
  | GitlabAuthExpiredNoRefresh
      { context :: GitlabCredentialContext
      }
  | GitlabRefreshFailed
      { context :: GitlabCredentialContext
      , detail :: Text
      }
  | GitlabAccessDenied
      { detail :: Text
      }
  | GitlabInvariantError Text
  deriving (Eq, Show)

renderGitlabError :: GitlabError -> Text
renderGitlabError gitlabErr =
  case gitlabErr of
    GitlabConfigError msg -> msg
    GitlabHttpError { operation = op, status = code, responseBody = bodySnippet } ->
      let prefix = op <> " (HTTP " <> T.pack (show code) <> ")"
       in case bodySnippet of
            Nothing -> prefix
            Just snippet -> prefix <> "\nGitLab response: " <> snippet
    GitlabDecodeError { operation = op, detail = decodeDetail } ->
      op <> ": failed to decode GitLab response: " <> decodeDetail
    GitlabAuthMissingCredential { context = ctx } ->
      case ctx of
        UserCredentialContext ->
          "No GitLab token is available for this user. Please sign in again."
        ProjectCredentialContext ->
          "No project GitLab token is available. An admin must sign in again and re-add the project."
    GitlabAuthExpiredNoRefresh { context = ctx } ->
      case ctx of
        UserCredentialContext ->
          "GitLab OAuth session expired and has no refresh token. Please sign in again."
        ProjectCredentialContext ->
          "Project GitLab OAuth session expired and has no refresh token. An admin must sign in again and re-add the project."
    GitlabRefreshFailed { context = ctx, detail = refreshDetail } ->
      case ctx of
        UserCredentialContext ->
          appendDetail "Failed to refresh GitLab OAuth session. Please sign in again." refreshDetail
        ProjectCredentialContext ->
          appendDetail "Failed to refresh project GitLab OAuth session. An admin must sign in again and re-add the project." refreshDetail
    GitlabAccessDenied { detail = deniedDetail } -> deniedDetail
    GitlabInvariantError msg -> msg
  where
    appendDetail :: Text -> Text -> Text
    appendDetail prefix detailText =
      if T.strip detailText == ""
        then prefix
        else prefix <> "\n" <> detailText

isReauthError :: GitlabError -> Bool
isReauthError gitlabErr =
  case gitlabErr of
    GitlabAuthMissingCredential {} -> True
    GitlabAuthExpiredNoRefresh {} -> True
    GitlabRefreshFailed {} -> True
    GitlabHttpError { status = code } -> code == 401 || code == 403
    _ -> False

isAccessDeniedError :: GitlabError -> Bool
isAccessDeniedError gitlabErr =
  case gitlabErr of
    GitlabAccessDenied {} -> True
    _ -> False

requireSecrets :: AppConfig -> Either GitlabError GitlabSecrets
requireSecrets cfg =
  let AppConfig { appGitlabSecrets = secrets } = cfg
   in case secrets of
    Just s -> Right s
    Nothing -> Left (GitlabConfigError "GitLab OAuth is not configured")

requireManager :: AppConfig -> Either GitlabError Manager
requireManager cfg =
  let AppConfig { appHttpManager = manager } = cfg
   in case manager of
    Just mgr -> pure mgr
    Nothing -> Left (GitlabConfigError "HTTP manager is not configured")

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

exchangeOAuthCode :: AppConfig -> Text -> Text -> IO (Either GitlabError GitlabTokenResponse)
exchangeOAuthCode cfg host code =
  requestOAuthToken
    cfg
    host
    [ ("code", TE.encodeUtf8 code)
    , ("grant_type", "authorization_code")
    , ("redirect_uri", TE.encodeUtf8 (oauthRedirectUri cfg))
    ]

refreshOAuthToken :: AppConfig -> Text -> Text -> IO (Either GitlabError GitlabTokenResponse)
refreshOAuthToken cfg host refreshToken =
  requestOAuthToken
    cfg
    host
    [ ("refresh_token", TE.encodeUtf8 refreshToken)
    , ("grant_type", "refresh_token")
    ]

requestOAuthToken :: AppConfig -> Text -> [(BS.ByteString, BS.ByteString)] -> IO (Either GitlabError GitlabTokenResponse)
requestOAuthToken cfg host extraParams = do
  case requireSecrets cfg of
    Left err -> pure (Left err)
    Right secrets ->
      case requireManager cfg of
        Left err -> pure (Left err)
        Right manager -> do
          let GitlabSecrets { gitlabClientId = clientId, gitlabClientSecret = clientSecret } = secrets
          initialReq <- parseRequest (T.unpack ("https://" <> host <> "/oauth/token"))
          let params =
                [ ("client_id", TE.encodeUtf8 clientId)
                , ("client_secret", TE.encodeUtf8 clientSecret)
                ] <> extraParams
          let req = initialReq
                { method = methodPost
                , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                , requestBody = RequestBodyLBS (BL.fromStrict (renderSimpleQuery False params))
                }
          resp <- httpLbs req manager
          if isSuccessStatus (responseStatus resp)
            then case A.eitherDecode' (responseBody resp) of
              Left err ->
                pure (Left (GitlabDecodeError "GitLab OAuth token exchange failed" (T.pack err)))
              Right token -> pure (Right token)
            else pure (Left (gitlabApiError "GitLab OAuth token exchange failed" (responseStatus resp) (responseBody resp)))

fetchGitlabUser :: AppConfig -> Text -> Text -> IO (Either GitlabError GitlabUser)
fetchGitlabUser cfg host token = do
  case requireManager cfg of
    Left err -> pure (Left err)
    Right manager -> do
      req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/user"))
      let req' = req { requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> token))] }
      resp <- httpLbs req' manager
      if isSuccessStatus (responseStatus resp)
        then case A.eitherDecode' (responseBody resp) of
          Left err ->
            pure (Left (GitlabDecodeError "GitLab user lookup failed" (T.pack err)))
          Right user -> pure (Right user)
        else pure (Left (gitlabApiError "GitLab user lookup failed" (responseStatus resp) (responseBody resp)))

loadUserOAuthCredential :: AppConfig -> SessionInfo -> IO (Either GitlabError OAuthCredential)
loadUserOAuthCredential cfg sess = withDb cfg $ \conn -> do
  let userIdVal = sess.user.id
  credentialResult <- loadLatestOAuthCredential cfg conn userIdVal
  case credentialResult of
    Left err -> pure (Left (GitlabInvariantError err))
    Right Nothing -> pure (Left (GitlabAuthMissingCredential UserCredentialContext))
    Right (Just cred) -> ensureUserCredential cfg conn userIdVal cred

loadUserProjects :: AppConfig -> SessionInfo -> IO (Either GitlabError [GitlabProject])
loadUserProjects cfg sess = do
  credentialResult <- loadUserOAuthCredential cfg sess
  case credentialResult of
    Left err -> pure (Left err)
    Right credential -> fetchGitlabProjects cfg credential.host credential.accessToken

fetchGitlabProjects :: AppConfig -> Text -> Text -> IO (Either GitlabError [GitlabProject])
fetchGitlabProjects cfg host token = do
  case requireManager cfg of
    Left err -> pure (Left err)
    Right manager -> do
      req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/projects?membership=true&per_page=100&simple=true"))
      let req' = req { requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> token))] }
      resp <- httpLbs req' manager
      if isSuccessStatus (responseStatus resp)
        then case A.eitherDecode' (responseBody resp) of
          Left err ->
            pure (Left (GitlabDecodeError "GitLab project listing failed" (T.pack err)))
          Right projects -> pure (Right projects)
        else pure (Left (gitlabApiError "GitLab project listing failed" (responseStatus resp) (responseBody resp)))

fetchGitlabProject :: AppConfig -> Text -> Text -> Int64 -> IO (Either GitlabError GitlabProject)
fetchGitlabProject cfg host token repoId = do
  case requireManager cfg of
    Left err -> pure (Left err)
    Right manager -> do
      req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/projects/" <> T.pack (show repoId)))
      let req' = req { requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> token))] }
      resp <- httpLbs req' manager
      if isSuccessStatus (responseStatus resp)
        then case A.eitherDecode' (responseBody resp) of
          Left err ->
            pure (Left (GitlabDecodeError "GitLab project lookup failed" (T.pack err)))
          Right project -> pure (Right project)
        else pure (Left (gitlabApiError "GitLab project lookup failed" (responseStatus resp) (responseBody resp)))

createGitlabWebhook :: AppConfig -> Text -> Text -> Int64 -> Text -> Text -> IO (Either GitlabError GitlabHook)
createGitlabWebhook cfg host token repoId url secret = do
  case requireManager cfg of
    Left err -> pure (Left err)
    Right manager -> do
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
          Left err ->
            pure (Left (GitlabDecodeError "GitLab webhook creation failed" (T.pack err)))
          Right hook -> pure (Right hook)
        else pure (Left (gitlabApiError "GitLab webhook creation failed" (responseStatus resp) (responseBody resp)))

updateGitlabWebhook :: AppConfig -> Text -> Text -> Int64 -> Int64 -> Text -> Text -> IO (Either GitlabError GitlabHook)
updateGitlabWebhook cfg host token repoId hookId url secret = do
  case requireManager cfg of
    Left err -> pure (Left err)
    Right manager -> do
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
          Left err ->
            pure (Left (GitlabDecodeError "GitLab webhook update failed" (T.pack err)))
          Right hook -> pure (Right hook)
        else pure (Left (gitlabApiError "GitLab webhook update failed" (responseStatus resp) (responseBody resp)))

createProjectDeployToken :: AppConfig -> Text -> Text -> Int64 -> IO (Either GitlabError GitlabDeployToken)
createProjectDeployToken cfg host oauthToken repoId = do
  case requireManager cfg of
    Left err -> pure (Left err)
    Right manager -> do
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
          Left err ->
            pure (Left (GitlabDecodeError "GitLab project deploy token creation failed" (T.pack err)))
          Right deployToken -> pure (Right deployToken)
        else pure (Left (gitlabApiError "GitLab project deploy token creation failed" (responseStatus resp) (responseBody resp)))

revokeProjectToken :: AppConfig -> Text -> Text -> Int64 -> Int64 -> IO (Either GitlabError ())
revokeProjectToken cfg host oauthToken repoId deployTokenId = do
  case requireManager cfg of
    Left err -> pure (Left err)
    Right manager -> do
      req <- parseRequest (T.unpack ("https://" <> host <> "/api/v4/projects/" <> T.pack (show repoId) <> "/access_tokens/" <> T.pack (show deployTokenId)))
      let req' = req { method = "DELETE", requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> oauthToken))] }
      resp <- httpLbs req' manager
      let code = statusCode (responseStatus resp)
      if isSuccessStatus (responseStatus resp) || code == 404
        then pure (Right ())
        else pure (Left (gitlabApiError "GitLab project deploy token revocation failed" (responseStatus resp) (responseBody resp)))

appendNixAccessTokenConfig :: Maybe Text -> Text -> NixGitlabTokenType -> Text -> Text
appendNixAccessTokenConfig mExisting host tokenType token =
  let line = "access-tokens = " <> host <> "=" <> renderTokenType tokenType <> ":" <> token
   in case mExisting of
    Nothing -> line
    Just existing ->
      if T.strip existing == ""
        then line
        else existing <> "\n" <> line
  where
    renderTokenType kind =
      case kind of
        NixGitlabOAuth2 -> "OAuth2"
        NixGitlabPAT -> "PAT"

ensureProjectCredential :: AppConfig -> DeployCredential -> IO (Either GitlabError DeployCredential)
ensureProjectCredential cfg credential = do
  now <- getCurrentTime
  if not (tokenNeedsRefresh now credential.expiresAt)
    then pure (Right credential)
    else case credential.refreshToken of
      Nothing ->
        pure (Left (GitlabAuthExpiredNoRefresh ProjectCredentialContext))
      Just refreshToken -> do
        refreshResult <- refreshOAuthToken cfg credential.host refreshToken
        case refreshResult of
          Left err ->
            pure (Left (GitlabRefreshFailed ProjectCredentialContext (renderGitlabError err)))
          Right refreshedToken -> do
            refreshedCredential <- oauthCredentialFromToken credential.host (Just (projectCredentialAsOAuth credential)) refreshedToken
            saveResult <- withDb cfg $ \conn -> upsertProjectOAuthCredential cfg conn credential.projectId refreshedCredential
            case saveResult of
              Left err -> pure (Left (GitlabInvariantError err))
              Right _ -> pure (Right (projectCredentialFromOAuth credential refreshedCredential))

projectCredentialAsOAuth :: DeployCredential -> OAuthCredential
projectCredentialAsOAuth credential =
  OAuthCredential
    { host = credential.host
    , accessToken = credential.accessToken
    , refreshToken = credential.refreshToken
    , tokenType = credential.tokenType
    , scopes = credential.scopes
    , expiresAt = credential.expiresAt
    }

projectCredentialFromOAuth :: DeployCredential -> OAuthCredential -> DeployCredential
projectCredentialFromOAuth projectCredential oauthCredential =
  DeployCredential
    { projectId = projectCredential.projectId
    , repoId = projectCredential.repoId
    , host = oauthCredential.host
    , accessToken = oauthCredential.accessToken
    , refreshToken = oauthCredential.refreshToken
    , tokenType = oauthCredential.tokenType
    , scopes = oauthCredential.scopes
    , expiresAt = oauthCredential.expiresAt
    }

ensureUserCredential :: AppConfig -> Connection -> Int -> OAuthCredential -> IO (Either GitlabError OAuthCredential)
ensureUserCredential cfg conn userIdVal credential = do
  now <- getCurrentTime
  if not (tokenNeedsRefresh now credential.expiresAt)
    then pure (Right credential)
    else case credential.refreshToken of
      Nothing ->
        pure (Left (GitlabAuthExpiredNoRefresh UserCredentialContext))
      Just refreshToken -> do
        refreshResult <- refreshOAuthToken cfg credential.host refreshToken
        case refreshResult of
          Left err -> pure (Left (GitlabRefreshFailed UserCredentialContext (renderGitlabError err)))
          Right refreshedToken -> do
            refreshedCredential <- oauthCredentialFromToken credential.host (Just credential) refreshedToken
            saveResult <- upsertOAuthCredential cfg conn userIdVal refreshedCredential
            case saveResult of
              Left err -> pure (Left (GitlabInvariantError err))
              Right _ -> pure (Right refreshedCredential)

tokenNeedsRefresh :: UTCTime -> Maybe UTCTime -> Bool
tokenNeedsRefresh _ Nothing = False
tokenNeedsRefresh now (Just expiry) = expiry <= addUTCTime 60 now

oauthCredentialFromToken :: Text -> Maybe OAuthCredential -> GitlabTokenResponse -> IO OAuthCredential
oauthCredentialFromToken host mPrevious token = do
  now <- getCurrentTime
  pure (oauthCredentialFromTokenAt now host mPrevious token)

oauthCredentialFromTokenAt :: UTCTime -> Text -> Maybe OAuthCredential -> GitlabTokenResponse -> OAuthCredential
oauthCredentialFromTokenAt now host mPrevious token =
  let refreshToken = case token.refreshToken of
        Just value | T.strip value /= "" -> Just value
        _ -> mPrevious >>= \prev -> prev.refreshToken
      tokenType = case token.tokenType of
        Just value | T.strip value /= "" -> Just value
        _ -> mPrevious >>= \prev -> prev.tokenType
      scopes = case token.scope of
        Just value | T.strip value /= "" -> value
        _ -> maybe "api read_repository" (\prev -> prev.scopes) mPrevious
      expiresAt = fmap (\seconds -> addUTCTime (fromIntegral (max 0 (seconds - 60))) now) token.expiresIn
   in OAuthCredential
        { host = T.toLower host
        , accessToken = token.accessToken
        , refreshToken = refreshToken
        , tokenType = tokenType
        , scopes = scopes
        , expiresAt = expiresAt
        }


-- GitLab JSON types

data GitlabTokenResponse = GitlabTokenResponse
  { accessToken :: Text
  , refreshToken :: Maybe Text
  , tokenType :: Maybe Text
  , scope :: Maybe Text
  , expiresIn :: Maybe Int
  } deriving (Eq, Show)

instance FromJSON GitlabTokenResponse where
  parseJSON = A.withObject "GitlabTokenResponse" $ \o ->
    GitlabTokenResponse
      <$> o .: "access_token"
      <*> o .:? "refresh_token"
      <*> o .:? "token_type"
      <*> o .:? "scope"
      <*> o .:? "expires_in"


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
  { id :: Int64
  , path :: Text
  , httpUrl :: Text
  , name :: Text
  } deriving (Eq, Show)

instance FromJSON GitlabProject where
  parseJSON = A.withObject "GitlabProject" $ \o ->
    GitlabProject
      <$> o .: "id"
      <*> o .: "path_with_namespace"
      <*> o .: "http_url_to_repo"
      <*> o .: "name"


data GitlabHook = GitlabHook
  { id :: Int64
  , url :: Text
  } deriving (Eq, Show)

instance FromJSON GitlabHook where
  parseJSON = A.withObject "GitlabHook" $ \o ->
    GitlabHook
      <$> o .: "id"
      <*> o .: "url"

data GitlabDeployToken = GitlabDeployToken
  { id :: Int64
  , value :: Text
  } deriving (Eq, Show)

instance FromJSON GitlabDeployToken where
  parseJSON = A.withObject "GitlabDeployToken" $ \o ->
    GitlabDeployToken
      <$> o .: "id"
      <*> o .: "token"

upsertUserSession :: AppConfig -> Text -> GitlabUser -> GitlabTokenResponse -> IO (Either GitlabError SessionInfo)
upsertUserSession cfg host glUser token = withDb cfg $ \conn -> do
  syncUsersConn cfg conn
  let normalizedHost = T.toLower host
      normalizedUsername = T.toLower glUser.glUserUsername
  mapped <- lookupProviderAccount conn "gitlab" normalizedHost normalizedUsername
  case mapped of
    Nothing ->
      pure (Left (GitlabAccessDenied (renderAccessDeniedMessage glUser.glUserUsername)))
    Just providerMatch ->
      case providerMatch.externalUserId of
        Just existingId
          | existingId /= glUser.glUserId ->
              pure (Left (GitlabAccessDenied (renderUserIdMismatchMessage glUser.glUserUsername glUser.glUserId existingId)))
        _ -> do
          case providerMatch.externalUserId of
            Nothing ->
              setProviderUserId conn providerMatch.rowId glUser.glUserId
            _ ->
              pure ()
          let userIdVal = providerMatch.userId
          credential <- oauthCredentialFromToken normalizedHost Nothing token
          saveResult <- upsertOAuthCredential cfg conn userIdVal credential
          case saveResult of
            Left err -> pure (Left (GitlabInvariantError err))
            Right _ -> Right <$> createSession conn userIdVal

isSuccessStatus :: Status -> Bool
isSuccessStatus st = statusCode st >= 200 && statusCode st < 300

gitlabApiError :: Text -> Status -> BL.ByteString -> GitlabError
gitlabApiError op status body =
  GitlabHttpError
    { operation = op
    , status = statusCode status
    , responseBody = sanitizeErrorBody body
    }
  where
    sanitizeErrorBody :: BL.ByteString -> Maybe Text
    sanitizeErrorBody raw =
      let decoded = TE.decodeUtf8With lenientDecode (BL.toStrict raw)
          safeChars = T.filter (\c -> c == '\n' || c == '\t' || c >= ' ') decoded
          flattened = T.unwords (T.words safeChars)
          snippet = T.take 500 (T.strip flattened)
       in if T.null snippet
            then Nothing
            else Just snippet

renderAccessDeniedMessage :: Text -> Text
renderAccessDeniedMessage username =
  T.unlines
    [ "The GitLab user account " <> username <> " has not been granted access to this Hostenv provider project."
    , ""
    , "Add your GitLab user account to:"
    , "- `allEnvironments.users.<name>.gitlabUsername`"
    , "- or `environments.<name>.users.<name>.gitlabUsername`"
    , ""
    , "Then re-deploy to rectify this."
    ]

renderUserIdMismatchMessage :: Text -> Int64 -> Int64 -> Text
renderUserIdMismatchMessage username actualId expectedId =
  "GitLab's user id for " <> username <> " (" <> T.pack (show actualId) <> ") does not match the user id in Hostenv's records (" <> T.pack (show expectedId) <> ")"
