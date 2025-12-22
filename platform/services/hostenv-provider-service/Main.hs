{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import "cryptonite" Crypto.Random (getRandomBytes)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (toLazyByteString)
import Data.Char (isAlphaNum, isSpace)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Tagged (Tagged (..))
import Database.PostgreSQL.Simple (Connection, Only (..), Query, close, connectPostgreSQL, execute, execute_, fromOnly, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, Request (..), RequestBody (..), httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status, hLocation, methodGet, methodPost, statusCode, status200, status302, status400, status401, status403, status404, status500)
import qualified Network.HTTP.Types.Header as HTTPHeader
import Network.HTTP.Types.URI (parseQuery, renderSimpleQuery)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), SockAddr (SockAddrUnix), bind, defaultProtocol, listen, socket)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS, strictRequestBody)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setBeforeMainLoop)
import Servant
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getXdgDirectory, removeFile, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>), isAbsolute, takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (setFileMode)
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, renderSetCookie, sameSiteLax)
import qualified Turtle as Sh

import Hostenv.Provider.Service
  ( CommandError
  , CommandOutput(..)
  , CommandRunner
  , CommandSpec(..)
  , DeployResult(..)
  , GitlabSecrets
  , PlanLoader
  , ProjectRef(..)
  , WebhookConfig(..)
  , WebhookError(..)
  , WebhookResult(..)
  , nodesForProject
  , projectForHash
  , projectHashFor
  , readGitlabSecrets
  , renderFlakeTemplate
  , renderGitCredentials
  , renderProjectInputs
  , runWebhookWith
  , verifyGitHubSignature
  , verifyGitLabToken
  )
import qualified Hostenv.Provider.Service as Service


sessionCookieName :: BS.ByteString
sessionCookieName = "hostenv_session"

csrfFieldName :: BS.ByteString
csrfFieldName = "csrf"


-- Configuration

data AppConfig = AppConfig
  { appDataDir :: FilePath
  , appRepoSource :: FilePath
  , appFlakeRoot :: FilePath
  , appListenSocket :: FilePath
  , appWebhookSecretFile :: Maybe FilePath
  , appWebhookSecretsDir :: Maybe FilePath
  , appWebhookConfig :: WebhookConfig
  , appUiBasePath :: Text
  , appUiBaseUrl :: Text
  , appWebhookHost :: Text
  , appDbConnString :: Maybe BS.ByteString
  , appGitlabSecrets :: Maybe GitlabSecrets
  , appGitlabHosts :: [Text]
  , appGitConfigPath :: FilePath
  , appGitCredentialsPath :: FilePath
  , appFlakeTemplate :: FilePath
  , appHttpManager :: Maybe Manager
  }


-- JSON + API types

data ErrorResponse = ErrorResponse
  { errMessage :: Text
  , errCommand :: Maybe Text
  , errExitCode :: Maybe Int
  , errStdout :: Maybe Text
  , errStderr :: Maybe Text
  }

instance ToJSON ErrorResponse where
  toJSON e =
    A.object
      [ "error" .= e.errMessage
      , "command" .= e.errCommand
      , "exitCode" .= e.errExitCode
      , "stdout" .= e.errStdout
      , "stderr" .= e.errStderr
      ]


type API =
  "webhook"
    :> Capture "hash" Text
    :> Header "X-Hub-Signature-256" Text
    :> Header "X-Gitlab-Token" Text
    :> ReqBody '[OctetStream] BL.ByteString
    :> Post '[JSON] WebhookResult
    :<|> Raw

api :: Proxy API
api = Proxy


main :: IO ()
main = do
  cfg <- loadConfig
  ensureProviderRepo cfg
  ensureGitConfig cfg
  ensureSchema cfg
  syncFlakeFromDb cfg
  runCommandOrDie cfg (CommandSpec "nix" ["flake", "update"] (appWorkDir cfg))
  runServer cfg


runServer :: AppConfig -> IO ()
runServer cfg = do
  sock <- openUnixSocket cfg.appListenSocket
  let settings = setBeforeMainLoop (putStrLn "hostenv-provider-service: listening") defaultSettings
  runSettingsSocket settings sock (app cfg)

app :: AppConfig -> Application
app cfg = serve api (server cfg)

server :: AppConfig -> Server API
server cfg = webhookHandler cfg :<|> Tagged (uiApp cfg)


webhookHandler :: AppConfig -> Text -> Maybe Text -> Maybe Text -> BL.ByteString -> Handler WebhookResult
webhookHandler cfg hash mHubSig mGitlabToken rawBody = do
  planRaw <- liftIO (loadPlan cfg)
  projectRef <-
    case projectForHash hash planRaw of
      Left msg ->
        if msg == "webhook hash not found in plan.json"
          then throwError err404
          else throwError (errorWithBody err500 (ErrorResponse msg Nothing Nothing Nothing Nothing))
      Right ref -> pure ref
  secretInfo <- liftIO (resolveSecret cfg hash projectRef)
  verifyWebhook secretInfo mHubSig mGitlabToken rawBody
  result <- liftIO $ runWebhookWith (runCommand cfg) (loadPlan cfg) cfg.appWebhookConfig projectRef
  case result of
    Left err -> throwError (serverError err)
    Right okResult ->
      if okResult.webhookOk
        then pure okResult
        else throwError (errorWithBody err500 okResult)


-- UI app

uiApp :: AppConfig -> Application
uiApp cfg req respond = do
  case stripBasePath cfg.appUiBasePath (pathInfo req) of
    Nothing -> respond (responseLBS status404 [] "")
    Just rest -> routeUi cfg rest req respond

stripBasePath :: Text -> [Text] -> Maybe [Text]
stripBasePath base segments =
  let baseSegs = filter (/= "") (T.splitOn "/" (normalizeBasePath base))
   in if baseSegs == []
        then Just segments
        else case splitAt (length baseSegs) segments of
          (prefix, remainder) | prefix == baseSegs -> Just remainder
          _ -> Nothing

routeUi :: AppConfig -> [Text] -> Application
routeUi cfg segments req respond =
  case segments of
    [] ->
      case requestMethod req of
        m | m == methodGet -> handleIndex cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["login"] ->
      case requestMethod req of
        m | m == methodGet -> handleLogin cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["logout"] ->
      case requestMethod req of
        m | m == methodGet -> handleLogout cfg respond
        _ -> respond (responseLBS status404 [] "")
    ["oauth", "gitlab", "start"] ->
      case requestMethod req of
        m | m == methodGet -> handleOauthStart cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["oauth", "gitlab", "callback"] ->
      case requestMethod req of
        m | m == methodGet -> handleOauthCallback cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["add-project"] ->
      case requestMethod req of
        m | m == methodGet -> handleAddProjectGet cfg req respond
        m | m == methodPost -> handleAddProjectPost cfg req respond
        _ -> respond (responseLBS status404 [] "")
    _ -> respond (responseLBS status404 [] "")


-- UI handlers

handleLogin :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleLogin cfg _ respond =
  respondHtml respond status200 (loginPage cfg Nothing)

handleLogout :: AppConfig -> (Wai.Response -> IO a) -> IO a
handleLogout cfg respond =
  respondHtmlWithHeaders respond status302 [(hLocation, TE.encodeUtf8 (uiPath cfg "/login")), ("Set-Cookie", logoutCookie)] ""

handleIndex :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleIndex cfg req respond = do
  mSession <- getSessionInfo cfg req
  case mSession of
    Nothing -> respondRedirect respond cfg "/login"
    Just sess ->
      if sess.sessionUser.userRole /= "admin"
        then respondHtml respond status403 (accessDeniedPage cfg)
        else do
          projects <- withDb cfg (\conn -> loadProjects conn)
          respondHtml respond status200 (indexPage cfg sess projects)

handleAddProjectGet :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleAddProjectGet cfg req respond = do
  mSession <- getSessionInfo cfg req
  case mSession of
    Nothing -> respondRedirect respond cfg "/login"
    Just sess ->
      if sess.sessionUser.userRole /= "admin"
        then respondHtml respond status403 (accessDeniedPage cfg)
        else do
          result <- loadUserProjects cfg sess
          case result of
            Left msg -> respondHtml respond status500 (errorPage cfg msg)
            Right repos -> respondHtml respond status200 (addProjectPage cfg sess repos)

handleAddProjectPost :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleAddProjectPost cfg req respond = do
  mSession <- getSessionInfo cfg req
  case mSession of
    Nothing -> respondRedirect respond cfg "/login"
    Just sess ->
      if sess.sessionUser.userRole /= "admin"
        then respondHtml respond status403 (accessDeniedPage cfg)
        else do
          body <- strictRequestBody req
          let params = parseForm body
          case lookupParam csrfFieldName params of
            Nothing -> respondHtml respond status400 (errorPage cfg "Missing CSRF token")
            Just csrfToken ->
              if csrfToken /= sess.sessionCsrf
                then respondHtml respond status403 (errorPage cfg "Invalid CSRF token")
                else do
                  let mRepoId = lookupParam "repo_id" params >>= readInt64
                  let orgInput = lookupParam "org" params
                  let projectInput = lookupParam "project" params
                  case mRepoId of
                    Nothing -> respondHtml respond status400 (errorPage cfg "Missing repository selection")
                    Just repoId -> do
                      result <- addProjectFlow cfg sess repoId orgInput projectInput
                      case result of
                        Left msg -> respondHtml respond status500 (errorPage cfg msg)
                        Right successMsg -> respondHtml respond status200 (successPage cfg successMsg)

handleOauthStart :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleOauthStart cfg req respond = do
  let host = selectGitlabHost cfg req
  case host of
    Nothing -> respondHtml respond status400 (errorPage cfg "No GitLab host configured")
    Just glHost -> do
      state <- createOauthState cfg glHost
      let redirectUri = oauthRedirectUri cfg
      let params =
            [ ("client_id", TE.encodeUtf8 ((requireSecrets cfg).gitlabClientId))
            , ("redirect_uri", TE.encodeUtf8 redirectUri)
            , ("response_type", "code")
            , ("scope", "api read_repository")
            , ("state", TE.encodeUtf8 state)
            ]
      let url = T.concat
            [ "https://"
            , glHost
            , "/oauth/authorize?"
            , TE.decodeUtf8 (renderSimpleQuery False params)
            ]
      respondHtmlWithHeaders respond status302 [(hLocation, TE.encodeUtf8 url)] ""

handleOauthCallback :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleOauthCallback cfg req respond = do
  let params = Wai.queryString req
  let mCode = lookup "code" params >>= id
  let mState = lookup "state" params >>= id
  case (mCode, mState) of
    (Just codeRaw, Just stateRaw) -> do
      let code = TE.decodeUtf8 codeRaw
      let state = TE.decodeUtf8 stateRaw
      mHost <- consumeOauthState cfg state
      case mHost of
        Nothing -> respondHtml respond status400 (errorPage cfg "Invalid OAuth state")
        Just glHost -> do
          tokenResp <- exchangeOAuthCode cfg glHost code
          case tokenResp of
            Left msg -> respondHtml respond status500 (errorPage cfg msg)
            Right token -> do
              mUser <- fetchGitlabUser cfg glHost token.tokenAccessToken
              case mUser of
                Left msg -> respondHtml respond status500 (errorPage cfg msg)
                Right glUser -> do
                  session <- upsertUserSession cfg glHost glUser token
                  let cookieHeader = ("Set-Cookie", renderSessionCookie session)
                  respondHtmlWithHeaders respond status302 [(hLocation, TE.encodeUtf8 (uiPath cfg "/")), cookieHeader] ""
    _ -> respondHtml respond status400 (errorPage cfg "Missing OAuth callback parameters")


-- UI helpers

respondHtml :: (Wai.Response -> IO a) -> Status -> Text -> IO a
respondHtml respond status body =
  respond (responseLBS status [("Content-Type", "text/html; charset=utf-8")] (BL.fromStrict (TE.encodeUtf8 body)))

respondHtmlWithHeaders :: (Wai.Response -> IO a) -> Status -> [HTTPHeader.Header] -> Text -> IO a
respondHtmlWithHeaders respond status headers body =
  respond (responseLBS status (("Content-Type", "text/html; charset=utf-8") : headers) (BL.fromStrict (TE.encodeUtf8 body)))

respondRedirect :: (Wai.Response -> IO a) -> AppConfig -> Text -> IO a
respondRedirect respond cfg path =
  respondHtmlWithHeaders respond status302 [(hLocation, TE.encodeUtf8 (uiPath cfg path))] ""

isSuccessStatus :: Status -> Bool
isSuccessStatus st = statusCode st >= 200 && statusCode st < 300

uiPath :: AppConfig -> Text -> Text
uiPath cfg path =
  let base = normalizeBasePath cfg.appUiBasePath
   in case (base, path) of
        ("/", "/") -> "/"
        ("/", _) -> path
        (_, "/") -> base
        _ -> base <> path

normalizeBasePath :: Text -> Text
normalizeBasePath t =
  let withSlash = if T.isPrefixOf "/" t then t else "/" <> t
      trimmed = T.dropWhileEnd (== '/') withSlash
   in if trimmed == "" then "/" else trimmed

loginPage :: AppConfig -> Maybe Text -> Text
loginPage cfg mMsg =
  page cfg "Hostenv Provider" $
    T.concat
      [ maybe "" (\msg -> alertBox msg) mMsg
      , "<h1>Hostenv Provider</h1>"
      , "<p>Sign in with GitLab to manage projects.</p>"
      , loginButtons
      ]
  where
    hosts = cfg.appGitlabHosts
    loginButtons =
      if length hosts <= 1
        then T.concat
          [ "<a class=\"btn\" href=\""
          , uiPath cfg "/oauth/gitlab/start"
          , "\">Sign in with GitLab</a>"
          ]
        else
          let renderHost host =
                T.concat
                  [ "<a class=\"btn\" href=\""
                  , uiPath cfg ("/oauth/gitlab/start?host=" <> host)
                  , "\">Sign in with "
                  , escapeHtml host
                  , "</a>"
                  ]
           in T.concat (map renderHost hosts)

accessDeniedPage :: AppConfig -> Text
accessDeniedPage cfg =
  page cfg "Access denied" $
    T.concat
      [ "<h1>Access denied</h1>"
      , "<p>This account does not have the admin role.</p>"
      , "<a class=\"btn subtle\" href=\""
      , uiPath cfg "/logout"
      , "\">Sign out</a>"
      ]

indexPage :: AppConfig -> SessionInfo -> [ProjectRow] -> Text
indexPage cfg sess projects =
  page cfg "Projects" $
    T.concat
      [ "<div class=\"header\"><h1>Projects</h1><div class=\"actions\">"
      , "<span class=\"user\">"
      , escapeHtml (sess.sessionUser.userUsername)
      , "</span>"
      , "<a class=\"btn subtle\" href=\""
      , uiPath cfg "/logout"
      , "\">Sign out</a></div></div>"
      , projectListHtml projects
      , "<div class=\"footer\"><a class=\"btn\" href=\""
      , uiPath cfg "/add-project"
      , "\">Add project from GitLab</a></div>"
      ]

projectListHtml :: [ProjectRow] -> Text
projectListHtml projects =
  if null projects
    then "<p>No projects added yet.</p>"
    else
      let rows = T.concat (map renderProject (sortOn (\p -> p.projectFlakeInput) projects))
       in T.concat
            [ "<table><thead><tr><th>Input</th><th>Repo</th><th>Host</th><th>Webhook hash</th></tr></thead><tbody>"
            , rows
            , "</tbody></table>"
            ]
  where
    renderProject p =
      T.concat
        [ "<tr><td><code>"
        , escapeHtml p.projectFlakeInput
        , "</code></td><td>"
        , escapeHtml p.projectRepoPath
        , "</td><td>"
        , escapeHtml p.projectGitHost
        , "</td><td><code>"
        , escapeHtml (fromMaybe "" p.projectHash)
        , "</code></td></tr>"
        ]

addProjectPage :: AppConfig -> SessionInfo -> [GitlabProject] -> Text
addProjectPage cfg sess repos =
  page cfg "Add project" $
    T.concat
      [ "<div class=\"header\"><h1>Add project</h1><div class=\"actions\">"
      , "<a class=\"btn subtle\" href=\""
      , uiPath cfg "/"
      , "\">Back</a></div></div>"
      , "<form method=\"post\" class=\"card\">"
      , "<input type=\"hidden\" name=\"csrf\" value=\""
      , escapeHtml sess.sessionCsrf
      , "\"/>"
      , "<label>Repository</label>"
      , "<select name=\"repo_id\">"
      , T.concat (map renderRepoOption repos)
      , "</select>"
      , "<label>Organisation</label>"
      , "<input type=\"text\" name=\"org\" placeholder=\"org\"/>"
      , "<label>Project</label>"
      , "<input type=\"text\" name=\"project\" placeholder=\"project\"/>"
      , "<button class=\"btn\" type=\"submit\">Add project</button>"
      , "</form>"
      ]
  where
    renderRepoOption repo =
      T.concat
        [ "<option value=\""
        , T.pack (show repo.glProjectId)
        , "\">"
        , escapeHtml repo.glProjectPath
        , "</option>"
        ]

successPage :: AppConfig -> Text -> Text
successPage cfg msg =
  page cfg "Success" $
    T.concat
      [ "<h1>Project added</h1>"
      , "<p>"
      , escapeHtml msg
      , "</p>"
      , "<a class=\"btn\" href=\""
      , uiPath cfg "/"
      , "\">Back to projects</a>"
      ]

errorPage :: AppConfig -> Text -> Text
errorPage cfg msg =
  page cfg "Error" $
    T.concat
      [ alertBox msg
      , "<a class=\"btn subtle\" href=\""
      , uiPath cfg "/"
      , "\">Back</a>"
      ]

alertBox :: Text -> Text
alertBox msg =
  T.concat ["<div class=\"alert\">", escapeHtml msg, "</div>"]

page :: AppConfig -> Text -> Text -> Text
page cfg title body =
  T.concat
    [ "<!doctype html><html><head><meta charset=\"utf-8\"/><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/>"
    , "<title>", escapeHtml title, "</title>"
    , "<style>"
    , "@font-face{font-family:ui;src:local('IBM Plex Sans'),local('Space Grotesk'),local('Avenir Next'),local('Segoe UI');}"
    , "*{box-sizing:border-box;}"
    , "body{margin:0;font-family:ui,system-ui,sans-serif;background:radial-gradient(circle at 10% 10%,#f2e8d5 0,#f8f3ea 30%,#f5f7fb 70%);color:#1e1d1a;}"
    , "main{max-width:920px;margin:40px auto;padding:0 24px;}"
    , ".shell{background:rgba(255,255,255,0.8);border-radius:20px;padding:28px 32px;box-shadow:0 20px 60px rgba(20,20,20,0.12);backdrop-filter:blur(10px);border:1px solid rgba(255,255,255,0.5);}"
    , "h1{font-size:32px;margin:0 0 12px 0;}"
    , "p{line-height:1.6;}"
    , ".btn{display:inline-block;padding:10px 16px;border-radius:999px;background:#1f2937;color:#fff;text-decoration:none;font-weight:600;border:none;cursor:pointer;}"
    , ".btn + .btn{margin-left:8px;}"
    , ".btn.subtle{background:#e8e1d6;color:#2c2a25;}"
    , ".header{display:flex;justify-content:space-between;align-items:center;margin-bottom:20px;}"
    , ".actions{display:flex;gap:12px;align-items:center;}"
    , ".user{padding:6px 12px;border-radius:999px;background:#f0ebe2;font-weight:600;}"
    , ".card{display:flex;flex-direction:column;gap:12px;padding:18px;border-radius:16px;background:#fff;border:1px solid #eee;}"
    , "label{font-weight:600;}"
    , "input,select{padding:10px 12px;border-radius:12px;border:1px solid #ddd;font-size:14px;}"
    , "table{width:100%;border-collapse:collapse;margin-top:12px;}"
    , "th,td{text-align:left;padding:10px;border-bottom:1px solid #eee;font-size:14px;}"
    , ".footer{margin-top:24px;}"
    , ".alert{background:#fce8e8;color:#9b1c1c;padding:10px 12px;border-radius:10px;margin-bottom:16px;}"
    , "code{background:#f4f4f0;padding:2px 6px;border-radius:6px;}"
    , "</style></head><body><main><div class=\"shell\">"
    , body
    , "</div></main></body></html>"
    ]

escapeHtml :: Text -> Text
escapeHtml =
  T.concatMap
    (\case
      '<' -> "&lt;"
      '>' -> "&gt;"
      '&' -> "&amp;"
      '"' -> "&quot;"
      '\'' -> "&#39;"
      c -> T.singleton c
    )


-- Form parsing

parseForm :: BL.ByteString -> [(BS.ByteString, BS.ByteString)]
parseForm body =
  let parsed = parseQuery (BL.toStrict body)
   in catMaybes (map flatten parsed)
  where
    flatten (k, Just v) = Just (k, v)
    flatten _ = Nothing

lookupParam :: BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> Maybe Text
lookupParam key params = TE.decodeUtf8 <$> lookup key params

readInt64 :: Text -> Maybe Int64
readInt64 t =
  case reads (T.unpack t) of
    [(n, "")] -> Just n
    _ -> Nothing


-- Session + DB

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


withDb :: AppConfig -> (Connection -> IO a) -> IO a
withDb cfg action =
  case cfg.appDbConnString of
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
        , "CREATE TABLE IF NOT EXISTS webhooks (id SERIAL PRIMARY KEY, project_id INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE, webhook_id BIGINT, webhook_url TEXT, secret TEXT NOT NULL, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE (project_id));"
        ]
  forM_ migrations (execute_ conn)

syncFlakeFromDb :: AppConfig -> IO ()
syncFlakeFromDb cfg = do
  projects <- withDb cfg loadProjects
  _ <- regenerateFlake cfg projects
  writeGitCredentials cfg
  pure ()

loadProjects :: Connection -> IO [ProjectRow]
loadProjects conn =
  query_ conn "SELECT id, org, project, git_host, repo_id, repo_url, repo_path, flake_input, default_env_hash FROM projects ORDER BY org, project"

getSessionInfo :: AppConfig -> Wai.Request -> IO (Maybe SessionInfo)
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


-- OAuth and GitLab

requireSecrets :: AppConfig -> GitlabSecrets
requireSecrets cfg =
  case cfg.appGitlabSecrets of
    Just s -> s
    Nothing -> error "GitLab secrets not configured"

selectGitlabHost :: AppConfig -> Wai.Request -> Maybe Text
selectGitlabHost cfg req =
  case lookup "host" (Wai.queryString req) >>= id of
    Nothing -> listToMaybe cfg.appGitlabHosts
    Just hostRaw ->
      let host = TE.decodeUtf8 hostRaw
       in if host `elem` cfg.appGitlabHosts then Just host else Nothing

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
oauthRedirectUri cfg = cfg.appUiBaseUrl <> uiPath cfg "/oauth/gitlab/callback"

exchangeOAuthCode :: AppConfig -> Text -> Text -> IO (Either Text GitlabTokenResponse)
exchangeOAuthCode cfg host code = do
  let secrets = requireSecrets cfg
  manager <- requireManager cfg
  initialReq <- parseRequest (T.unpack ("https://" <> host <> "/oauth/token"))
  let params =
        [ ("client_id", TE.encodeUtf8 secrets.gitlabClientId)
        , ("client_secret", TE.encodeUtf8 secrets.gitlabClientSecret)
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
  let userIdVal = sess.sessionUser.userId
  tokenRows <- query conn "SELECT git_host, token FROM gitlab_tokens WHERE user_id = ? AND project_id IS NULL ORDER BY created_at DESC LIMIT 1" (Only userIdVal)
  case tokenRows of
    [] -> pure (Left "No GitLab token available for this user")
    ((host, token):_) -> fetchGitlabProjects cfg host token

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

requireManager :: AppConfig -> IO Manager
requireManager cfg =
  case cfg.appHttpManager of
    Just mgr -> pure mgr
    Nothing -> error "HTTP manager not configured"


-- GitLab JSON types

data GitlabTokenResponse = GitlabTokenResponse
  { tokenAccessToken :: Text
  , tokenType :: Text
  , tokenScope :: Text
  } deriving (Eq, Show, Generic)

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
  } deriving (Eq, Show, Generic)

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
  } deriving (Eq, Show, Generic)

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
  } deriving (Eq, Show, Generic)

instance FromJSON GitlabHook where
  parseJSON = A.withObject "GitlabHook" $ \o ->
    GitlabHook
      <$> o .: "id"
      <*> o .: "url"


upsertUserSession :: AppConfig -> Text -> GitlabUser -> GitlabTokenResponse -> IO SessionInfo
upsertUserSession cfg host glUser token = withDb cfg $ \conn -> do
  rows <- query conn "SELECT id FROM users WHERE gitlab_user_id = ?" (Only glUser.glUserId)
  userIdVal <- case rows of
    (Only uid:_) -> do
      _ <- execute conn "UPDATE users SET username = ?, name = ?, updated_at = now() WHERE id = ?" (glUser.glUserUsername, glUser.glUserName, uid)
      pure uid
    [] -> do
      [Only uid] <- query conn "INSERT INTO users (gitlab_user_id, username, name) VALUES (?, ?, ?) RETURNING id" (glUser.glUserId, glUser.glUserUsername, glUser.glUserName)
      pure uid
  _ <- execute conn "INSERT INTO gitlab_tokens (user_id, project_id, git_host, token, scopes) VALUES (?, NULL, ?, ?, ?)" (userIdVal, host, token.tokenAccessToken, token.tokenScope)
  createSession conn userIdVal


-- Project add flow

addProjectFlow :: AppConfig -> SessionInfo -> Int64 -> Maybe Text -> Maybe Text -> IO (Either Text Text)
addProjectFlow cfg sess repoId orgInput projectInput = withDb cfg $ \conn -> do
  tokenRows <- query conn "SELECT git_host, token FROM gitlab_tokens WHERE user_id = ? AND project_id IS NULL ORDER BY created_at DESC LIMIT 1" (Only sess.sessionUser.userId)
  case tokenRows of
    [] -> pure (Left "Missing GitLab token for user")
    ((host, token):_) -> do
      projectInfo <- fetchGitlabProject cfg host token repoId
      case projectInfo of
        Left msg -> pure (Left msg)
        Right repo -> do
          let (defaultOrg, defaultProject) = splitNamespace repo.glProjectPath
          let orgCandidate = fromMaybe defaultOrg orgInput
          let projCandidate = fromMaybe defaultProject projectInput
          let org = if T.strip orgCandidate == "" then sanitizeName defaultOrg else sanitizeName orgCandidate
          let proj = if T.strip projCandidate == "" then sanitizeName defaultProject else sanitizeName projCandidate
          if org == "" || proj == ""
            then pure (Left "Organisation and project names must be non-empty")
            else do
              let flakeInput = org <> "__" <> proj
              [projectRow] <- query conn
                "INSERT INTO projects (org, project, git_host, repo_id, repo_url, repo_path, flake_input) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT (git_host, repo_id) DO UPDATE SET org = EXCLUDED.org, project = EXCLUDED.project, repo_url = EXCLUDED.repo_url, repo_path = EXCLUDED.repo_path, flake_input = EXCLUDED.flake_input, updated_at = now() RETURNING id, org, project, git_host, repo_id, repo_url, repo_path, flake_input, default_env_hash"
                (org, proj, host, repo.glProjectId, repo.glProjectHttpUrl, repo.glProjectPath, flakeInput)
              _ <- execute conn "DELETE FROM gitlab_tokens WHERE project_id = ?" (Only projectRow.projectId)
              _ <- execute conn "INSERT INTO gitlab_tokens (user_id, project_id, git_host, token, scopes) VALUES (?, ?, ?, ?, ?)" (sess.sessionUser.userId, projectRow.projectId, host, token, ("api read_repository" :: Text))
              writeGitCredentials cfg
              regenResult <- regenerateFlake cfg =<< loadProjects conn
              case regenResult of
                Left msg -> pure (Left msg)
                Right _ -> do
                  cmdRes <- runCommand cfg (CommandSpec "nix" ["flake", "update", flakeInput] (appWorkDir cfg))
                  case cmdRes of
                    Left err -> pure (Left (commandErrorText err))
                    Right _ -> do
                      planRes <- runCommand cfg (CommandSpec "nix" ["run", ".#hostenv-provider-plan"] (appWorkDir cfg))
                      case planRes of
                        Left err -> pure (Left (commandErrorText err))
                        Right _ -> do
                          planRaw <- loadPlan cfg
                          case projectHashFor org proj planRaw of
                            Left err -> pure (Left err)
                            Right projHash -> do
                              _ <- execute conn "UPDATE projects SET default_env_hash = ?, updated_at = now() WHERE id = ?" (projHash, projectRow.projectId)
                              webhookResult <- ensureWebhook cfg conn host token repoId projectRow projHash
                              case webhookResult of
                                Left msg -> pure (Left msg)
                                Right (secret, _) -> do
                                  maybeWriteSecret cfg projHash org proj secret
                                  let hookUrl = "https://" <> cfg.appWebhookHost <> "/webhook/" <> projHash
                                  pure (Right ("Webhook configured at " <> hookUrl))

ensureWebhook :: AppConfig -> Connection -> Text -> Text -> Int64 -> ProjectRow -> Text -> IO (Either Text (Text, Maybe Int64))
ensureWebhook cfg conn host token repoId projectRow projHash = do
  existing <- query conn "SELECT secret, webhook_id FROM webhooks WHERE project_id = ?" (Only projectRow.projectId)
  (secret, hookId) <- case existing of
    ((s, mId):_) -> pure (s, mId)
    [] -> do
      s <- randomToken 24
      pure (s, Nothing)
  let url = "https://" <> cfg.appWebhookHost <> "/webhook/" <> projHash
  hookResult <- case hookId of
    Just hid -> do
      updated <- updateGitlabWebhook cfg host token repoId hid url secret
      case updated of
        Left _ -> createGitlabWebhook cfg host token repoId url secret
        Right hook -> pure (Right hook)
    Nothing -> createGitlabWebhook cfg host token repoId url secret
  case hookResult of
    Left msg -> pure (Left msg)
    Right hook -> do
      _ <- execute conn "INSERT INTO webhooks (project_id, secret, webhook_id, webhook_url) VALUES (?, ?, ?, ?) ON CONFLICT (project_id) DO UPDATE SET secret = EXCLUDED.secret, webhook_id = EXCLUDED.webhook_id, webhook_url = EXCLUDED.webhook_url, updated_at = now()" (projectRow.projectId, secret, hook.glHookId, hook.glHookUrl)
      pure (Right (secret, Just hook.glHookId))


splitNamespace :: Text -> (Text, Text)
splitNamespace path =
  case T.splitOn "/" path of
    [] -> ("", path)
    [single] -> (single, single)
    (x:xs) -> (x, last xs)

sanitizeName :: Text -> Text
sanitizeName = T.filter isAlphaNum . T.toLower


maybeWriteSecret :: AppConfig -> Text -> Text -> Text -> Text -> IO ()
maybeWriteSecret cfg hash org proj secret =
  case cfg.appWebhookSecretsDir of
    Nothing -> pure ()
    Just dir -> do
      createDirectoryIfMissing True dir
      let byHash = dir </> T.unpack hash
      let byProject = dir </> T.unpack (org <> "__" <> proj)
      writeSecretFile byHash secret
      writeSecretFile byProject secret

writeSecretFile :: FilePath -> Text -> IO ()
writeSecretFile path secret = do
  BSC.writeFile path (TE.encodeUtf8 secret)
  setFileMode path 0o640


writeGitCredentials :: AppConfig -> IO ()
writeGitCredentials cfg =
  case cfg.appDbConnString of
    Nothing -> pure ()
    Just _ -> do
      entries <- withDb cfg $ \conn -> do
        rows <- query_ conn "SELECT projects.repo_url, gitlab_tokens.token FROM gitlab_tokens JOIN projects ON gitlab_tokens.project_id = projects.id WHERE gitlab_tokens.project_id IS NOT NULL ORDER BY projects.repo_url"
        pure rows
      let credsText = renderGitCredentials entries
      createDirectoryIfMissing True (takeDirectory cfg.appGitCredentialsPath)
      BSC.writeFile cfg.appGitCredentialsPath (TE.encodeUtf8 credsText)
      setFileMode cfg.appGitCredentialsPath 0o640

regenerateFlake :: AppConfig -> [ProjectRow] -> IO (Either Text ())
regenerateFlake cfg projects = do
  let templatePath = resolvePath cfg cfg.appFlakeTemplate
  exists <- doesFileExist templatePath
  if not exists
    then pure (Left ("flake template not found: " <> T.pack templatePath))
    else do
      templateText <- T.pack <$> readFile templatePath
      let inputs = [ (p.projectFlakeInput, projectInputUrl p) | p <- projects ]
      let inputBlock = renderProjectInputs inputs
      case renderFlakeTemplate templateText inputBlock of
        Left err -> pure (Left err)
        Right flakeText -> do
          let flakePath = appWorkDir cfg </> "flake.nix"
          writeFile flakePath (T.unpack flakeText)
          pure (Right ())

projectInputUrl :: ProjectRow -> Text
projectInputUrl p =
  let base = if p.projectGitHost == "gitlab.com" then "gitlab:" else "gitlab:" <> p.projectGitHost <> "/"
   in base <> p.projectRepoPath <> "?dir=.hostenv"


commandErrorText :: CommandError -> Text
commandErrorText cmdErr =
  T.unlines
    [ "Command failed: " <> renderCommand cmdErr.errSpec
    , "Exit: " <> T.pack (show cmdErr.errExit)
    , "stdout:\n" <> cmdErr.errStdout
    , "stderr:\n" <> cmdErr.errStderr
    ]


-- Webhook signature resolution

data SecretInfo = SecretInfo
  { secretConfigured :: Bool
  , secretValue :: Maybe BS.ByteString
  }

verifyWebhook :: SecretInfo -> Maybe Text -> Maybe Text -> BL.ByteString -> Handler ()
verifyWebhook secretInfo mHubSig mGitlabToken rawBody = do
  let hasGitHub = mHubSig /= Nothing
  let hasGitLab = mGitlabToken /= Nothing
  case secretInfo.secretValue of
    Nothing ->
      if secretInfo.secretConfigured && (hasGitHub || hasGitLab)
        then throwError (errorWithBody err401 (ErrorResponse "webhook secret not configured" Nothing Nothing Nothing Nothing))
        else if secretInfo.secretConfigured
          then throwError (errorWithBody err401 (ErrorResponse "missing webhook signature" Nothing Nothing Nothing Nothing))
          else pure ()
    Just secret ->
      case (mHubSig, mGitlabToken) of
        (Nothing, Nothing) ->
          throwError (errorWithBody err401 (ErrorResponse "missing webhook signature" Nothing Nothing Nothing Nothing))
        _ -> do
          let githubOk = maybe False (\sigText -> verifyGitHubSignature secret rawBody (TE.encodeUtf8 sigText)) mHubSig
          let gitlabOk = maybe False (\tokText -> verifyGitLabToken secret (TE.encodeUtf8 tokText)) mGitlabToken
          unless (githubOk || gitlabOk) $
            throwError (errorWithBody err401 (ErrorResponse "invalid webhook signature" Nothing Nothing Nothing Nothing))

resolveSecret :: AppConfig -> Text -> ProjectRef -> IO SecretInfo
resolveSecret cfg hash ref = do
  let configured = True
  dbSecret <- case cfg.appDbConnString of
    Nothing -> pure Nothing
    Just _ -> withDb cfg $ \conn -> do
      rows <- query conn "SELECT secret FROM webhooks JOIN projects ON webhooks.project_id = projects.id WHERE projects.default_env_hash = ?" (Only hash)
      pure (listToMaybe (map fromOnly rows))
  case dbSecret of
    Just secret -> pure (SecretInfo configured (Just (TE.encodeUtf8 secret)))
    Nothing -> do
      fromDir <- case cfg.appWebhookSecretsDir of
        Nothing -> pure Nothing
        Just dir -> do
          let byHash = dir </> T.unpack hash
          let byProject = dir </> T.unpack (ref.prOrg <> "__" <> ref.prProject)
          pickFirstExisting [byHash, byProject]
      case fromDir of
        Just secret -> pure (SecretInfo configured (Just secret))
        Nothing ->
          case cfg.appWebhookSecretFile of
            Nothing -> pure (SecretInfo configured Nothing)
            Just path -> do
              secret <- readSecret path
              pure (SecretInfo configured (Just secret))


-- Server / command helpers

serverError :: WebhookError -> ServerError
serverError err =
  case err of
    WebhookPlanError msg -> errorWithBody err500 (ErrorResponse msg Nothing Nothing Nothing Nothing)
    WebhookCommandError cmdErr ->
      let cmd = renderCommand cmdErr.errSpec
          exitCode = exitCodeToInt cmdErr.errExit
          response = ErrorResponse "command failed" (Just cmd) exitCode (Just cmdErr.errStdout) (Just cmdErr.errStderr)
       in errorWithBody err500 response

errorWithBody :: ToJSON a => ServerError -> a -> ServerError
errorWithBody base payload =
  base
    { errBody = A.encode payload
    , errHeaders = [("Content-Type", "application/json")]
    }

renderCommand :: CommandSpec -> Text
renderCommand spec = T.unwords (spec.cmdName : spec.cmdArgs)

exitCodeToInt :: ExitCode -> Maybe Int
exitCodeToInt ExitSuccess = Just 0
exitCodeToInt (ExitFailure n) = Just n


loadPlan :: AppConfig -> IO BL.ByteString
loadPlan cfg = BL.readFile (cfg.appWebhookConfig.whPlanPath)


ensureProviderRepo :: AppConfig -> IO ()
ensureProviderRepo cfg = do
  createDirectoryIfMissing True cfg.appDataDir
  createDirectoryIfMissing True (appWorkDir cfg </> "generated")
  let flakePath = appWorkDir cfg </> "flake.nix"
  let statePath = appWorkDir cfg </> "generated" </> "state.json"
  hasFlake <- doesFileExist flakePath
  unless hasFlake $ do
    srcExists <- doesDirectoryExist cfg.appRepoSource
    unless srcExists $ do
      hPutStrLn stderr "hostenv-provider-service: repoSource not found"
      exitFailure
    let srcContents = cfg.appRepoSource </> "."
    runCommandOrDie cfg (CommandSpec "cp" ["-a", "-n", T.pack srcContents, T.pack cfg.appDataDir] cfg.appDataDir)
  hasState <- doesFileExist statePath
  unless hasState $
    BL.writeFile statePath "{}\n"


openUnixSocket :: FilePath -> IO Socket
openUnixSocket path = do
  createDirectoryIfMissing True (takeDirectory path)
  exists <- doesFileExist path
  when exists (removeFile path)
  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock (SockAddrUnix path)
  listen sock 1024
  setFileMode path 0o660
  pure sock


runCommand :: AppConfig -> CommandRunner
runCommand _ spec = do
  let cmd = spec.cmdName
  let args = spec.cmdArgs
  let cwd = spec.cmdCwd
  withCurrentDirectory cwd $ do
    (code, out, err) <- Sh.procStrictWithErr cmd args Sh.empty
    case code of
      ExitSuccess -> pure (Right (CommandOutput out err))
      _ -> pure (Left (Service.CommandError spec code out err))


runCommandOrDie :: AppConfig -> CommandSpec -> IO ()
runCommandOrDie cfg spec = do
  res <- runCommand cfg spec
  case res of
    Right _ -> pure ()
    Left err -> do
      hPutStrLn stderr ("hostenv-provider-service: " <> T.unpack (renderCommand err.errSpec))
      hPutStrLn stderr (T.unpack err.errStderr)
      exitFailure


appWorkDir :: AppConfig -> FilePath
appWorkDir cfg = cfg.appDataDir </> cfg.appFlakeRoot


loadConfig :: IO AppConfig
loadConfig = do
  dataDir <- do
    override <- lookupEnv "HOSTENV_PROVIDER_DATA_DIR"
    case override of
      Just v -> pure v
      Nothing -> getXdgDirectory XdgData "hostenv-provider"

  repoSource <- requireEnv "HOSTENV_PROVIDER_REPO_SOURCE"
  flakeRoot <- fromMaybe "." <$> lookupEnv "HOSTENV_PROVIDER_FLAKE_ROOT"
  listenSocket <- requireEnv "HOSTENV_PROVIDER_LISTEN_SOCKET"
  secretFile <- lookupEnv "HOSTENV_PROVIDER_WEBHOOK_SECRET_FILE"
  secretsDir <- lookupEnv "HOSTENV_PROVIDER_WEBHOOK_SECRETS_DIR"
  webhookHost <- T.pack <$> requireEnv "HOSTENV_PROVIDER_WEBHOOK_HOST"

  uiBasePath <- T.pack <$> (fromMaybe "/ui" <$> lookupEnv "HOSTENV_PROVIDER_UI_BASE_PATH")
  uiBaseUrl <- T.pack <$> (fromMaybe "https://example.invalid" <$> lookupEnv "HOSTENV_PROVIDER_UI_BASE_URL")

  dbUri <- Just <$> requireEnv "HOSTENV_PROVIDER_DB_URI"
  gitlabHosts <- fmap parseList (lookupEnv "HOSTENV_PROVIDER_GITLAB_HOSTS")
  gitConfigPath <- fromMaybe (dataDir </> "gitconfig") <$> lookupEnv "HOSTENV_PROVIDER_GIT_CONFIG_FILE"
  gitCredentialsPath <- fromMaybe (dataDir </> "git-credentials") <$> lookupEnv "HOSTENV_PROVIDER_GIT_CREDENTIALS_FILE"
  flakeTemplate <- fromMaybe "flake.template.nix" <$> lookupEnv "HOSTENV_PROVIDER_FLAKE_TEMPLATE"

  mSecretsPath <- lookupEnv "HOSTENV_PROVIDER_GITLAB_SECRETS_FILE"
  secrets <- case mSecretsPath of
    Nothing -> die "GitLab OAuth secrets file missing"
    Just path -> Just <$> readGitlabSecrets path

  manager <- Just <$> newManager tlsManagerSettings

  let workDir = dataDir </> flakeRoot
  let planPath = workDir </> "generated" </> "plan.json"
  let webhookCfg = WebhookConfig workDir planPath

  pure
    AppConfig
      { appDataDir = dataDir
      , appRepoSource = repoSource
      , appFlakeRoot = flakeRoot
      , appListenSocket = listenSocket
      , appWebhookSecretFile = secretFile
      , appWebhookSecretsDir = secretsDir
      , appWebhookConfig = webhookCfg
      , appUiBasePath = normalizeBasePath uiBasePath
      , appUiBaseUrl = T.dropWhileEnd (== '/') uiBaseUrl
      , appWebhookHost = webhookHost
      , appDbConnString = BSC.pack <$> dbUri
      , appGitlabSecrets = secrets
      , appGitlabHosts = if null gitlabHosts then ["gitlab.com"] else gitlabHosts
      , appGitConfigPath = gitConfigPath
      , appGitCredentialsPath = gitCredentialsPath
      , appFlakeTemplate = flakeTemplate
      , appHttpManager = manager
      }

parseList :: Maybe String -> [Text]
parseList Nothing = []
parseList (Just raw) =
  filter (/= "") (map T.strip (T.splitOn "," (T.pack raw)))


requireEnv :: String -> IO String
requireEnv key = do
  val <- lookupEnv key
  case val of
    Just v -> pure v
    Nothing -> die ("missing required env var: " <> key)


readSecret :: FilePath -> IO BS.ByteString
readSecret path = do
  raw <- BS.readFile path
  let trimmed = BSC.dropWhile isSpace (BSC.dropWhileEnd isSpace raw)
  if BS.null trimmed
    then die "webhook secret file is empty"
    else pure trimmed

pickFirstExisting :: [FilePath] -> IO (Maybe BS.ByteString)
pickFirstExisting [] = pure Nothing
pickFirstExisting (p:ps) = do
  exists <- doesFileExist p
  if exists
    then Just <$> readSecret p
    else pickFirstExisting ps


ensureGitConfig :: AppConfig -> IO ()
ensureGitConfig cfg = do
  createDirectoryIfMissing True (takeDirectory cfg.appGitConfigPath)
  let content = unlines
        [ "[credential]"
        , "\thelper = store --file " <> cfg.appGitCredentialsPath
        ]
  writeFile cfg.appGitConfigPath content
  setEnv "GIT_CONFIG_GLOBAL" cfg.appGitConfigPath
  setEnv "GIT_TERMINAL_PROMPT" "0"

resolvePath :: AppConfig -> FilePath -> FilePath
resolvePath cfg path =
  if isAbsolute path then path else appWorkDir cfg </> path


randomToken :: Int -> IO Text
randomToken bytes = do
  raw <- getRandomBytes bytes
  pure (TE.decodeUtf8 (BAE.convertToBase BAE.Base16 (raw :: BS.ByteString)))


die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitFailure
