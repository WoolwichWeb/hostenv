{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.UI.Handlers
  ( handleLogin
  , handleLogout
  , handleIndex
  , handleAddProjectGet
  , handleAddProjectPost
  , handleBootstrapRepoGet
  , handleBootstrapRepoPost
  , handleOauthStart
  , handleOauthCallback
  ) where

import Control.Concurrent.MVar (MVar, withMVar)
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (hLocation, status200, status302, status400, status403, status500)
import Network.HTTP.Types.URI (renderSimpleQuery)
import qualified Network.Wai as Wai
import Network.Wai (strictRequestBody)

import Hostenv.Provider.Config (AppConfig(..), uiPath)
import Hostenv.Provider.DB (SessionInfo(..), User(..), getSessionInfo, loadProjects, logoutCookie, renderSessionCookie, withDb)
import Hostenv.Provider.Gitlab (GitlabTokenResponse(..), UpsertUserSessionError(..), consumeOauthState, createOauthState, exchangeOAuthCode, fetchGitlabUser, loadUserProjects, oauthRedirectUri, requireSecrets, selectGitlabHost, upsertUserSession)
import Hostenv.Provider.Project (addProjectFlow, bootstrapRepoFlow)
import Hostenv.Provider.Repo (RepoStatus(..))
import Hostenv.Provider.Service (GitlabSecrets(..))
import Hostenv.Provider.UI.Helpers (respondHtml, respondHtmlWithHeaders, respondRedirect)
import Hostenv.Provider.UI.Views (accessDeniedPage, addProjectPage, bootstrapRepoPage, errorPage, indexPage, loginPage, successPage)
import Hostenv.Provider.Util (lookupParam, parseForm, readInt64)


csrfFieldName :: BSC.ByteString
csrfFieldName = "csrf"

handleLogin :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleLogin cfg _ respond =
  respondHtml respond status200 (loginPage cfg Nothing)

handleLogout :: AppConfig -> (Wai.Response -> IO a) -> IO a
handleLogout cfg respond =
  respondHtmlWithHeaders respond status302 [(hLocation, TE.encodeUtf8 (uiPath cfg "/login")), ("Set-Cookie", logoutCookie)] mempty

handleIndex :: IORef RepoStatus -> AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleIndex repoStatusRef cfg req respond =
  requireAdmin cfg req respond $ \sess -> do
    repoStatus <- readIORef repoStatusRef
    projects <-
      case repoStatus of
        RepoMissing -> pure []
        RepoReady -> withDb cfg loadProjects
    respondHtml respond status200 (indexPage cfg sess repoStatus projects)

handleAddProjectGet :: IORef RepoStatus -> AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleAddProjectGet repoStatusRef cfg req respond =
  requireAdmin cfg req respond $ \sess -> do
    repoStatus <- readIORef repoStatusRef
    case repoStatus of
      RepoMissing -> respondRedirect respond cfg "/bootstrap-repo"
      RepoReady -> do
        result <- loadUserProjects cfg sess
        case result of
          Left msg -> respondHtml respond status500 (errorPage cfg msg)
          Right repos -> respondHtml respond status200 (addProjectPage cfg sess repos)

handleAddProjectPost :: IORef RepoStatus -> AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleAddProjectPost repoStatusRef cfg req respond =
  requireAdmin cfg req respond $ \sess -> do
    repoStatus <- readIORef repoStatusRef
    case repoStatus of
      RepoMissing -> respondRedirect respond cfg "/bootstrap-repo"
      RepoReady -> do
        body <- strictRequestBody req
        let params = parseForm body
        case lookupParam csrfFieldName params of
          Nothing -> respondHtml respond status400 (errorPage cfg "Missing CSRF token")
          Just csrfToken ->
            if csrfToken /= sess.csrf
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

handleBootstrapRepoGet :: IORef RepoStatus -> AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleBootstrapRepoGet repoStatusRef cfg req respond =
  requireAdmin cfg req respond $ \sess -> do
    repoStatus <- readIORef repoStatusRef
    case repoStatus of
      RepoReady -> respondRedirect respond cfg "/"
      RepoMissing -> do
        result <- loadUserProjects cfg sess
        case result of
          Left msg -> respondHtml respond status500 (errorPage cfg msg)
          Right repos -> respondHtml respond status200 (bootstrapRepoPage cfg sess repos)

handleBootstrapRepoPost :: MVar () -> IORef RepoStatus -> AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleBootstrapRepoPost bootstrapLock repoStatusRef cfg req respond =
  requireAdmin cfg req respond $ \sess -> do
    body <- strictRequestBody req
    let params = parseForm body
    case lookupParam csrfFieldName params of
      Nothing -> respondHtml respond status400 (errorPage cfg "Missing CSRF token")
      Just csrfToken ->
        if csrfToken /= sess.csrf
          then respondHtml respond status403 (errorPage cfg "Invalid CSRF token")
          else do
            let mRepoId = lookupParam "repo_id" params >>= readInt64
            case mRepoId of
              Nothing -> respondHtml respond status400 (errorPage cfg "Missing repository selection")
              Just repoId ->
                withMVar bootstrapLock $ \_ -> do
                  repoStatus <- readIORef repoStatusRef
                  case repoStatus of
                    RepoReady -> respondHtml respond status200 (successPage cfg "Provider repository is already bootstrapped")
                    RepoMissing -> do
                      result <- bootstrapRepoFlow cfg sess repoId
                      case result of
                        Left msg -> respondHtml respond status500 (errorPage cfg msg)
                        Right successMsg -> do
                          writeIORef repoStatusRef RepoReady
                          respondHtml respond status200 (successPage cfg successMsg)

handleOauthStart :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleOauthStart cfg req respond = do
  case requireSecrets cfg of
    Left err -> respondHtml respond status400 (errorPage cfg err)
    Right GitlabSecrets { gitlabClientId = clientId } -> do
      let host = selectGitlabHost cfg req
      case host of
        Nothing -> respondHtml respond status400 (errorPage cfg "No GitLab host configured")
        Just glHost -> do
          state <- createOauthState cfg glHost
          let redirectUri = oauthRedirectUri cfg
          let params =
                [ ("client_id", TE.encodeUtf8 clientId)
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
          respondHtmlWithHeaders respond status302 [(hLocation, TE.encodeUtf8 url)] mempty

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
              let GitlabTokenResponse { tokenAccessToken = accessToken } = token
              mUser <- fetchGitlabUser cfg glHost accessToken
              case mUser of
                Left msg -> respondHtml respond status500 (errorPage cfg msg)
                Right glUser -> do
                  sessionResult <- upsertUserSession cfg glHost glUser token
                  case sessionResult of
                    Left (AccessDenied msg) -> respondHtml respond status403 (errorPage cfg msg)
                    Left (InternalError msg) -> respondHtml respond status500 (errorPage cfg msg)
                    Right session -> do
                      let cookieHeader = ("Set-Cookie", renderSessionCookie session)
                      respondHtmlWithHeaders respond status302 [(hLocation, TE.encodeUtf8 (uiPath cfg "/")), cookieHeader] mempty
    _ -> respondHtml respond status400 (errorPage cfg "Missing OAuth callback parameters")

requireAdmin :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> (SessionInfo -> IO a) -> IO a
requireAdmin cfg req respond onAdmin = do
  mSession <- getSessionInfo cfg req
  case mSession of
    Nothing -> respondRedirect respond cfg "/login"
    Just sess ->
      let SessionInfo { user = User { role = role } } = sess
       in if role /= "admin"
        then respondHtml respond status403 (accessDeniedPage cfg)
        else onAdmin sess
