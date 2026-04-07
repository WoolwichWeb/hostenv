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
  , handleJobGet
  , handleJobEventsGet
  , handleDeployStatusGet
  , handleDeployActionsGet
  , handleJobDeployEventsGet
  , handleOauthStart
  , handleOauthCallback
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Status, hLocation, status200, status302, status400, status403, status404, status500)
import Network.HTTP.Types.URI (renderSimpleQuery)
import qualified Network.Wai as Wai
import Network.Wai (strictRequestBody)

import Hostenv.Provider.Config (AppConfig(..), uiPath)
import Hostenv.Provider.DB (SessionInfo(..), User(..), getSessionInfo, loadDeployActions, loadDeployEventsSince, loadDeployStatuses, loadProjects, logoutCookie, renderSessionCookie, withDb)
import Hostenv.Provider.Gitlab
  ( GitlabError
  , GitlabTokenResponse(..)
  , consumeOauthState
  , createOauthState
  , exchangeOAuthCode
  , fetchGitlabUser
  , isAccessDeniedError
  , isReauthError
  , loadUserProjects
  , oauthRedirectUri
  , renderGitlabError
  , requireSecrets
  , selectGitlabHost
  , upsertUserSession
  )
import Hostenv.Provider.Project (ProjectFlowError(..), addProjectFlow, bootstrapRepoFlow, projectFlowErrorText)
import Hostenv.Provider.Jobs (JobLogger(..), JobOutcome(..), JobRuntime, NewJob(..), enqueueJob, loadJobById, loadJobEventsSince, loadRecentJobs)
import Hostenv.Provider.Repo (RepoStatus(..))
import Hostenv.Provider.Service (GitlabSecrets(..))
import Hostenv.Provider.UI.Helpers (respondHtml, respondHtmlWithHeaders, respondJson, respondRedirect)
import Hostenv.Provider.UI.Views (accessDeniedPage, addProjectPage, bootstrapRepoPage, errorPage, indexPage, jobPage, loginPage)
import Hostenv.Provider.Util (lookupParam, parseForm, readInt64)


csrfFieldName :: BSC.ByteString
csrfFieldName = "csrf"

handleLogin :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleLogin cfg _ respond =
  respondHtml respond status200 (loginPage cfg Nothing)

handleLogout :: AppConfig -> (Wai.Response -> IO a) -> IO a
handleLogout cfg respond =
  respondHtmlWithHeaders respond status302 [(hLocation, TE.encodeUtf8 (uiPath cfg "/login")), ("Set-Cookie", logoutCookie)] mempty

handleIndex :: JobRuntime -> IORef RepoStatus -> AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleIndex _runtime repoStatusRef cfg req respond =
  requireAdmin cfg req respond $ \sess -> do
    repoStatus <- readIORef repoStatusRef
    projects <-
      case repoStatus of
        RepoMissing -> pure []
        RepoReady -> withDb cfg loadProjects
    jobs <- loadRecentJobs cfg
    respondHtml respond status200 (indexPage cfg sess repoStatus projects jobs)

handleAddProjectGet :: IORef RepoStatus -> AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleAddProjectGet repoStatusRef cfg req respond =
  requireAdmin cfg req respond $ \sess -> do
    repoStatus <- readIORef repoStatusRef
    case repoStatus of
      RepoMissing -> respondRedirect respond cfg "/bootstrap-repo"
      RepoReady -> do
        result <- loadUserProjects cfg sess
        case result of
          Left err -> respondGitlabFailure cfg respond err
          Right repos -> respondHtml respond status200 (addProjectPage cfg sess repos)

handleAddProjectPost :: JobRuntime -> IORef RepoStatus -> AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleAddProjectPost runtime repoStatusRef cfg req respond =
  requireAdmin cfg req respond $ \sess -> do
    repoStatus <- readIORef repoStatusRef
    case repoStatus of
      RepoMissing -> respondRedirect respond cfg "/bootstrap-repo"
      RepoReady -> do
        authCheck <- loadUserProjects cfg sess
        case authCheck of
          Left err -> respondGitlabFailure cfg respond err
          Right _ -> do
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
                        let payload =
                              A.object
                                [ "repoId" A..= repoId
                                , "orgInput" A..= orgInput
                                , "projectInput" A..= projectInput
                                ]
                            jobDef =
                              NewJob
                                { kind = "add_project"
                                , requestedByUserId = Just sess.user.id
                                , projectId = Nothing
                                , payload = payload
                                , run = \logger -> do
                                    logger.logInfo ("Adding project for repository id " <> T.pack (show repoId))
                                    result <- addProjectFlow cfg sess repoId orgInput projectInput
                                    case result of
                                      Left flowErr ->
                                        case flowErr of
                                          ProjectFlowAuthError msg -> pure (Left msg)
                                          ProjectFlowError _ -> pure (Left (projectFlowErrorText flowErr))
                                      Right successMsg -> pure (Right (JobComplete successMsg))
                                }
                        jobId <- enqueueJob runtime jobDef
                        respondRedirect respond cfg ("/jobs/" <> jobId)

handleBootstrapRepoGet :: IORef RepoStatus -> AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleBootstrapRepoGet repoStatusRef cfg req respond =
  requireAdmin cfg req respond $ \sess -> do
    repoStatus <- readIORef repoStatusRef
    case repoStatus of
      RepoReady -> respondRedirect respond cfg "/"
      RepoMissing -> do
        result <- loadUserProjects cfg sess
        case result of
          Left err -> respondGitlabFailure cfg respond err
          Right repos -> respondHtml respond status200 (bootstrapRepoPage cfg sess repos)

handleBootstrapRepoPost :: JobRuntime -> IORef RepoStatus -> AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleBootstrapRepoPost runtime repoStatusRef cfg req respond =
  requireAdmin cfg req respond $ \sess -> do
    body <- strictRequestBody req
    let params = parseForm body
    case lookupParam csrfFieldName params of
      Nothing -> respondHtml respond status400 (errorPage cfg "Missing CSRF token")
      Just csrfToken ->
        if csrfToken /= sess.csrf
          then respondHtml respond status403 (errorPage cfg "Invalid CSRF token")
          else do
            authCheck <- loadUserProjects cfg sess
            case authCheck of
              Left err -> respondGitlabFailure cfg respond err
              Right _ -> do
                let mRepoId = lookupParam "repo_id" params >>= readInt64
                case mRepoId of
                  Nothing -> respondHtml respond status400 (errorPage cfg "Missing repository selection")
                  Just repoId -> do
                    repoStatus <- readIORef repoStatusRef
                    case repoStatus of
                      RepoReady -> respondRedirect respond cfg "/"
                      RepoMissing -> do
                        let payload = A.object [ "repoId" A..= repoId ]
                            jobDef =
                              NewJob
                                { kind = "bootstrap_repo"
                                , requestedByUserId = Just sess.user.id
                                , projectId = Nothing
                                , payload = payload
                                , run = \logger -> do
                                    logger.logInfo ("Bootstrapping provider repository from repository id " <> T.pack (show repoId))
                                    result <- bootstrapRepoFlow cfg sess repoId
                                    case result of
                                      Left msg -> pure (Left msg)
                                      Right successMsg -> do
                                        writeIORef repoStatusRef RepoReady
                                        pure (Right (JobComplete successMsg))
                                }
                        jobId <- enqueueJob runtime jobDef
                        respondRedirect respond cfg ("/jobs/" <> jobId)

handleJobGet :: AppConfig -> Text -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleJobGet cfg jobId req respond =
  requireAdmin cfg req respond $ \sess -> do
    mJob <- loadJobById cfg jobId
    case mJob of
      Nothing -> respondHtml respond status404 (errorPage cfg "Job not found")
      Just job -> respondHtml respond status200 (jobPage cfg sess job)

handleJobEventsGet :: AppConfig -> Text -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleJobEventsGet cfg jobId req respond =
  handleJobEventsWith cfg jobId req respond loadJobEventsSince

handleDeployStatusGet :: AppConfig -> Text -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleDeployStatusGet cfg jobId req respond =
  requireAdmin cfg req respond $ \_sess -> do
    mJob <- loadJobById cfg jobId
    case mJob of
      Nothing -> respondHtml respond status404 (errorPage cfg "Job not found")
      Just _ -> do
        statuses <- loadDeployStatuses cfg jobId
        respondJson respond status200 statuses

handleDeployActionsGet :: AppConfig -> Text -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleDeployActionsGet cfg jobId req respond =
  requireAdmin cfg req respond $ \_sess -> do
    mJob <- loadJobById cfg jobId
    case mJob of
      Nothing -> respondHtml respond status404 (errorPage cfg "Job not found")
      Just _ -> do
        actions <- loadDeployActions cfg jobId
        respondJson respond status200 actions

handleJobDeployEventsGet :: AppConfig -> Text -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleJobDeployEventsGet cfg jobId req respond =
  handleJobEventsWith cfg jobId req respond loadDeployEventsSince

handleJobEventsWith :: A.ToJSON a1 => AppConfig -> Text -> Wai.Request -> (Wai.Response -> IO a) -> (AppConfig -> Text -> Int64 -> IO a1) -> IO a
handleJobEventsWith cfg jobId req respond loadEvents =
  requireAdmin cfg req respond $ \_sess -> do
    mJob <- loadJobById cfg jobId
    case mJob of
      Nothing -> respondHtml respond status404 (errorPage cfg "Job not found")
      Just _ -> do
        let after =
              case lookup "after" (Wai.queryString req) >>= id of
                Nothing -> 0
                Just raw ->
                  case readInt64 (TE.decodeUtf8 raw) of
                    Just n -> max 0 n
                    Nothing -> 0
        events <- loadEvents cfg jobId after
        respondJson respond status200 events

handleOauthStart :: AppConfig -> Wai.Request -> (Wai.Response -> IO a) -> IO a
handleOauthStart cfg req respond = do
  case requireSecrets cfg of
    Left err -> respondHtml respond status400 (errorPage cfg (renderGitlabError err))
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
      flowResult <- runExceptT (oauthCallbackSession cfg state code)
      case flowResult of
        Left callbackErr ->
          respondHtml respond callbackErr.status (errorPage cfg callbackErr.message)
        Right session -> do
          let cookieHeader = ("Set-Cookie", renderSessionCookie session)
          respondHtmlWithHeaders respond status302 [(hLocation, TE.encodeUtf8 (uiPath cfg "/")), cookieHeader] mempty
    _ -> respondHtml respond status400 (errorPage cfg "Missing OAuth callback parameters")

data OauthCallbackError = OauthCallbackError
  { status :: Status
  , message :: Text
  }

oauthCallbackSession :: AppConfig -> Text -> Text -> ExceptT OauthCallbackError IO SessionInfo
oauthCallbackSession cfg state code = do
  mHost <- lift (consumeOauthState cfg state)
  glHost <- case mHost of
    Nothing -> throwE (OauthCallbackError status400 "Invalid OAuth state")
    Just host -> pure host
  token <- hoistGitlab500 (exchangeOAuthCode cfg glHost code)
  let GitlabTokenResponse { accessToken = accessToken } = token
  glUser <- hoistGitlab500 (fetchGitlabUser cfg glHost accessToken)
  sessionResult <- lift (upsertUserSession cfg glHost glUser token)
  case sessionResult of
    Left err -> throwE (oauthSessionError err)
    Right session -> pure session
  where
    hoistGitlab500 :: IO (Either GitlabError a) -> ExceptT OauthCallbackError IO a
    hoistGitlab500 action = do
      result <- lift action
      case result of
        Left err -> throwE (OauthCallbackError status500 (renderGitlabError err))
        Right value -> pure value

    oauthSessionError :: GitlabError -> OauthCallbackError
    oauthSessionError err =
      let msg = renderGitlabError err
       in if isAccessDeniedError err
            then OauthCallbackError status403 msg
            else OauthCallbackError status500 msg

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

respondReauth :: AppConfig -> (Wai.Response -> IO a) -> Text -> IO a
respondReauth cfg respond msg =
  respondHtmlWithHeaders
    respond
    status403
    [("Set-Cookie", logoutCookie)]
    (errorPage cfg (msg <> "\nPlease sign in with GitLab again."))

respondGitlabFailure :: AppConfig -> (Wai.Response -> IO a) -> GitlabError -> IO a
respondGitlabFailure cfg respond gitlabErr =
  let msg = renderGitlabError gitlabErr
   in if isReauthError gitlabErr
        then respondReauth cfg respond msg
        else respondHtml respond status500 (errorPage cfg msg)
