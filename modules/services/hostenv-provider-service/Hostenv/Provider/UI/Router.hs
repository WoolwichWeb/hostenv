{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.UI.Router
  ( uiApp
  ) where

import Data.IORef (IORef)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Control.Concurrent.STM (atomically, readTChan)
import Network.HTTP.Types (methodGet, methodPost, status403, status404)
import qualified Network.Wai as Wai
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS

import Hostenv.Provider.Config (AppConfig(..), normalizeBasePath)
import Hostenv.Provider.DB (SessionInfo(..), User(..), getSessionInfo)
import Hostenv.Provider.Jobs (JobRuntime, JobSummary(..), duplicateBroadcastChannel, loadJobById)
import Hostenv.Provider.Repo (RepoStatus)
import Hostenv.Provider.UI.Handlers
  ( handleAddProjectGet
  , handleAddProjectPost
  , handleBootstrapRepoGet
  , handleBootstrapRepoPost
  , handleDeployActionsGet
  , handleDeployStatusGet
  , handleIndex
  , handleJobDeployEventsGet
  , handleJobEventsGet
  , handleJobGet
  , handleLogin
  , handleLogout
  , handleOauthCallback
  , handleOauthStart
  )


uiApp :: JobRuntime -> IORef RepoStatus -> AppConfig -> Application
uiApp runtime repoStatusRef cfg req respond = do
  let AppConfig { appUiBasePath = basePath } = cfg
  case stripBasePath basePath (Wai.pathInfo req) of
    Nothing -> respond (responseLBS status404 [] "")
    Just rest -> routeUi runtime repoStatusRef cfg rest req respond

stripBasePath :: Text -> [Text] -> Maybe [Text]
stripBasePath base segments =
  let baseSegs = filter (/= "") (T.splitOn "/" (normalizeBasePath base))
   in if baseSegs == []
        then Just segments
        else case splitAt (length baseSegs) segments of
          (prefix, remainder) | prefix == baseSegs -> Just remainder
          _ -> Nothing

routeUi :: JobRuntime -> IORef RepoStatus -> AppConfig -> [Text] -> Application
routeUi runtime repoStatusRef cfg segments req respond =
  case segments of
    [] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleIndex runtime repoStatusRef cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["login"] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleLogin cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["logout"] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleLogout cfg respond
        _ -> respond (responseLBS status404 [] "")
    ["oauth", "gitlab", "start"] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleOauthStart cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["oauth", "gitlab", "callback"] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleOauthCallback cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["add-project"] ->
      case Wai.requestMethod req of
        m | m == methodPost -> handleAddProjectPost runtime repoStatusRef cfg req respond
        m | m == methodGet -> handleAddProjectGet repoStatusRef cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["bootstrap-repo"] ->
      case Wai.requestMethod req of
        m | m == methodPost -> handleBootstrapRepoPost runtime repoStatusRef cfg req respond
        m | m == methodGet -> handleBootstrapRepoGet repoStatusRef cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["jobs", jobId] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleJobGet cfg jobId req respond
        _ -> respond (responseLBS status404 [] "")
    ["jobs", jobId, "events"] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleJobEventsGet cfg jobId req respond
        _ -> respond (responseLBS status404 [] "")
    ["jobs", jobId, "deploy-status"] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleDeployStatusGet cfg jobId req respond
        _ -> respond (responseLBS status404 [] "")
    ["jobs", jobId, "deploy-actions"] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleDeployActionsGet cfg jobId req respond
        _ -> respond (responseLBS status404 [] "")
    ["jobs", jobId, "deploy-events"] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleJobDeployEventsGet cfg jobId req respond
        _ -> respond (responseLBS status404 [] "")
    ["jobs", jobId, "ws"] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleJobWs runtime cfg jobId req respond
        _ -> respond (responseLBS status404 [] "")
    _ -> respond (responseLBS status404 [] "")

handleJobWs :: JobRuntime -> AppConfig -> Text -> Application
handleJobWs runtime cfg jobId req respond = do
  mSession <- getSessionInfo cfg req
  case mSession of
    Nothing -> respond (responseLBS status403 [("Content-Type", "text/plain")] "forbidden")
    Just sess ->
      let SessionInfo { user = User { role = role } } = sess
       in if role /= "admin"
        then respond (responseLBS status403 [("Content-Type", "text/plain")] "forbidden")
        else do
          mJob <- loadJobById cfg jobId
          case mJob of
            Nothing -> respond (responseLBS status404 [("Content-Type", "text/plain")] "job not found")
            Just job -> do
              let fallback = responseLBS status404 [("Content-Type", "text/plain")] "websocket required"
              websocketsOr WS.defaultConnectionOptions (jobSocketServer runtime job) (\_ respondFallback -> respondFallback fallback) req respond

jobSocketServer :: JobRuntime -> JobSummary -> WS.ServerApp
jobSocketServer runtime job pending = do
  connection <- WS.acceptRequest pending
  WS.sendTextData connection (A.encode (A.object [ "type" A..= ("job_snapshot" :: Text), "job" A..= job ]))
  channel <- duplicateBroadcastChannel runtime
  let loop = do
        message <- atomically (readTChan channel)
        case extractJobId message of
          Just msgJobId | msgJobId == job.id -> WS.sendTextData connection (A.encode message)
          _ -> pure ()
        loop
  loop

extractJobId :: A.Value -> Maybe Text
extractJobId value =
  case value of
    A.Object obj ->
      case KM.lookup (K.fromText "jobId") obj of
        Just (A.String t) -> Just t
        _ -> Nothing
    _ -> Nothing
