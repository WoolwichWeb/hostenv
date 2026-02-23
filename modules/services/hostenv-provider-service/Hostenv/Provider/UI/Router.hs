{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.UI.Router
  ( uiApp
  ) where

import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (methodGet, methodPost, status404)
import qualified Network.Wai as Wai
import Network.Wai (Application, responseLBS)

import Hostenv.Provider.Config (AppConfig(..), normalizeBasePath)
import Hostenv.Provider.Repo (RepoStatus)
import Hostenv.Provider.UI.Handlers
  ( handleAddProjectGet
  , handleAddProjectPost
  , handleBootstrapRepoGet
  , handleBootstrapRepoPost
  , handleIndex
  , handleLogin
  , handleLogout
  , handleOauthCallback
  , handleOauthStart
  )


uiApp :: IORef RepoStatus -> MVar () -> AppConfig -> Application
uiApp repoStatusRef bootstrapLock cfg req respond = do
  let AppConfig { appUiBasePath = basePath } = cfg
  case stripBasePath basePath (Wai.pathInfo req) of
    Nothing -> respond (responseLBS status404 [] "")
    Just rest -> routeUi repoStatusRef bootstrapLock cfg rest req respond

stripBasePath :: Text -> [Text] -> Maybe [Text]
stripBasePath base segments =
  let baseSegs = filter (/= "") (T.splitOn "/" (normalizeBasePath base))
   in if baseSegs == []
        then Just segments
        else case splitAt (length baseSegs) segments of
          (prefix, remainder) | prefix == baseSegs -> Just remainder
          _ -> Nothing

routeUi :: IORef RepoStatus -> MVar () -> AppConfig -> [Text] -> Application
routeUi repoStatusRef bootstrapLock cfg segments req respond =
  case segments of
    [] ->
      case Wai.requestMethod req of
        m | m == methodGet -> handleIndex repoStatusRef cfg req respond
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
        m | m == methodPost -> handleAddProjectPost repoStatusRef cfg req respond
        m | m == methodGet -> handleAddProjectGet repoStatusRef cfg req respond
        _ -> respond (responseLBS status404 [] "")
    ["bootstrap-repo"] ->
      case Wai.requestMethod req of
        m | m == methodPost -> handleBootstrapRepoPost bootstrapLock repoStatusRef cfg req respond
        m | m == methodGet -> handleBootstrapRepoGet repoStatusRef cfg req respond
        _ -> respond (responseLBS status404 [] "")
    _ -> respond (responseLBS status404 [] "")
