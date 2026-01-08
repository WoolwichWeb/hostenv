{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.UI.Views
  ( loginPage
  , accessDeniedPage
  , indexPage
  , addProjectPage
  , successPage
  , errorPage
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid

import Hostenv.Provider.Config (AppConfig(..), uiPath)
import Hostenv.Provider.DB (ProjectRow(..), SessionInfo(..), User(..))
import Hostenv.Provider.Gitlab (GitlabProject(..))


loginPage :: AppConfig -> Maybe Text -> Html ()
loginPage cfg mMsg =
  page cfg "Hostenv Provider" $ do
    maybe mempty alertBox mMsg
    h1_ "Hostenv Provider"
    p_ "Sign in with GitLab to manage projects."
    loginButtons
  where
    AppConfig { appGitlabHosts = hosts } = cfg
    loginButtons :: Html ()
    loginButtons =
      if length hosts <= 1
        then a_ [class_ "btn", href_ (uiPath cfg "/oauth/gitlab/start")] "Sign in with GitLab"
        else mconcat (map renderHost hosts)
    renderHost :: Text -> Html ()
    renderHost host =
      a_ [class_ "btn", href_ (uiPath cfg ("/oauth/gitlab/start?host=" <> host))] $ do
        toHtml ("Sign in with " :: Text)
        toHtml host

accessDeniedPage :: AppConfig -> Html ()
accessDeniedPage cfg =
  page cfg "Access denied" $ do
    h1_ "Access denied"
    p_ "This account does not have the admin role."
    a_ [class_ "btn subtle", href_ (uiPath cfg "/logout")] "Sign out"

indexPage :: AppConfig -> SessionInfo -> [ProjectRow] -> Html ()
indexPage cfg sess projects =
  page cfg "Projects" $ do
    div_ [class_ "header"] $ do
      h1_ "Projects"
      div_ [class_ "actions"] $ do
        span_ [class_ "user"] (toHtml username)
        a_ [class_ "btn subtle", href_ (uiPath cfg "/logout")] "Sign out"
    projectListHtml projects
    div_ [class_ "footer"] $
      a_ [class_ "btn", href_ (uiPath cfg "/add-project")] "Add project from GitLab"
  where
    SessionInfo { sessionUser = User { userUsername = username } } = sess

projectListHtml :: [ProjectRow] -> Html ()
projectListHtml projects =
  if null projects
    then p_ "No projects added yet."
    else
      let rows = mconcat (map renderProject (sortOn (\ProjectRow { projectFlakeInput = input } -> input) projects))
       in table_ $ do
            thead_ $
              tr_ $ do
                th_ "Input"
                th_ "Repo"
                th_ "Host"
                th_ "Webhook hash"
            tbody_ rows
  where
    renderProject :: ProjectRow -> Html ()
    renderProject p =
      let ProjectRow
            { projectFlakeInput = flakeInput
            , projectRepoPath = repoPath
            , projectGitHost = gitHost
            , projectHash = mHash
            } = p
       in tr_ $ do
            td_ $ code_ (toHtml flakeInput)
            td_ (toHtml repoPath)
            td_ (toHtml gitHost)
            td_ $ code_ (toHtml (fromMaybe "" mHash))

addProjectPage :: AppConfig -> SessionInfo -> [GitlabProject] -> Html ()
addProjectPage cfg sess repos =
  page cfg "Add project" $ do
    div_ [class_ "header"] $ do
      h1_ "Add project"
      div_ [class_ "actions"] $
        a_ [class_ "btn subtle", href_ (uiPath cfg "/")] "Back"
    form_ [method_ "post", class_ "card"] $ do
      input_ [type_ "hidden", name_ "csrf", value_ csrfToken]
      label_ "Repository"
      select_ [name_ "repo_id"] (mconcat (map renderRepoOption repos))
      label_ "Organisation"
      input_ [type_ "text", name_ "org", placeholder_ "org"]
      label_ "Project"
      input_ [type_ "text", name_ "project", placeholder_ "project"]
      button_ [class_ "btn", type_ "submit"] "Add project"
  where
    SessionInfo { sessionCsrf = csrfToken } = sess
    renderRepoOption :: GitlabProject -> Html ()
    renderRepoOption repo =
      let GitlabProject { glProjectId = repoId, glProjectPath = repoPath } = repo
       in option_ [value_ (T.pack (show repoId))] (toHtml repoPath)

successPage :: AppConfig -> Text -> Html ()
successPage cfg msg =
  page cfg "Success" $ do
    h1_ "Project added"
    p_ (toHtml msg)
    a_ [class_ "btn", href_ (uiPath cfg "/")] "Back to projects"

errorPage :: AppConfig -> Text -> Html ()
errorPage cfg msg =
  page cfg "Error" $ do
    alertBox msg
    a_ [class_ "btn subtle", href_ (uiPath cfg "/")] "Back"

alertBox :: Text -> Html ()
alertBox msg =
  div_ [class_ "alert"] (toHtml msg)

page :: AppConfig -> Text -> Html () -> Html ()
page _cfg title body =
  doctypehtml_ $
    html_ $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width,initial-scale=1"]
        title_ (toHtml title)
        style_ pageStyles
      body_ $
        main_ $
          div_ [class_ "shell"] body

pageStyles :: Text
pageStyles =
  T.concat
    [ "@font-face{font-family:ui;src:local('IBM Plex Sans'),local('Space Grotesk'),local('Avenir Next'),local('Segoe UI');}"
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
    ]
