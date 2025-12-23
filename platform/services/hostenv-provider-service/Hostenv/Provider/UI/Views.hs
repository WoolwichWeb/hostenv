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

import Hostenv.Provider.Config (AppConfig(..), uiPath)
import Hostenv.Provider.DB (ProjectRow(..), SessionInfo(..), User(..))
import Hostenv.Provider.Gitlab (GitlabProject(..))
import Hostenv.Provider.Util (escapeHtml)


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
    AppConfig { appGitlabHosts = hosts } = cfg
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
      , escapeHtml username
      , "</span>"
      , "<a class=\"btn subtle\" href=\""
      , uiPath cfg "/logout"
      , "\">Sign out</a></div></div>"
      , projectListHtml projects
      , "<div class=\"footer\"><a class=\"btn\" href=\""
      , uiPath cfg "/add-project"
      , "\">Add project from GitLab</a></div>"
      ]
  where
    SessionInfo { sessionUser = User { userUsername = username } } = sess

projectListHtml :: [ProjectRow] -> Text
projectListHtml projects =
  if null projects
    then "<p>No projects added yet.</p>"
    else
      let rows = T.concat (map renderProject (sortOn (\ProjectRow { projectFlakeInput = input } -> input) projects))
       in T.concat
            [ "<table><thead><tr><th>Input</th><th>Repo</th><th>Host</th><th>Webhook hash</th></tr></thead><tbody>"
            , rows
            , "</tbody></table>"
            ]
  where
    renderProject p =
      let ProjectRow
            { projectFlakeInput = flakeInput
            , projectRepoPath = repoPath
            , projectGitHost = gitHost
            , projectHash = mHash
            } = p
       in
      T.concat
        [ "<tr><td><code>"
        , escapeHtml flakeInput
        , "</code></td><td>"
        , escapeHtml repoPath
        , "</td><td>"
        , escapeHtml gitHost
        , "</td><td><code>"
        , escapeHtml (fromMaybe "" mHash)
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
      , escapeHtml csrfToken
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
    SessionInfo { sessionCsrf = csrfToken } = sess
    renderRepoOption repo =
      let GitlabProject { glProjectId = repoId, glProjectPath = repoPath } = repo
       in
      T.concat
        [ "<option value=\""
        , T.pack (show repoId)
        , "\">"
        , escapeHtml repoPath
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
