{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.UI.Views
  ( loginPage
  , accessDeniedPage
  , indexPage
  , addProjectPage
  , bootstrapRepoPage
  , jobPage
  , successPage
  , errorPage
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid

import Hostenv.Provider.Config (AppConfig(..), uiPath)
import Hostenv.Provider.Jobs (JobSummary(..))
import Hostenv.Provider.Repo (RepoStatus(..))
import Hostenv.Provider.DB (ProjectRow(..), SessionInfo(..), User(..))
import Hostenv.Provider.Gitlab (GitlabProject(..))


loginPage :: AppConfig -> Maybe Text -> Html ()
loginPage cfg mMsg =
  page cfg "Hostenv Provider" $ do
    maybe mempty alertBox mMsg
    h1_ "Hostenv Provider"
    case cfg.appGitlabSecrets of
      Nothing -> p_ "GitLab OAuth is not configured for this provider."
      Just _ -> do
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

indexPage :: AppConfig -> SessionInfo -> RepoStatus -> [ProjectRow] -> [JobSummary] -> Html ()
indexPage cfg sess repoStatus projects jobs =
  page cfg "Projects" $ do
    div_ [class_ "header"] $ do
      h1_ "Projects"
      div_ [class_ "actions"] $ do
        span_ [class_ "user"] (toHtml username)
        a_ [class_ "btn subtle", href_ (uiPath cfg "/logout")] "Sign out"
    case repoStatus of
      RepoMissing -> do
        p_ "Provider repository is not initialized yet."
        p_ "Bootstrap it from GitLab before adding projects."
        div_ [class_ "footer"] $
          a_ [class_ "btn", href_ (uiPath cfg "/bootstrap-repo")] "Bootstrap provider repository"
      RepoReady -> do
        projectListHtml projects
        jobsListHtml cfg jobs
        div_ [class_ "footer"] $
          a_ [class_ "btn", href_ (uiPath cfg "/add-project")] "Add project from GitLab"
  where
    SessionInfo { user = User { username = username } } = sess

projectListHtml :: [ProjectRow] -> Html ()
projectListHtml projects =
  if null projects
    then p_ "No projects added yet."
    else
      let rows = mconcat (map renderProject (sortOn (\ProjectRow { flakeInput = input } -> input) projects))
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
            { flakeInput = flakeInput
            , repoPath = repoPath
            , gitHost = gitHost
            , hash = mHash
            } = p
       in tr_ $ do
            td_ $ code_ (toHtml flakeInput)
            td_ (toHtml repoPath)
            td_ (toHtml gitHost)
            td_ $ code_ (toHtml (fromMaybe "" mHash))

jobsListHtml :: AppConfig -> [JobSummary] -> Html ()
jobsListHtml cfg jobs = do
  h2_ "Recent jobs"
  if null jobs
    then p_ "No jobs yet."
    else
      let rows = mconcat (map renderJob jobs)
       in table_ $ do
            thead_ $
              tr_ $ do
                th_ "Job"
                th_ "Kind"
                th_ "Status"
                th_ "Created"
            tbody_ rows
  where
    renderJob :: JobSummary -> Html ()
    renderJob job =
      tr_ $ do
        td_ $
          a_
            [ href_ (uiPath cfg ("/jobs/" <> job.id))
            ]
            (code_ (toHtml job.id))
        td_ (toHtml job.kind)
        td_ (toHtml job.status)
        td_ (toHtml (T.pack (show job.createdAt)))

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
    SessionInfo { csrf = csrfToken } = sess
    renderRepoOption :: GitlabProject -> Html ()
    renderRepoOption repo =
      let repoId = repo.id
          repoPath = repo.path
       in option_ [value_ (T.pack (show repoId))] (toHtml repoPath)

bootstrapRepoPage :: AppConfig -> SessionInfo -> [GitlabProject] -> Html ()
bootstrapRepoPage cfg sess repos =
  page cfg "Bootstrap repository" $ do
    div_ [class_ "header"] $ do
      h1_ "Bootstrap provider repository"
      div_ [class_ "actions"] $
        a_ [class_ "btn subtle", href_ (uiPath cfg "/")] "Back"
    p_ "Select the GitLab repository that should be cloned into this provider environment."
    form_ [method_ "post", class_ "card"] $ do
      input_ [type_ "hidden", name_ "csrf", value_ csrfToken]
      label_ "Repository"
      select_ [name_ "repo_id"] (mconcat (map renderRepoOption repos))
      button_ [class_ "btn", type_ "submit"] "Clone repository"
  where
    SessionInfo { csrf = csrfToken } = sess
    renderRepoOption :: GitlabProject -> Html ()
    renderRepoOption repo =
      let repoId = repo.id
          repoPath = repo.path
       in option_ [value_ (T.pack (show repoId))] (toHtml repoPath)

jobPage :: AppConfig -> SessionInfo -> JobSummary -> Html ()
jobPage cfg _sess job =
  page cfg ("Job " <> job.id) $ do
    div_ [class_ "header"] $ do
      h1_ "Job"
      div_ [class_ "actions"] $
        a_ [class_ "btn subtle", href_ (uiPath cfg "/")] "Back"
    p_ $ do
      "Job id: "
      code_ (toHtml job.id)
    p_ $ do
      "Kind: "
      code_ (toHtml job.kind)
    p_ $ do
      "Status: "
      span_ [id_ "job-status"] (toHtml job.status)
    div_ [class_ "card"] $ do
      label_ "Node deploy status"
      table_ [id_ "deploy-status-table"] $ do
        thead_ $
          tr_ $ do
            th_ "Node"
            th_ "Status"
            th_ "Phase"
            th_ "Message"
        tbody_ [id_ "deploy-status-body"] mempty
    div_ [class_ "card"] $ do
      label_ "Deploy actions"
      table_ [id_ "deploy-actions-table"] $ do
        thead_ $
          tr_ $ do
            th_ "Node"
            th_ "#"
            th_ "User"
            th_ "Op"
            th_ "Status"
            th_ "Message"
        tbody_ [id_ "deploy-actions-body"] mempty
    div_ [class_ "card"] $ do
      label_ "Logs"
      pre_ [id_ "job-log", style_ "min-height:320px;max-height:60vh;overflow:auto;background:#f7f7f4;padding:14px;border-radius:10px;"] ""
    script_ [type_ "application/javascript"] (jobPageScript job.id)

-- | Horrible, hacky way to add a script to the page.
-- | @todo: replace janky frontend with Miso.
jobPageScript :: Text -> Text
jobPageScript jobId =
  T.unlines
    [ "(function(){"
    , "  const logEl = document.getElementById('job-log');"
    , "  const statusEl = document.getElementById('job-status');"
    , "  const deployStatusBody = document.getElementById('deploy-status-body');"
    , "  const deployActionsBody = document.getElementById('deploy-actions-body');"
    , "  let lastSeq = 0;"
    , "  let lastDeployEventId = 0;"
    , "  function append(line) {"
    , "    if (!line) return;"
    , "    logEl.textContent += line + '\\n';"
    , "    logEl.scrollTop = logEl.scrollHeight;"
    , "  }"
    , "  function applyEvent(ev) {"
    , "    if (!ev) return;"
    , "    if (typeof ev.seq === 'number' && ev.seq > lastSeq) { lastSeq = ev.seq; }"
    , "    if (typeof ev.line === 'string') { append('[' + ev.stream + '] ' + ev.line); }"
    , "  }"
    , "  function applyMessage(msg) {"
    , "    if (!msg || !msg.type) return;"
    , "    if (msg.type === 'job_event') { applyEvent(msg); }"
    , "    if (msg.type === 'job_status') {"
    , "      if (msg.status) statusEl.textContent = msg.status;"
    , "      if (msg.error) append('[error] ' + msg.error);"
    , "    }"
    , "    if (msg.type === 'deploy_event' && msg.event) {"
    , "      applyDeployEvent(msg.event);"
    , "    }"
    , "    if (msg.type === 'deploy_status') {"
    , "      renderDeployStatus(msg.statuses || []);"
    , "    }"
    , "    if (msg.type === 'deploy_actions') {"
    , "      renderDeployActions(msg.actions || []);"
    , "    }"
    , "  }"
    , "  function applyDeployEvent(ev) {"
    , "    if (!ev) return;"
    , "    if (typeof ev.id === 'number' && ev.id > lastDeployEventId) { lastDeployEventId = ev.id; }"
    , "    const node = typeof ev.node === 'string' ? ev.node : 'unknown';"
    , "    const status = typeof ev.status === 'string' ? ev.status : 'running';"
    , "    const phase = typeof ev.phase === 'string' ? ev.phase : '';"
    , "    const message = typeof ev.message === 'string' ? ev.message : '';"
    , "    const phaseSuffix = phase ? ':' + phase : '';"
    , "    append('[node:' + node + '] [' + status + phaseSuffix + '] ' + message);"
    , "  }"
    , "  function esc(v) {"
    , "    return String(v || '').replaceAll('&','&amp;').replaceAll('<','&lt;').replaceAll('>','&gt;');"
    , "  }"
    , "  function renderDeployStatus(items) {"
    , "    if (!deployStatusBody) return;"
    , "    const normalized = (items || []).map((item) => {"
    , "      const status = String(item && item.status ? item.status : '').toLowerCase();"
    , "      return { ...item, _status: status };"
    , "    });"
    , "    const hasFailure = normalized.some((item) => item._status === 'failed' || item._status === 'timed_out');"
    , "    if (hasFailure && statusEl && statusEl.textContent !== 'succeeded') {"
    , "      statusEl.textContent = 'failed';"
    , "    }"
    , "    const rows = normalized.map((item) => {"
    , "      const node = esc(item.node || '');"
    , "      const status = esc(item.status || '');"
    , "      const statusClass = item._status ? ('status-' + esc(item._status)) : '';"
    , "      const phase = esc(item.phase || '');"
    , "      const message = esc(item.message || '');"
    , "      return '<tr><td><code>' + node + '</code></td><td class=\"' + statusClass + '\">' + status + '</td><td>' + phase + '</td><td>' + message + '</td></tr>';"
    , "    }).join('');"
    , "    deployStatusBody.innerHTML = rows || '<tr><td colspan=\"4\">No node events yet.</td></tr>';"
    , "  }"
    , "  function renderDeployActions(items) {"
    , "    if (!deployActionsBody) return;"
    , "    const normalized = (items || []).map((item) => {"
    , "      const status = String(item && item.status ? item.status : '').toLowerCase();"
    , "      return { ...item, _status: status };"
    , "    });"
    , "    const rows = normalized.map((item) => {"
    , "      const node = esc(item.node || '');"
    , "      const idx = esc(item.actionIndex);"
    , "      const user = esc(item.user || '');"
    , "      const op = esc(item.op || '');"
    , "      const status = esc(item.status || '');"
    , "      const statusClass = item._status ? ('status-' + esc(item._status)) : '';"
    , "      const message = esc(item.message || '');"
    , "      return '<tr><td><code>' + node + '</code></td><td><code>' + idx + '</code></td><td><code>' + user + '</code></td><td><code>' + op + '</code></td><td class=\"' + statusClass + '\">' + status + '</td><td>' + message + '</td></tr>';"
    , "    }).join('');"
    , "    deployActionsBody.innerHTML = rows || '<tr><td colspan=\"6\">No deploy actions yet.</td></tr>';"
    , "  }"
    , "  function fetchDeployStatus() {"
    , "    return fetch(window.location.pathname + '/deploy-status')"
    , "      .then(r => r.json())"
    , "      .then(renderDeployStatus);"
    , "  }"
    , "  function fetchDeployActions() {"
    , "    return fetch(window.location.pathname + '/deploy-actions')"
    , "      .then(r => r.json())"
    , "      .then(renderDeployActions);"
    , "  }"
    , "  function fetchDeployEvents() {"
    , "    return fetch(window.location.pathname + '/deploy-events?after=' + String(lastDeployEventId))"
    , "      .then(r => r.json())"
    , "      .then(items => { (items || []).forEach(applyDeployEvent); });"
    , "  }"
    , "  fetch(window.location.pathname + '/events?after=0')"
    , "    .then(r => r.json())"
    , "    .then(items => {"
    , "      (items || []).forEach(applyEvent);"
    , "      return fetchDeployStatus();"
    , "    })"
    , "    .then(() => {"
    , "      return fetchDeployEvents();"
    , "    })"
    , "    .then(() => {"
    , "      return fetchDeployActions();"
    , "    })"
    , "    .then(() => {"
    , "      setInterval(() => {"
    , "        fetchDeployStatus().catch(err => append('[error] failed to load deploy status: ' + String(err)));"
    , "        fetchDeployEvents().catch(err => append('[error] failed to load deploy events: ' + String(err)));"
    , "        fetchDeployActions().catch(err => append('[error] failed to load deploy actions: ' + String(err)));"
    , "      }, 2000);"
    , "      const proto = window.location.protocol === 'https:' ? 'wss://' : 'ws://';"
    , "      const ws = new WebSocket(proto + window.location.host + window.location.pathname + '/ws');"
    , "      ws.onmessage = (event) => {"
    , "        try { applyMessage(JSON.parse(event.data)); } catch (_) {}"
    , "      };"
    , "    })"
    , "    .catch(err => append('[error] failed to load logs: ' + String(err)));"
    , "})();"
    ]

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
    , ".status-queued,.status-waiting,.status-running{color:#8a5a00;font-weight:600;}"
    , ".status-success,.status-succeeded,.status-skipped{color:#0f6b2a;font-weight:600;}"
    , ".status-failed,.status-timed_out{color:#9b1c1c;font-weight:700;}"
    , ".footer{margin-top:24px;}"
    , ".alert{background:#fce8e8;color:#9b1c1c;padding:10px 12px;border-radius:10px;margin-bottom:16px;}"
    , "code{background:#f4f4f0;padding:2px 6px;border-radius:6px;}"
    ]
