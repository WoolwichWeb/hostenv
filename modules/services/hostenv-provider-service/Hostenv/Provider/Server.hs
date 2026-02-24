{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Hostenv.Provider.Server
  ( runServer
  , app
  ) where

import Data.IORef (IORef, newIORef)
import qualified Data.ByteString.Lazy as BL
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setBeforeMainLoop)
import Servant

import Hostenv.Provider.Config (AppConfig(..))
import Hostenv.Provider.Jobs (JobRuntime, startJobRuntime)
import Hostenv.Provider.Repo (RepoStatus, openUnixSocket)
import Hostenv.Provider.UI.Router (uiApp)
import Hostenv.Provider.Webhook (WebhookAccepted, webhookHandler)


type API =
  "webhook"
    :> Capture "hash" Text
    :> Header "X-Hub-Signature-256" Text
    :> Header "X-Gitlab-Token" Text
    :> ReqBody '[OctetStream] BL.ByteString
    :> PostAccepted '[JSON] WebhookAccepted
    :<|> Raw

api :: Proxy API
api = Proxy

runServer :: AppConfig -> RepoStatus -> IO ()
runServer cfg initialRepoStatus = do
  let AppConfig { appListenSocket = listenSocket } = cfg
  repoStatusRef <- newIORef initialRepoStatus
  jobRuntime <- startJobRuntime cfg
  sock <- openUnixSocket listenSocket
  let settings = setBeforeMainLoop (putStrLn "hostenv-provider-service: listening") defaultSettings
  runSettingsSocket settings sock (app jobRuntime repoStatusRef cfg)

app :: JobRuntime -> IORef RepoStatus -> AppConfig -> Application
app jobRuntime repoStatusRef cfg = serve api (server jobRuntime repoStatusRef cfg)

server :: JobRuntime -> IORef RepoStatus -> AppConfig -> Server API
server jobRuntime repoStatusRef cfg =
  webhookHandler jobRuntime repoStatusRef cfg :<|> Tagged (uiApp jobRuntime repoStatusRef cfg)
