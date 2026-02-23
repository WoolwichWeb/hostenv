{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Hostenv.Provider.Server
  ( runServer
  , app
  ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Data.IORef (IORef, newIORef)
import qualified Data.ByteString.Lazy as BL
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setBeforeMainLoop)
import Servant

import Hostenv.Provider.Config (AppConfig(..))
import Hostenv.Provider.Repo (RepoStatus, openUnixSocket)
import Hostenv.Provider.UI.Router (uiApp)
import Hostenv.Provider.Service (WebhookResult)
import Hostenv.Provider.Webhook (webhookHandler)


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

runServer :: AppConfig -> RepoStatus -> IO ()
runServer cfg initialRepoStatus = do
  let AppConfig { appListenSocket = listenSocket } = cfg
  webhookLock <- newMVar ()
  bootstrapLock <- newMVar ()
  repoStatusRef <- newIORef initialRepoStatus
  sock <- openUnixSocket listenSocket
  let settings = setBeforeMainLoop (putStrLn "hostenv-provider-service: listening") defaultSettings
  runSettingsSocket settings sock (app webhookLock bootstrapLock repoStatusRef cfg)

app :: MVar () -> MVar () -> IORef RepoStatus -> AppConfig -> Application
app webhookLock bootstrapLock repoStatusRef cfg = serve api (server webhookLock bootstrapLock repoStatusRef cfg)

server :: MVar () -> MVar () -> IORef RepoStatus -> AppConfig -> Server API
server webhookLock bootstrapLock repoStatusRef cfg =
  webhookHandler webhookLock repoStatusRef cfg :<|> Tagged (uiApp repoStatusRef bootstrapLock cfg)
