module Hostenv.Provider.DeployAgent.Config
  ( AgentConfig(..)
  , defaultEventStderrMaxLines
  , loadConfig
  , readNodeToken
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

defaultEventStderrMaxLines :: Int
defaultEventStderrMaxLines = 25

data AgentConfig = AgentConfig
  { providerApiBaseUrl :: Text
  , nodeAuthTokenFile :: FilePath
  , nodeName :: Text
  , stateFile :: FilePath
  , actionTimeoutSeconds :: Int
  , reconnectSeconds :: Int
  , eventStderrMaxLines :: Int
  }
  deriving (Eq, Show, Generic)

data AgentConfigFile = AgentConfigFile
  { providerApiBaseUrl :: Text
  , nodeAuthTokenFile :: FilePath
  , nodeName :: Text
  , stateFile :: FilePath
  , actionTimeoutSeconds :: Maybe Int
  , reconnectSeconds :: Maybe Int
  , eventStderrMaxLines :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON AgentConfigFile

loadConfig :: FilePath -> IO AgentConfig
loadConfig path = do
  exists <- doesFileExist path
  if not exists
    then dieWith ("provider deploy agent config file not found: " <> path)
    else do
      bytes <- BL.readFile path
      raw <-
        case A.eitherDecode' bytes of
          Left err -> dieWith ("failed to parse provider deploy agent config file " <> path <> ": " <> err)
          Right cfg -> pure cfg
      validateConfig raw

readNodeToken :: AgentConfig -> IO Text
readNodeToken cfg = do
  exists <- doesFileExist cfg.nodeAuthTokenFile
  if not exists
    then dieWith ("provider deploy agent token file missing: " <> cfg.nodeAuthTokenFile)
    else do
      token <- T.strip <$> T.readFile cfg.nodeAuthTokenFile
      if token == ""
        then dieWith ("provider deploy agent token file is empty: " <> cfg.nodeAuthTokenFile)
        else pure token

validateConfig :: AgentConfigFile -> IO AgentConfig
validateConfig raw = do
  let cfg =
        AgentConfig
          { providerApiBaseUrl = T.strip raw.providerApiBaseUrl
          , nodeAuthTokenFile = raw.nodeAuthTokenFile
          , nodeName = T.strip raw.nodeName
          , stateFile = raw.stateFile
          , actionTimeoutSeconds = max 1 (maybe 1800 id raw.actionTimeoutSeconds)
          , reconnectSeconds = max 1 (maybe 5 id raw.reconnectSeconds)
          , eventStderrMaxLines = max 1 (maybe defaultEventStderrMaxLines id raw.eventStderrMaxLines)
          }
  if cfg.providerApiBaseUrl == ""
    then dieWith "providerApiBaseUrl must be configured"
    else
      if cfg.nodeName == ""
        then dieWith "nodeName must be configured"
        else pure cfg

dieWith :: String -> IO a
dieWith msg = do
  hPutStrLn stderr msg
  exitFailure
