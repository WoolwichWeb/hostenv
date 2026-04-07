module Hostenv.Provider.DeployAgent.UserSession
  ( UserIdentity(..)
  , UserSession(..)
  , buildUserSession
  , isSafeUserValue
  , runUserSpec
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Hostenv.Provider.DeployAgent.Executor.Process (ProcessSpec(..))
import Hostenv.Provider.DeployAgent.Protocol (isSafeAtom)
import System.FilePath ((</>))

data UserIdentity = UserIdentity
  { name :: Text
  , uid :: Int
  , home :: FilePath
  }
  deriving (Eq, Show)

data UserSession = UserSession
  { identity :: UserIdentity
  , runtimeDir :: FilePath
  , configDir :: FilePath
  , cacheDir :: FilePath
  , dataDir :: FilePath
  , stateDir :: FilePath
  , dbusSessionBusAddress :: Text
  , profile :: FilePath
  , profileDir :: FilePath
  }
  deriving (Eq, Show)

buildUserSession :: UserIdentity -> UserSession
buildUserSession identity =
  let runtimeDir = "/run/user/" <> show identity.uid
      configDir = identity.home </> ".config"
      cacheDir = identity.home </> ".cache"
      dataDir = identity.home </> ".local" </> "share"
      stateDir = identity.home </> ".local" </> "state"
      profileDir = "/nix/var/nix/profiles/per-user" </> T.unpack identity.name
      profile = profileDir </> "profile"
   in UserSession
        { identity
        , runtimeDir
        , configDir
        , cacheDir
        , dataDir
        , stateDir
        , dbusSessionBusAddress = T.pack ("unix:path=" <> runtimeDir </> "bus")
        , profile
        , profileDir
        }

runUserSpec :: UserSession -> ProcessSpec -> ProcessSpec
runUserSpec session spec =
  ProcessSpec
    { description = spec.description
    , executable = "runuser"
    , arguments =
        [ "-u"
        , T.unpack session.identity.name
        , "--"
        , "env"
        , "HOME=" <> session.identity.home
        , "XDG_RUNTIME_DIR=" <> session.runtimeDir
        , "DBUS_SESSION_BUS_ADDRESS=" <> T.unpack session.dbusSessionBusAddress
        , "XDG_CONFIG_HOME=" <> session.configDir
        , "XDG_CACHE_HOME=" <> session.cacheDir
        , "XDG_DATA_HOME=" <> session.dataDir
        , "XDG_STATE_HOME=" <> session.stateDir
        ]
          <> map renderEnv spec.environment
          <> [spec.executable]
          <> spec.arguments
    , environment = []
    , workingDirectory = spec.workingDirectory
    , timeoutSeconds = spec.timeoutSeconds
    }

isSafeUserValue :: Text -> Bool
isSafeUserValue = isSafeAtom

renderEnv :: (String, String) -> String
renderEnv (key, value) = key <> "=" <> value
