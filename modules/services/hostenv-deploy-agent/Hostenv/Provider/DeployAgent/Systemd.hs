module Hostenv.Provider.DeployAgent.Systemd
  ( UserManagerBootstrap(..)
  , bootstrapUserManager
  , defaultWantedUnitsDir
  , normalizeWantedUnits
  , verifyUnitsSpec
  ) where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Hostenv.Provider.DeployAgent.Executor.Process (ProcessSpec(..))
import Hostenv.Provider.DeployAgent.UserSession (UserIdentity(..), UserSession(..))
import System.FilePath ((</>), takeFileName)

data UserManagerBootstrap = UserManagerBootstrap
  { startCommand :: ProcessSpec
  , socketPaths :: [FilePath]
  , maxAttempts :: Int
  , retryDelaySeconds :: Int
  }
  deriving (Eq, Show)

bootstrapUserManager :: UserSession -> UserManagerBootstrap
bootstrapUserManager session =
  let UserSession { identity = UserIdentity { uid = uidValue }, runtimeDir = runtimeDirValue } = session
   in UserManagerBootstrap
     { startCommand =
        ProcessSpec
          { description = "start-user-manager"
          , executable = "systemctl"
          , arguments = ["start", "user@" <> show uidValue <> ".service"]
          , environment = []
          , workingDirectory = Nothing
          , timeoutSeconds = Nothing
          }
     , socketPaths =
        [ runtimeDirValue </> "systemd" </> "private"
        , runtimeDirValue </> "bus"
        ]
    , maxAttempts = 30
    , retryDelaySeconds = 1
    }

defaultWantedUnitsDir :: FilePath -> FilePath
defaultWantedUnitsDir storePath = storePath </> "systemd" </> "user" </> "default.target.wants"

normalizeWantedUnits :: [FilePath] -> [Text]
normalizeWantedUnits = map (T.pack . takeFileName) . sort

verifyUnitsSpec :: UserSession -> [Text] -> ProcessSpec
verifyUnitsSpec _session units =
  let startLine =
        if null units
          then "true"
          else "systemctl --user start " <> unwords (map shellQuote (map T.unpack units))
      checkLines =
        concat
           [ [ "load_state=$(systemctl --user show -p LoadState --value " <> shellQuote (T.unpack unit) <> ")"
             , "if [ \"$load_state\" != \"loaded\" ]; then exit 1; fi"
             ]
           | unit <- units
           ]
    in ProcessSpec
         { description = "verify-user-units"
         , executable = "bash"
         , arguments =
             [ "-lc"
             , unlines
                 ( [ "set -euo pipefail"
                   , "if [ ! -L \"$XDG_CONFIG_HOME/systemd\" ]; then exit 1; fi"
                   , "if [ ! -d \"$XDG_CONFIG_HOME/systemd/user\" ]; then exit 1; fi"
                   , "if [ ! -L \"$XDG_STATE_HOME/hostenv/current-state/systemd\" ]; then exit 1; fi"
                   , "systemctl --user daemon-reload"
                   , "systemctl --user reset-failed || true"
                   , startLine
                   ]
                     <> checkLines
                 )
             ]
         , environment = []
         , workingDirectory = Nothing
         , timeoutSeconds = Nothing
         }

shellQuote :: String -> String
shellQuote raw = "'" <> concatMap escape raw <> "'"
  where
    escape '\'' = "'\\''"
    escape c = [c]
