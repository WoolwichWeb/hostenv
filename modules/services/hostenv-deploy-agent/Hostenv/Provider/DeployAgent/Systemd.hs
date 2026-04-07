module Hostenv.Provider.DeployAgent.Systemd
  ( UserManagerBootstrap(..)
  , SystemdRuntime(..)
  , bootstrapUserManager
  , defaultWantedUnitsDir
  , mkSystemdRuntime
  , normalizeWantedUnits
  , verifyUnitsSpec
  ) where

import qualified Data.Aeson as A
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad ((>=>), forever, unless, when)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Hostenv.Provider.DeployAgent.Executor.Process (ProcessResult(..), ProcessSpec(..), runProcessIO)
import Hostenv.Provider.DeployAgent.Logging (Logger, logDebug, logInfo, logWarn)
import Hostenv.Provider.DeployAgent.UserSession (UserIdentity(..), UserSession(..))
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
import Text.Read (readMaybe)

data UserManagerBootstrap = UserManagerBootstrap
  { startCommand :: ProcessSpec
  , socketPaths :: [FilePath]
  , maxAttempts :: Int
  , retryDelaySeconds :: Int
  }
  deriving (Eq, Show)

data SystemdRuntime = SystemdRuntime
  { signalReady :: Text -> Text -> IO ()
  , noteActivity :: Text -> IO ()
  , withWatchdogSupport :: IO () -> IO ()
  }

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

mkSystemdRuntime :: Logger -> IO SystemdRuntime
mkSystemdRuntime logger = do
  notifySocket <- lookupEnv "NOTIFY_SOCKET"
  watchdogUsec <- parsePositiveInt =<< lookupEnv "WATCHDOG_USEC"
  readyRef <- newIORef False
  lastActivityRef <- newIORef (Nothing :: Maybe Int)
  let notificationsEnabled = maybe False (not . null) notifySocket
      watchdogIntervalMicros = (`div` 2) <$> watchdogUsec
      staleThresholdMicros = fmap (max 30000000 . (* 2)) watchdogIntervalMicros
      runNotify args =
        when notificationsEnabled do
          result <- runProcessIO notifySpec { arguments = args }
          logNotifyFailure logger args result
      noteActivity :: Text -> IO ()
      noteActivity reason = do
        nowMicros <- currentMicros
        writeIORef lastActivityRef (Just nowMicros)
        logDebug logger "systemd" "watchdog_activity_recorded"
          [ "reason" A..= reason
          ]
      signalReady sessionIdValue messageIdValue = do
        alreadyReady <- readIORef readyRef
        unless alreadyReady do
          noteActivity "service_ready"
          writeIORef readyRef True
          runNotify ["--ready", "--status=authenticated websocket session established"]
          logInfo logger "systemd" "service_ready"
            [ "session_id" A..= sessionIdValue
            , "message_id" A..= messageIdValue
            ]
      withWatchdogSupport action =
        case watchdogIntervalMicros of
          Nothing -> action
          Just intervalMicros -> do
            logInfo logger "systemd" "watchdog_enabled" ["interval_ms" A..= (intervalMicros `div` 1000)]
            withAsync (watchdogLoop logger readyRef lastActivityRef intervalMicros (maybe intervalMicros id staleThresholdMicros) runNotify) (const action)
  pure SystemdRuntime { signalReady, noteActivity, withWatchdogSupport }

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

notifySpec :: ProcessSpec
notifySpec =
  ProcessSpec
    { description = "systemd-notify"
    , executable = "systemd-notify"
    , arguments = []
    , environment = []
    , workingDirectory = Nothing
    , timeoutSeconds = Just 5
    }

logNotifyFailure :: Logger -> [String] -> ProcessResult -> IO ()
logNotifyFailure logger args result =
  unless (result.exitCode == 0) do
    logWarn logger "systemd" "notify_command_failed"
      [ "exit_code" A..= result.exitCode
      , "stderr_summary" A..= result.stderrText
      , "arguments" A..= args
      ]

watchdogLoop :: Logger -> IORef Bool -> IORef (Maybe Int) -> Int -> Int -> ([String] -> IO ()) -> IO ()
watchdogLoop logger readyRef lastActivityRef intervalMicros staleThresholdMicros runNotify =
  forever do
    threadDelay intervalMicros
    ready <- readIORef readyRef
    when ready do
      nowMicros <- currentMicros
      mLastActivity <- readIORef lastActivityRef
      case mLastActivity of
        Just lastActivityMicros
          | nowMicros - lastActivityMicros <= staleThresholdMicros ->
              runNotify ["WATCHDOG=1"]
          | otherwise ->
              logWarn logger "systemd" "watchdog_activity_stale"
                [ "stale_for_ms" A..= ((nowMicros - lastActivityMicros) `div` 1000)
                , "threshold_ms" A..= (staleThresholdMicros `div` 1000)
                ]
        Nothing ->
          logWarn logger "systemd" "watchdog_activity_missing"
            [ "threshold_ms" A..= (staleThresholdMicros `div` 1000)
            ]

currentMicros :: IO Int
currentMicros = floor . (* 1000000) <$> getPOSIXTime

parsePositiveInt :: Maybe String -> IO (Maybe Int)
parsePositiveInt = pure . (>>= readMaybe >=> positive)
  where
    positive value
      | value > 0 = Just value
      | otherwise = Nothing
