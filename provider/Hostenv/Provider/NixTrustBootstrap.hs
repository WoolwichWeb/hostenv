module Hostenv.Provider.NixTrustBootstrap
    ( NixTrustBootstrapConfig (..)
    , ensureNodeNixTrust
    ) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))

data NixTrustBootstrapConfig = NixTrustBootstrapConfig
    { nodeName :: Text
    , runRemote :: [Text] -> IO (ExitCode, Text, Text)
    , logLine :: Text -> IO ()
    }

data BootstrapResult
    = TrustAlreadyConfigured
    | TrustPatched {backupPath :: Text}

data BootstrapFailure
    = FailNoRollback {reason :: Text}
    | FailWithRollback {backupPath :: Text, reason :: Text}

ensureNodeNixTrust :: NixTrustBootstrapConfig -> IO ExitCode
ensureNodeNixTrust cfg = do
    cfg.logLine ("hostenv-provider: checking nix trust bootstrap on node " <> cfg.nodeName)
    result <- runExceptT (bootstrap cfg)
    case result of
        Right TrustAlreadyConfigured -> do
            cfg.logLine ("hostenv-provider: nix trust already configured on node " <> cfg.nodeName)
            pure ExitSuccess
        Right (TrustPatched backupPath) -> do
            cfg.logLine ("hostenv-provider: patched nix trust on node " <> cfg.nodeName)
            cfg.logLine ("hostenv-provider: node " <> cfg.nodeName <> " backup written to " <> backupPath)
            pure ExitSuccess
        Left (FailNoRollback reason) ->
            failNoRollback cfg reason
        Left (FailWithRollback backupPath reason) ->
            failWithRollback cfg backupPath reason

bootstrap :: NixTrustBootstrapConfig -> ExceptT BootstrapFailure IO BootstrapResult
bootstrap cfg = do
    confText <- readNixConf cfg
    if hasDeployTrustedUser confText
        then pure TrustAlreadyConfigured
        else do
            let backupPath = "/etc/nix/nix.conf.hostenv-provider.bak"
            runCommandNoRollback cfg ["sudo", "cp", "/etc/nix/nix.conf", backupPath]
            runCommandNoRollback cfg ["sudo", "chmod", "0600", backupPath]

            let patchedConf = patchTrustedUsersConfig confText
            runActionWithRollback backupPath ["sudo", "tee", "/etc/nix/nix.conf"] (writeRemoteFileAsRoot cfg "/etc/nix/nix.conf" patchedConf)
            runActionWithRollback backupPath ["sudo", "systemctl", "restart", "nix-daemon(.service)"] (restartNixDaemon cfg)

            trusted <- lift (checkDeployTrustedEffective cfg)
            if trusted
                then pure (TrustPatched backupPath)
                else throwE (FailWithRollback backupPath "deploy not present in trusted-users after patch")

failNoRollback :: NixTrustBootstrapConfig -> Text -> IO ExitCode
failNoRollback cfg reason = do
    cfg.logLine ("hostenv-provider: nix trust bootstrap failed on node " <> cfg.nodeName)
    when (not (T.null reason)) $
        cfg.logLine ("hostenv-provider: bootstrap reason for " <> cfg.nodeName <> ": " <> reason)
    cfg.logLine (remediationMessage cfg.nodeName)
    pure (ExitFailure 1)

failWithRollback :: NixTrustBootstrapConfig -> Text -> Text -> IO ExitCode
failWithRollback cfg backupPath reason = do
    rollbackOk <- rollbackNixConf cfg backupPath
    cfg.logLine ("hostenv-provider: nix trust bootstrap failed on node " <> cfg.nodeName)
    cfg.logLine ("hostenv-provider: attempted rollback using " <> backupPath)
    when (not (T.null reason)) $
        cfg.logLine ("hostenv-provider: bootstrap reason for " <> cfg.nodeName <> ": " <> reason)
    when (not rollbackOk) $
        cfg.logLine ("hostenv-provider: warning: rollback may be incomplete on node " <> cfg.nodeName)
    cfg.logLine (remediationMessage cfg.nodeName)
    pure (ExitFailure 1)

remediationMessage :: Text -> Text
remediationMessage nodeName =
    "hostenv-provider: remediation for "
        <> nodeName
        <> ": run `sudo systemctl status nix-daemon`, `sudo nix config show trusted-users`, and inspect /etc/nix/nix.conf plus any backup file."

readNixConf :: NixTrustBootstrapConfig -> ExceptT BootstrapFailure IO Text
readNixConf cfg = do
    (readCode, confText, readErr) <- lift (cfg.runRemote ["sudo", "cat", "/etc/nix/nix.conf"])
    case readCode of
        ExitSuccess -> pure confText
        ExitFailure _ ->
            throwE (FailNoRollback (commandFailure ["sudo", "cat", "/etc/nix/nix.conf"] readCode confText readErr))

runCommandNoRollback :: NixTrustBootstrapConfig -> [Text] -> ExceptT BootstrapFailure IO ()
runCommandNoRollback cfg args =
    runActionNoRollback args (cfg.runRemote args)

runActionNoRollback :: [Text] -> IO (ExitCode, Text, Text) -> ExceptT BootstrapFailure IO ()
runActionNoRollback args action = do
    (code, out, err) <- lift action
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> throwE (FailNoRollback (commandFailure args code out err))

runActionWithRollback :: Text -> [Text] -> IO (ExitCode, Text, Text) -> ExceptT BootstrapFailure IO ()
runActionWithRollback backupPath args action = do
    (code, out, err) <- lift action
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> throwE (FailWithRollback backupPath (commandFailure args code out err))

showCmd :: [Text] -> Text
showCmd = T.unwords . map shellEscape

trimmedOutputFallback :: Text -> Text -> Text
trimmedOutputFallback out err =
    let out' = T.strip out
        err' = T.strip err
     in if not (T.null err')
            then err'
            else out'

commandFailure :: [Text] -> ExitCode -> Text -> Text -> Text
commandFailure args code out err =
    let detail = trimmedOutputFallback out err
     in "command failed: "
            <> showCmd args
            <> " (exit "
            <> T.pack (show code)
            <> ")"
            <> if T.null detail then "" else " - " <> detail

shellEscape :: Text -> Text
shellEscape t =
    if T.null t
        then "''"
        else "'" <> T.replace "'" "'\\''" t <> "'"

pickTag :: Text -> Text -> Text
pickTag base body =
    let go n =
            let tag =
                    if n == 0
                        then base
                        else base <> "_" <> T.pack (show n)
             in if T.isInfixOf tag body then go (n + 1) else tag
     in go 0

isTrustedUsersLine :: Text -> Bool
isTrustedUsersLine line =
    let stripped = T.stripStart line
        (lhs, rhs) = T.breakOn "=" stripped
     in (T.strip lhs == "trusted-users") && (not (T.null rhs))

parseTrustedUsers :: Text -> [Text]
parseTrustedUsers line =
    let stripped = T.stripStart line
        rhs = T.drop 1 (snd (T.breakOn "=" stripped))
        rhsNoComment = T.takeWhile (/= '#') rhs
     in filter (not . T.null) (T.words rhsNoComment)

hasDeployTrustedUser :: Text -> Bool
hasDeployTrustedUser conf =
    any
        (elem "deploy" . parseTrustedUsers)
        (filter isTrustedUsersLine (T.lines conf))

dedupeKeepOrder :: [Text] -> [Text]
dedupeKeepOrder =
    reverse . fst . foldl step ([], S.empty)
  where
    step (acc, seen) value =
        if S.member value seen
            then (acc, seen)
            else (value : acc, S.insert value seen)

patchTrustedUsersConfig :: Text -> Text
patchTrustedUsersConfig conf =
    let rewrite [] = ([], False)
        rewrite (line : rest)
            | isTrustedUsersLine line =
                let users = dedupeKeepOrder (parseTrustedUsers line <> ["deploy"])
                 in ("trusted-users = " <> T.unwords users : rest, True)
            | otherwise =
                let (rest', touched) = rewrite rest
                 in (line : rest', touched)
        (linesPatched, touchedLine) = rewrite (T.lines conf)
        finalLines =
            if touchedLine
                then linesPatched
                else linesPatched <> ["trusted-users = root deploy"]
     in T.unlines finalLines

restartNixDaemon :: NixTrustBootstrapConfig -> IO (ExitCode, Text, Text)
restartNixDaemon cfg = do
    let firstCmd = ["sudo", "systemctl", "restart", "nix-daemon.service"]
    (firstCode, firstOut, firstErr) <- cfg.runRemote firstCmd
    case firstCode of
        ExitSuccess -> pure (ExitSuccess, firstOut, firstErr)
        ExitFailure _ -> do
            let secondCmd = ["sudo", "systemctl", "restart", "nix-daemon"]
            (secondCode, secondOut, secondErr) <- cfg.runRemote secondCmd
            case secondCode of
                ExitSuccess -> pure (ExitSuccess, secondOut, secondErr)
                ExitFailure _ ->
                    pure
                        ( secondCode
                        , secondOut
                        , T.unlines
                            [ "failed both restart commands:"
                            , commandFailure firstCmd firstCode firstOut firstErr
                            , commandFailure secondCmd secondCode secondOut secondErr
                            ]
                        )

checkDeployTrustedEffective :: NixTrustBootstrapConfig -> IO Bool
checkDeployTrustedEffective cfg = do
    (cfgCode, cfgOut, _cfgErr) <- cfg.runRemote ["sudo", "nix", "config", "show", "trusted-users"]
    case cfgCode of
        ExitSuccess -> pure ("deploy" `elem` T.words cfgOut)
        ExitFailure _ -> do
            (readCode, confText, _readErr) <- cfg.runRemote ["sudo", "cat", "/etc/nix/nix.conf"]
            pure (readCode == ExitSuccess && hasDeployTrustedUser confText)

writeRemoteFileAsRoot :: NixTrustBootstrapConfig -> Text -> Text -> IO (ExitCode, Text, Text)
writeRemoteFileAsRoot cfg path content = do
    let payloadTag = pickTag "HOSTENV_NIXCONF" content
    let writeCmd =
            T.unlines
                [ "sudo tee " <> shellEscape path <> " >/dev/null <<'" <> payloadTag <> "'"
                , content
                , payloadTag
                , "sudo chmod 0644 " <> shellEscape path
                ]
    cfg.runRemote ["bash", "-lc", writeCmd]

rollbackNixConf :: NixTrustBootstrapConfig -> Text -> IO Bool
rollbackNixConf cfg backupPath = do
    (restoreCode, _restoreOut, _restoreErr) <- cfg.runRemote ["sudo", "cp", backupPath, "/etc/nix/nix.conf"]
    (chmodCode, _chmodOut, _chmodErr) <- cfg.runRemote ["sudo", "chmod", "0644", "/etc/nix/nix.conf"]
    (restartCode, _restartOut, _restartErr) <- restartNixDaemon cfg
    pure (restoreCode == ExitSuccess && chmodCode == ExitSuccess && restartCode == ExitSuccess)
