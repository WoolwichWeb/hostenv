{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- hostenv-provider CLI: plan | dns-gate | deploy
-- dns-gate ports the legacy scripts/postgen.hs DNS/ACME gate and Cloudflare upsert logic.

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Char (isHexDigit)
import Data.Foldable (toList)
import Data.List (find, intersect, (\\))
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Conversions (convertText)
import Data.Text.Encoding qualified as TE
import Distribution.Compat.Prelude qualified as Sh
import Options.Applicative qualified as OA
import System.Environment qualified as Env
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Turtle (FilePath, (<|>))
import Turtle qualified as Sh
import Prelude hiding (FilePath)

-- -------- CLI --------
data Command
    = CmdPlan
    | CmdDnsGate {node :: Maybe Text, token :: Maybe Text, zone :: Maybe Text, withDnsUpdate :: Bool}
    | CmdDeploy {node :: Maybe Text, skipMigrations :: [Text], ignoreMigrationErrors :: Bool}

data CLI = CLI {cliCmd :: Command}

nodeOpt :: OA.Parser (Maybe Text)
nodeOpt =
    OA.optional . fmap T.pack $
        OA.strOption
            ( OA.long "node"
                <> OA.short 'n'
                <> OA.metavar "NODE"
                <> OA.help "Restrict to a specific node"
            )

tokenOpt :: OA.Parser (Maybe Text)
tokenOpt = OA.optional . fmap T.pack $ OA.strOption (OA.long "cf-token" <> OA.metavar "TOKEN" <> OA.help "Cloudflare API token (overrides CF_API_TOKEN env)")

zoneOpt :: OA.Parser (Maybe Text)
zoneOpt = OA.optional . fmap T.pack $ OA.strOption (OA.long "cf-zone" <> OA.metavar "ZONE" <> OA.help "Cloudflare zone id (overrides CF_ZONE_ID env)")

withDnsUpdateOpt :: OA.Parser Bool
withDnsUpdateOpt =
    OA.switch
        ( OA.long "with-dns-update"
            <> OA.help "Allow Cloudflare upserts (otherwise only checks and disables ACME/forceSSL)"
        )

skipMigrationsOpt :: OA.Parser [Text]
skipMigrationsOpt =
    OA.many . fmap T.pack $
        OA.strOption
            ( OA.long "skip-migrations"
                <> OA.metavar "ENV"
                <> OA.help "Skip migrations for an environment (repeatable)"
            )

ignoreMigrationErrorsOpt :: OA.Parser Bool
ignoreMigrationErrorsOpt =
    OA.switch
        ( OA.long "ignore-migration-errors"
            <> OA.help "Continue deployment even if migration steps fail (may deploy with stale data)"
        )

cliParser :: OA.Parser CLI
cliParser =
    CLI
        <$> OA.hsubparser
            ( OA.command "plan" (OA.info (pure CmdPlan) (OA.progDesc "Generate plan.json, state.json, and flake.nix"))
                <> OA.command "dns-gate" (OA.info (CmdDnsGate <$> nodeOpt <*> tokenOpt <*> zoneOpt <*> withDnsUpdateOpt) (OA.progDesc "Disable ACME/forceSSL for vhosts not pointing at node; add --with-dns-update to upsert Cloudflare records"))
                <> OA.command "deploy" (OA.info (CmdDeploy <$> nodeOpt <*> skipMigrationsOpt <*> ignoreMigrationErrorsOpt) (OA.progDesc "Deploy via deploy-rs"))
            )

cliOpts :: OA.ParserInfo CLI
cliOpts =
    OA.info
        (cliParser OA.<**> OA.helper)
        ( OA.fullDesc
            <> OA.header "hostenv-provider"
        )

-- -------- DNS helpers (dig) --------
stripDot :: Text -> Text
stripDot = T.dropWhileEnd (== '.')

digRR :: Text -> Text -> IO [Text]
digRR name rr = do
    out <- Sh.strict $ Sh.inproc "dig" ["+short", name, rr] Sh.empty
    pure $ filter (not . T.null) $ map (stripDot . T.toLower . T.strip) $ T.lines out

digCNAMEs :: Text -> IO [Text]
digCNAMEs name = digRR name "CNAME"

digAddrs :: Text -> IO [Text]
digAddrs name = do
    a4 <- digRR name "A"
    a6 <- digRR name "AAAA"
    pure $ a4 ++ a6

dnsPointsTo :: Text -> Text -> IO Bool
dnsPointsTo vhost expectedHost = do
    let expectHostNorm = T.toLower expectedHost
    cn <- digCNAMEs vhost
    if expectHostNorm `elem` cn
        then pure True
        else do
            expIPs <- digAddrs expectedHost
            vhIPs <- digAddrs vhost
            pure $ not (null (expIPs `intersect` vhIPs))

discoverPrevNodeFromDns :: Text -> Text -> [Text] -> IO (Maybe Text)
discoverPrevNodeFromDns hostenvHostname envName vhosts =
    let suffix = "." <> T.toLower hostenvHostname
        envHost = envName <> "." <> hostenvHostname
        go [] = pure Nothing
        go (vh : rest) = do
            cn <- digCNAMEs vh
            case find (T.isSuffixOf suffix) cn of
                Just cname ->
                    case T.stripSuffix suffix cname of
                        Just node | node /= "" -> pure (Just node)
                        _ -> pure Nothing
                Nothing -> go rest
     in go (envHost : vhosts)

-- -------- JSON helpers --------
lookupText :: KM.Key -> KM.KeyMap A.Value -> Maybe Text
lookupText k o = case KM.lookup k o of
    Just (A.String t) -> Just t
    _ -> Nothing

lookupObj :: KM.Key -> KM.KeyMap A.Value -> Maybe (KM.KeyMap A.Value)
lookupObj k o = case KM.lookup k o of
    Just (A.Object x) -> Just x
    _ -> Nothing

lookupInt :: KM.Key -> KM.KeyMap A.Value -> Maybe Integer
lookupInt k o = case KM.lookup k o of
    Just (A.Number n) -> case floatingOrInteger n of
        Right i -> Just i
        Left (_ :: Double) -> Nothing
    _ -> Nothing

lookupTextList :: KM.Key -> KM.KeyMap A.Value -> [Text]
lookupTextList k o = case KM.lookup k o of
    Just (A.Array arr) -> mapMaybe asText (toList arr)
    _ -> []
  where
    asText (A.String t) = Just t
    asText _ = Nothing

modifyAt :: [KM.Key] -> (A.Value -> A.Value) -> KM.KeyMap A.Value -> KM.KeyMap A.Value
modifyAt [] _ obj = obj
modifyAt [k] f obj = maybe obj (\v -> KM.insert k (f v) obj) (KM.lookup k obj)
modifyAt (k : ks) f obj =
    case KM.lookup k obj of
        Just (A.Object sub) -> KM.insert k (A.Object (modifyAt ks f sub)) obj
        _ -> obj

setBoolAt :: [Text] -> Bool -> KM.KeyMap A.Value -> KM.KeyMap A.Value
setBoolAt path b = modifyAt (map K.fromText path) (const (A.Bool b))

disableAcmePaths :: Text -> Text -> KM.KeyMap A.Value -> KM.KeyMap A.Value
disableAcmePaths name vhostName root =
    let pEnvEnable = ["environments", name, "virtualHosts", vhostName, "enableACME"]
        pEnvSSL = ["environments", name, "virtualHosts", vhostName, "forceSSL"]
     in setBoolAt pEnvSSL False (setBoolAt pEnvEnable False root)

disableAcmeOnNode :: Text -> Text -> KM.KeyMap A.Value -> KM.KeyMap A.Value
disableAcmeOnNode nodeName vhostName root =
    let pNodeEnable = ["nodes", nodeName, "services", "nginx", "virtualHosts", vhostName, "enableACME"]
        pNodeSSL = ["nodes", nodeName, "services", "nginx", "virtualHosts", vhostName, "forceSSL"]
     in setBoolAt pNodeSSL False (setBoolAt pNodeEnable False root)

-- -------- Migration helpers --------
data EnvInfo = EnvInfo
    { name :: Text
    , userName :: Text
    , node :: Text
    , prevNode :: Maybe Text
    , migrateBackups :: [Text]
    , runtimeDir :: Text
    , vhosts :: [Text]
    , uid :: Maybe Integer
    }

data Snapshot = Snapshot {snapId :: Text}

instance A.FromJSON Snapshot where
    parseJSON = A.withObject "Snapshot" $ \v -> Snapshot <$> v .: "id"

parseSnapshotId :: Text -> Maybe Text
parseSnapshotId out =
    case A.eitherDecode' (BL.fromStrict (TE.encodeUtf8 out)) of
        Right (snaps :: [Snapshot]) ->
            case snaps of
                (s : _) -> Just s.snapId
                _ -> Nothing
        Left _ -> Nothing

parseSnapshotIdFromJournal :: Text -> Maybe Text
parseSnapshotIdFromJournal out =
    let parseLine line =
            let ws = T.words line
             in case dropWhile (/= "snapshot") ws of
                    ("snapshot" : snap : rest) | "saved" `elem` rest ->
                        let snapId = T.takeWhile isHexDigit snap
                         in if T.null snapId then Nothing else Just snapId
                    _ -> Nothing
     in listToMaybe (mapMaybe parseLine (reverse (T.lines out)))

extractEnvInfos :: KM.KeyMap A.Value -> [EnvInfo]
extractEnvInfos envs =
    let parseEnv (kEnv, vEnv) =
            case vEnv of
                A.Object envObj ->
                    case lookupText (K.fromString "node") envObj of
                        Just nodeName ->
                            let envNameText = K.toText kEnv
                                prevNode = lookupText (K.fromString "previousNode") envObj
                                hostenvObj = lookupObj (K.fromString "hostenv") envObj
                                envUserName = fromMaybe envNameText (hostenvObj >>= lookupText (K.fromString "userName"))
                                runtimeRoot = fromMaybe "/run/hostenv" (hostenvObj >>= lookupText (K.fromString "runtimeRoot"))
                                runtimeDir = fromMaybe (runtimeRoot <> "/user/" <> envUserName) (hostenvObj >>= lookupText (K.fromString "runtimeDir"))
                                vhosts =
                                    case lookupObj (K.fromString "virtualHosts") envObj of
                                        Just vhostsObj -> map (K.toText . fst) (KM.toList vhostsObj)
                                        Nothing -> []
                                uid = lookupInt (K.fromString "uid") envObj
                                migrateBackups = lookupTextList (K.fromString "migrations") envObj
                             in Just (EnvInfo envNameText envUserName nodeName prevNode migrateBackups runtimeDir vhosts uid)
                        Nothing -> Nothing
                _ -> Nothing
     in mapMaybe parseEnv (KM.toList envs)

-- -------- Cloudflare helpers --------
data CFRecord = CFRecord
    { id :: Text
    , rType :: Text
    , name :: Text
    , content :: Text
    , proxied :: Maybe Bool
    }

instance A.FromJSON CFRecord where
    parseJSON = A.withObject "CFRecord" $ \v ->
        CFRecord
            <$> v .: "id"
            <*> v .: "type"
            <*> v .: "name"
            <*> v .: "content"
            <*> v .:? "proxied"

data CFList = CFList {lSuccess :: Bool, lResult :: [CFRecord]}
instance A.FromJSON CFList where
    parseJSON = A.withObject "CFList" $ \v ->
        CFList
            <$> v .: "success"
            <*> v .: "result"

newtype CFWrite = CFWrite {wSuccess :: Bool}
instance A.FromJSON CFWrite where
    parseJSON = A.withObject "CFWrite" $ \v ->
        CFWrite <$> v .: "success"

data CFZoneInfo = CFZoneInfo {zName :: Text}
instance A.FromJSON CFZoneInfo where
    parseJSON = A.withObject "CFZoneInfo" $ \v ->
        CFZoneInfo <$> v .: "name"

data CFZoneResp = CFZoneResp {zSuccess :: Bool, zResult :: CFZoneInfo}
instance A.FromJSON CFZoneResp where
    parseJSON = A.withObject "CFZoneResp" $ \v ->
        CFZoneResp
            <$> v .: "success"
            <*> v .: "result"

cfListByName :: Text -> Text -> Text -> IO [CFRecord]
cfListByName token zoneId name = do
    let url = "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records?per_page=1000&name=" <> name
    out <-
        Sh.strict $
            Sh.inproc
                "curl"
                [ "-sS"
                , "-H"
                , "Authorization: Bearer " <> token
                , "-H"
                , "Content-Type: application/json"
                , url
                ]
                Sh.empty
    let bs = BL.fromStrict (TE.encodeUtf8 out)
    case A.eitherDecode' bs of
        Left _ -> pure []
        Right (lst :: CFList) -> if lst.lSuccess then pure lst.lResult else pure []

cfDeleteRecord :: Text -> Text -> Text -> Sh.Shell ()
cfDeleteRecord token zoneId rid = do
    let url = "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records/" <> rid
    _ <-
        Sh.inproc
            "curl"
            [ "-sS"
            , "-X"
            , "DELETE"
            , "-H"
            , "Authorization: Bearer " <> token
            , "-H"
            , "Content-Type: application/json"
            , url
            ]
            Sh.empty
    pure ()

cfUpsertCname :: Text -> Text -> Text -> Text -> IO ()
cfUpsertCname token zoneId name target = do
    existing <- cfListByName token zoneId name
    case filter ((== "CNAME") . (.rType)) existing of
        (c : _) -> do
            let url = "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records/" <> c.id
            let body = "{\"type\":\"CNAME\",\"name\":\"" <> name <> "\",\"content\":\"" <> target <> "\",\"proxied\":false}"
            Sh.stdout $ Sh.inproc "curl" ["-sS", "-X", "PUT", "-H", "Authorization: Bearer " <> token, "-H", "Content-Type: application/json", "--data", body, url] Sh.empty
        [] -> do
            let url = "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records"
            let body = "{\"type\":\"CNAME\",\"name\":\"" <> name <> "\",\"content\":\"" <> target <> "\",\"proxied\":false}"
            Sh.stdout $ Sh.inproc "curl" ["-sS", "-X", "POST", "-H", "Authorization: Bearer " <> token, "-H", "Content-Type: application/json", "--data", body, url] Sh.empty

cfZoneName :: Text -> Text -> IO (Maybe Text)
cfZoneName token zoneId = do
    let url = "https://api.cloudflare.com/client/v4/zones/" <> zoneId
    out <-
        Sh.strict $
            Sh.inproc
                "curl"
                [ "-sS"
                , "-H"
                , "Authorization: Bearer " <> token
                , "-H"
                , "Content-Type: application/json"
                , url
                ]
                Sh.empty
    let bs = BL.fromStrict (TE.encodeUtf8 out)
    case A.eitherDecode' bs of
        Left _ -> pure Nothing
        Right (resp :: CFZoneResp) ->
            if resp.zSuccess
                then pure (Just resp.zResult.zName)
                else pure Nothing

isSubdomainOf :: Text -> Text -> Bool
isSubdomainOf host zone =
    let host' = T.toLower (stripDot host)
        zone' = T.toLower (stripDot zone)
     in host' == zone' || T.isSuffixOf ("." <> zone') host'

-- -------- DNS gate --------
runDnsGate :: Maybe Text -> Maybe Text -> Maybe Text -> Bool -> IO ()
runDnsGate mNode mTok mZone withDnsUpdate = do
    let dest = "generated"
    let planPath = dest <> "/plan.json"
    planExists <- Sh.testfile (fromString (T.unpack planPath))
    when (not planExists) $ do
        BLC.putStrLn "plan.json not found; dns-gate noop"
        Sh.exitSuccess
    raw <- BL.readFile (T.unpack planPath)
    case A.eitherDecode' raw of
        Left err -> error err
        Right (plan :: KM.KeyMap A.Value) -> do
            let hostenvHostname = fromMaybe "" (lookupText (K.fromString "hostenvHostname") plan)
            let cfPlan = lookupObj (K.fromString "cloudflare") plan
            let envs = fromMaybe KM.empty (lookupObj (K.fromString "environments") plan)
            let nodes = fromMaybe KM.empty (lookupObj (K.fromString "nodes") plan)
            cfTok <- case mTok of
                Just t -> pure (Just (T.unpack t))
                Nothing -> case cfPlan >>= lookupText (K.fromString "apiTokenFile") of
                    Just path -> Just <$> readFileTrim (T.unpack path)
                    Nothing -> Env.lookupEnv "CF_API_TOKEN"
            cfZone <- case mZone of
                Just z -> pure (Just (T.unpack z))
                Nothing -> case cfPlan >>= lookupText (K.fromString "zoneId") of
                    Just z -> pure (Just (T.unpack z))
                    Nothing -> Env.lookupEnv "CF_ZONE_ID"
            cfZoneName' <- case (cfTok, cfZone) of
                (Just t, Just z) -> cfZoneName (T.pack t) (T.pack z)
                _ -> pure Nothing
            let hasCF = isJustPair cfTok cfZone
            plan' <- foldlM (processEnv hostenvHostname nodes hasCF cfTok cfZone cfZoneName' withDnsUpdate) plan (KM.toList envs)
            let tmpPath = dest <> "/plan.json.tmp"
            BL.writeFile (T.unpack tmpPath) (A.encode plan')
            pretty <- Sh.strict $ Sh.inproc "jq" ["-S", ".", tmpPath] Sh.empty
            BL.writeFile (T.unpack tmpPath) (BL.fromStrict (TE.encodeUtf8 pretty))
            Sh.mv (fromString (T.unpack tmpPath)) (fromString (T.unpack planPath))
            BLC.putStrLn "✅ dns-gate updated plan.json"
  where
    isJustPair (Just _) (Just _) = True
    isJustPair _ _ = False
    readFileTrim p = T.unpack . T.strip . T.pack <$> readFile p
    foldlM f z [] = pure z
    foldlM f z (x : xs) = f z x >>= \z' -> foldlM f z' xs
    processEnv hostenvHostname nodes hasCF cfTok cfZone cfZoneName' withDnsUpdate acc (kEnv, vEnv) =
        case vEnv of
            A.Object envObj -> do
                let vhosts = fromMaybe KM.empty (lookupObj (K.fromString "virtualHosts") envObj)
                foldlM (processVhost hostenvHostname nodes hasCF cfTok cfZone cfZoneName' withDnsUpdate (K.toText kEnv)) acc (KM.toList vhosts)
            _ -> pure acc
    processVhost hostenvHostname nodes hasCF cfTok cfZone cfZoneName' withDnsUpdate name planAcc (vhKey, _vhObj) = do
        let vhName = K.toText vhKey
        let node = case lookupObj (K.fromString "environments") planAcc >>= KM.lookup (K.fromText name) of
                Just (A.Object o) -> lookupText (K.fromString "node") o
                _ -> Nothing
        let nodeName = fromMaybe "" (node <|> mNode)
        let expectedHost = if nodeName == "" then vhName else nodeName <> "." <> hostenvHostname
        ok <- dnsPointsTo vhName expectedHost
        planAcc' <-
            if ok
                then pure planAcc
                else do
                    when (hasCF && withDnsUpdate) $
                        case (cfTok, cfZone) of
                            (Just t, Just z) ->
                                case cfZoneName' of
                                    Just zoneName ->
                                        if isSubdomainOf vhName zoneName
                                            then cfUpsertCname (T.pack t) (T.pack z) vhName expectedHost >> pure ()
                                            else Sh.print ("Skipping Cloudflare upsert for " <> vhName <> " (outside zone " <> zoneName <> ")")
                                    Nothing -> Sh.print "DNS setup ('dnsGate') failed: could not resolve Cloudflare zone name"
                            (Nothing, _) -> Sh.print "DNS setup ('dnsGate') failed: Cloudflare token was not provided"
                            (_, Nothing) -> Sh.print "DNS setup ('dnsGate') failed: Cloudflare zone was not provided"
                    let plan1 = disableAcmePaths name vhName planAcc
                    let plan2 = disableAcmeOnNode nodeName vhName plan1
                    pure plan2
        pure planAcc'

-- -------- Plan (copy from provider package) --------
runPlan :: IO ()
runPlan = do
    _ <- Sh.procs "hostenv-provider-plan" [] Sh.empty
    pure ()

-- -------- Deploy wrapper --------
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

heredocSubst :: Text -> Text -> Text
heredocSubst tag body =
    let body' = if T.isSuffixOf "\n" body then body else body <> "\n"
     in "\"$(cat <<'" <> tag <> "'\n" <> body' <> tag <> "\n)\""

bashLcCommand :: Text -> Text
bashLcCommand cmd =
    let tag = pickTag "HOSTENV_CMD" cmd
     in "bash -lc " <> heredocSubst tag cmd

sudoBashLcCommand :: Text -> Text -> Text
sudoBashLcCommand user cmd =
    "sudo -u " <> shellEscape user <> " -H -- " <> bashLcCommand cmd

scriptForArgs :: [Text] -> Text
scriptForArgs args =
    let header = "#!/usr/bin/env bash\nset -euo pipefail\n"
     in case args of
            ["bash", "-lc", cmd] ->
                header <> bashLcCommand cmd <> "\n"
            ["sudo", "-u", user, "-H", "--", "bash", "-lc", cmd] ->
                header <> sudoBashLcCommand user cmd <> "\n"
            _ ->
                header <> "exec " <> T.intercalate " " (map shellEscape args) <> "\n"

runRemoteScript :: Text -> Text -> IO ExitCode
runRemoteScript userHost script = do
    let runner =
            "tmp=$(mktemp /tmp/hostenv-provider-XXXXXX.sh); "
                <> "cat > \"$tmp\"; "
                <> "bash \"$tmp\"; "
                <> "status=$?; "
                <> "rm -f \"$tmp\"; "
                <> "exit $status"
    (code, _out, _err) <- readProcessWithExitCode "ssh" ["-o", "BatchMode=yes", T.unpack userHost, T.unpack runner] (T.unpack script)
    pure code

runRemoteScriptStrict :: Text -> Text -> IO Text
runRemoteScriptStrict userHost script = do
    let runner =
            "tmp=$(mktemp /tmp/hostenv-provider-XXXXXX.sh); "
                <> "cat > \"$tmp\"; "
                <> "bash \"$tmp\"; "
                <> "status=$?; "
                <> "rm -f \"$tmp\"; "
                <> "exit $status"
    (code, out, err) <- readProcessWithExitCode "ssh" ["-o", "BatchMode=yes", T.unpack userHost, T.unpack runner] (T.unpack script)
    case code of
        ExitSuccess -> pure (T.pack out)
        ExitFailure c ->
            error
                ( "ssh failed for "
                    <> T.unpack userHost
                    <> " (exit "
                    <> show c
                    <> "): "
                    <> err
                )

runRemote :: Text -> [Text] -> IO ExitCode
runRemote userHost args =
    runRemoteScript userHost (scriptForArgs args)

runRemoteStrict :: Text -> [Text] -> IO Text
runRemoteStrict userHost args =
    runRemoteScriptStrict userHost (scriptForArgs args)

runMigrationBackup :: Text -> EnvInfo -> Text -> Text -> IO Text
runMigrationBackup hostenvHostname envInfo prevNode backupName = do
    let prevHost = prevNode <> "." <> hostenvHostname
    let deployHost = "deploy@" <> prevHost
    let sudoArgs cmd =
            [ "sudo"
            , "-u"
            , envInfo.userName
            , "-H"
            , "--"
            , "bash"
            , "-lc"
            , cmd
            ]
    let unitFor name = "restic-backups-" <> name <> ".service"
    let userBusPrefix = "export XDG_RUNTIME_DIR=/run/user/$(id -u); "
    let loadStateCmd unit =
            userBusPrefix
                <> "systemctl --user show -p LoadState "
                <> unit
                <> " 2>/dev/null || echo \"LoadState=not-found\""
    let parseState t =
            let trimmed = T.strip t
             in fromMaybe trimmed (T.stripPrefix "LoadState=" trimmed)
    let chooseBackupName = do
            state <- fmap parseState (runRemoteStrict deployHost (sudoArgs (loadStateCmd (unitFor backupName))))
            if state == "loaded"
                then pure backupName
                else case T.stripSuffix "-migrate" backupName of
                    Just baseName -> do
                        baseState <- fmap parseState (runRemoteStrict deployHost (sudoArgs (loadStateCmd (unitFor baseName))))
                        if baseState == "loaded"
                            then do
                                Sh.print
                                    ( "hostenv: migrate backup unit "
                                        <> unitFor backupName
                                        <> " not found on "
                                        <> prevHost
                                        <> "; falling back to "
                                        <> baseName
                                    )
                                pure baseName
                            else error ("migration backup unit missing: " <> T.unpack (unitFor backupName) <> " (fallback " <> T.unpack (unitFor baseName) <> " not found)")
                    Nothing -> error ("migration backup unit missing: " <> T.unpack (unitFor backupName))
    effectiveName <- chooseBackupName
    let unit = unitFor effectiveName
    let startCmd =
            "set -euo pipefail; "
                <> userBusPrefix
                <> "systemctl --user start --wait "
                <> unit
    startExit <- runRemote deployHost (sudoArgs startCmd)
    case startExit of
        ExitSuccess -> pure ()
        ExitFailure code -> error ("migration backup failed for " <> T.unpack envInfo.name <> ":" <> T.unpack backupName <> " (exit " <> show code <> ")")

    let wrapperPath = "~/.local/bin/restic-" <> effectiveName
    let wrapperCheckCmd = "test -x " <> wrapperPath
    wrapperExit <- runRemote deployHost (sudoArgs wrapperCheckCmd)
    if wrapperExit == ExitSuccess
        then do
            let snapshotCmdTagged =
                    "set -euo pipefail; "
                        <> wrapperPath
                        <> " snapshots --latest 1 --tag "
                        <> effectiveName
                        <> " --json"
            snapOutTagged <- runRemoteStrict deployHost (sudoArgs snapshotCmdTagged)
            case parseSnapshotId snapOutTagged of
                Just snap -> pure snap
                Nothing -> do
                    let snapshotCmdUntagged =
                            "set -euo pipefail; "
                                <> wrapperPath
                                <> " snapshots --latest 1 --json"
                    snapOutUntagged <- runRemoteStrict deployHost (sudoArgs snapshotCmdUntagged)
                    case parseSnapshotId snapOutUntagged of
                        Just snap -> do
                            Sh.print
                                ( "hostenv: warning: no tagged restic snapshot found for "
                                    <> effectiveName
                                    <> " on "
                                    <> prevHost
                                    <> "; using latest untagged snapshot"
                                )
                            pure snap
                        Nothing -> error ("could not parse snapshot id for " <> T.unpack envInfo.name <> ":" <> T.unpack backupName <> " (tagged or untagged)")
        else do
            Sh.print ("hostenv: restic wrapper missing for " <> effectiveName <> " on " <> prevHost <> "; reading snapshot id from journal")
            let invocationCmd = userBusPrefix <> "systemctl --user show -p InvocationID --value " <> unit
            invOut <- runRemoteStrict deployHost (sudoArgs invocationCmd)
            let invocation = T.strip invOut
            let journalCmd =
                    userBusPrefix
                        <> if T.null invocation
                            then "journalctl --user -u " <> unit <> " -n 200 -o cat --no-pager || true"
                            else "journalctl --user _SYSTEMD_INVOCATION_ID=" <> invocation <> " -o cat --no-pager || true"
            logOut <- runRemoteStrict deployHost (sudoArgs journalCmd)
            case parseSnapshotIdFromJournal logOut of
                Just snap -> pure snap
                Nothing -> error ("could not parse snapshot id from journal for " <> T.unpack envInfo.name <> ":" <> T.unpack backupName)

resolvePrevNode :: Text -> EnvInfo -> IO (Maybe Text)
resolvePrevNode hostenvHostname envInfo =
    case envInfo.prevNode of
        Just prev -> pure (Just prev)
        Nothing -> do
            discovered <- discoverPrevNodeFromDns hostenvHostname envInfo.userName (envInfo.vhosts)
            case discovered of
                Just node -> do
                    Sh.print ("hostenv: previous node for " <> envInfo.name <> " discovered via DNS: " <> node)
                    pure (Just node)
                Nothing -> pure Nothing

writeRestorePlan :: Text -> EnvInfo -> Text -> [(Text, Text)] -> IO ()
writeRestorePlan hostenvHostname envInfo prevNode snapshots = do
    let newHost = envInfo.node <> "." <> hostenvHostname
    let deployHost = "deploy@" <> newHost
    let runtimeDir = envInfo.runtimeDir
    let restoreDir = runtimeDir <> "/restore"
    let restorePath = restoreDir <> "/plan.json"
    let envUser = envInfo.userName
    let ownerSpec = maybe envUser (T.pack . show) envInfo.uid
    let runtimeGroup = "users"
    let groupSpec = ownerSpec
    let snapObj = KM.fromList (map (\(name, sid) -> (K.fromText name, A.String sid)) snapshots)
    let payload =
            A.object
                [ "sourceNode" .= prevNode
                , "snapshots" .= A.Object snapObj
                ]

    let dirCmd =
            let
                args :: (Monoid a) => a -> [a] -> a
                args x ys = x <> mconcat ys
             in
                T.intercalate
                    " && "
                    [ "sudo mkdir -p " <> runtimeDir
                    , "sudo chmod 2700 " <> runtimeDir
                    , "sudo chown " `args` [ownerSpec, ":", runtimeGroup, " ", runtimeDir]
                    , "sudo mkdir -p " <> restoreDir
                    , "sudo chmod 0700 " <> restoreDir
                    , "sudo chown " `args` [ownerSpec, ":", groupSpec, " ", restoreDir]
                    ]
    dirExit <-
        runRemote
            deployHost
            [ "bash"
            , "-lc"
            , dirCmd
            ]
    case dirExit of
        ExitSuccess -> pure ()
        ExitFailure code -> error ("failed to create restore dir on " <> T.unpack deployHost <> " (exit " <> show code <> ")")

    fileExit <- runRemote deployHost ["bash", "-lc", "sudo install -m 0600 /dev/null " <> restorePath <> " && sudo chown " <> ownerSpec <> ":" <> groupSpec <> " " <> restorePath]
    case fileExit of
        ExitSuccess -> pure ()
        ExitFailure code -> error ("failed to create restore plan file on " <> T.unpack deployHost <> " (exit " <> show code <> ")")

    let payloadText = TE.decodeUtf8 (BL.toStrict (A.encode payload))
    let payloadTag = pickTag "HOSTENV_JSON" payloadText
    let writeCmd =
            T.unlines
                [ "sudo tee " <> shellEscape restorePath <> " >/dev/null <<'" <> payloadTag <> "'"
                , payloadText
                , payloadTag
                ]
    writeExit <- runRemote deployHost ["bash", "-lc", writeCmd]
    case writeExit of
        ExitSuccess -> pure ()
        ExitFailure code -> error ("failed to write restore plan for " <> T.unpack envInfo.name <> " (exit " <> show code <> ")")

runDeploy :: Maybe Text -> [Text] -> Bool -> IO ()
runDeploy mNode skipMigrations ignoreMigrationErrors = do
    let planPath = "generated/plan.json"
    planExists <- Sh.testfile (fromString (T.unpack planPath))
    when (not planExists) $ do
        BLC.putStrLn "plan.json not found; deploy aborted"
        Sh.exit (ExitFailure 1)

    raw <- BL.readFile (T.unpack planPath)
    case A.eitherDecode' raw of
        Left err -> error err
        Right (plan :: KM.KeyMap A.Value) -> do
            let hostenvHostname = fromMaybe "" (lookupText (K.fromString "hostenvHostname") plan)
            let envs = fromMaybe KM.empty (lookupObj (K.fromString "environments") plan)
            when (hostenvHostname == "") $ error "hostenvHostname missing from plan.json"
            let envInfos = extractEnvInfos envs
            let envInfosFiltered =
                    case mNode of
                        Nothing -> envInfos
                        Just n -> filter (\e -> e.node == n) envInfos
            let shouldSkip envInfo =
                    envInfo.name `elem` skipMigrations || envInfo.userName `elem` skipMigrations
            let skipHits =
                    filter
                        (\s -> any (\e -> e.name == s || e.userName == s) envInfosFiltered)
                        skipMigrations
            let skipMisses = skipMigrations \\ skipHits
            case mNode of
                Nothing -> pure ()
                Just n -> Sh.print ("hostenv-provider: deploy filtered to node " <> n)
            Sh.print ("hostenv-provider: " <> T.pack (show (length envInfosFiltered)) <> " environment(s) considered")
            when (not (null skipHits)) $
                Sh.print ("hostenv-provider: skipping migrations for: " <> T.intercalate ", " skipHits)
            when (not (null skipMisses)) $
                Sh.print ("hostenv-provider: warning: skip-migrations targets not found: " <> T.intercalate ", " skipMisses)
            when ignoreMigrationErrors $
                Sh.print "hostenv-provider: warning: migration errors will be ignored; deployments may proceed with stale data"
            migrations <- fmap catMaybes $
                forM envInfosFiltered $ \envInfo -> do
                    if shouldSkip envInfo
                        then pure Nothing
                        else
                            if envInfo.migrateBackups == []
                                then pure Nothing
                                else do
                                    prevNode <- resolvePrevNode hostenvHostname envInfo
                                    pure $ case prevNode of
                                        Just prev | prev /= envInfo.node -> Just (envInfo, prev)
                                        _ -> Nothing

            if null migrations
                then Sh.print "hostenv-provider: no migrations required"
                else do
                    Sh.print ("hostenv-provider: migrations required for " <> T.pack (show (length migrations)) <> " environment(s)")
                    forM_ migrations $ \(envInfo, prevNode) -> do
                        let backups = T.intercalate ", " envInfo.migrateBackups
                        Sh.print ("hostenv-provider: migrate backups for " <> envInfo.name <> " from " <> prevNode <> " -> " <> envInfo.node <> " (" <> backups <> ")")
                    let runMigration (envInfo, prevNode) = do
                            snapshots <- forM envInfo.migrateBackups $ \backupName -> do
                                snap <- runMigrationBackup hostenvHostname envInfo prevNode backupName
                                pure (backupName, snap)
                            writeRestorePlan hostenvHostname envInfo prevNode snapshots
                            let snapshotPairs = map (\(name, sid) -> name <> "=" <> sid) snapshots
                            Sh.print ("hostenv-provider: restore plan written for " <> envInfo.name <> " (snapshots: " <> T.intercalate ", " snapshotPairs <> ")")
                    if ignoreMigrationErrors
                        then do
                            failures <- fmap catMaybes $
                                forM migrations $ \(envInfo, prevNode) -> do
                                    res <- try (runMigration (envInfo, prevNode)) :: IO (Either SomeException ())
                                    case res of
                                        Right _ -> pure Nothing
                                        Left err -> do
                                            Sh.print ("hostenv-provider: warning: migration failed for " <> envInfo.name <> ": " <> T.pack (displayException err))
                                            pure (Just envInfo.name)
                            when (not (null failures)) $
                                Sh.print ("hostenv-provider: warning: ignored migration failures for " <> T.intercalate ", " failures)
                        else
                            forM_ migrations runMigration

            let nodeArg = maybe [] (\n -> ["-s", n]) mNode
            let deployArgs = ["run", "github:serokell/deploy-rs", "--", "--remote-build", ".#"] <> nodeArg
            Sh.print ("hostenv-provider: running nix " <> T.unwords deployArgs)
            Sh.exit =<< Sh.proc "nix" deployArgs Sh.empty

-- -------- Main --------
main :: IO ()
main = do
    CLI cmd <- OA.execParser cliOpts
    case cmd of
        CmdPlan -> runPlan
        CmdDnsGate mNode mTok mZone withDnsUpdate -> runDnsGate mNode mTok mZone withDnsUpdate
        CmdDeploy mNode skipMigrations ignoreMigrationErrors -> runDeploy mNode skipMigrations ignoreMigrationErrors
