{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- hostenv-provider CLI: plan | dns-gate | deploy
-- dns-gate ports the legacy scripts/postgen.hs DNS/ACME gate and Cloudflare upsert logic.

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (iparseEither, parseJSON)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Char (isAlphaNum, isHexDigit)
import Data.Foldable (toList)
import Data.List (find, intersect, (\\))
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Conversions (convertText)
import Data.Text.Encoding qualified as TE
import Distribution.Compat.Prelude qualified as Sh
import Hostenv.Provider.DeployGuidance (trustedKeySetupLines)
import Hostenv.Provider.DeployPreflight qualified as Preflight
import Options.Applicative qualified as OA
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.Exit (ExitCode (..))
import System.FilePath qualified as FP
import System.Process (readProcessWithExitCode)
import Turtle (FilePath, (<|>))
import Turtle qualified as Sh
import Prelude hiding (FilePath)

-- -------- CLI --------
data Command
    = CmdPlan
    | CmdDnsGate {node :: Maybe Text, token :: Maybe Text, zone :: Maybe Text, withDnsUpdate :: Bool}
    | CmdDeploy
        { node :: Maybe Text
        , signingKeyFile :: Maybe Text
        , remoteBuild :: Bool
        , skipMigrations :: [Text]
        , migrationSources :: [Text]
        , ignoreMigrationErrors :: Bool
        }

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

signingKeyFileOpt :: OA.Parser (Maybe Text)
signingKeyFileOpt =
    OA.optional . fmap T.pack $
        OA.strOption
            ( OA.long "signing-key-file"
                <> OA.metavar "PATH"
                <> OA.help "Path to Nix signing secret key (defaults to HOSTENV_SIGNING_KEY_FILE or secrets/signing/secret.key)"
            )

remoteBuildOpt :: OA.Parser Bool
remoteBuildOpt =
    OA.switch
        ( OA.long "remote-build"
            <> OA.help "Force deploy-rs remote builds for all targets in this run"
        )

ignoreMigrationErrorsOpt :: OA.Parser Bool
ignoreMigrationErrorsOpt =
    OA.switch
        ( OA.long "ignore-migration-errors"
            <> OA.help "Continue deployment even if migration steps fail (may deploy with stale data)"
        )

migrationSourceOpt :: OA.Parser [Text]
migrationSourceOpt =
    OA.many . fmap T.pack $
        OA.strOption
            ( OA.long "migration-source"
                <> OA.metavar "ENV=NODE"
                <> OA.help "Force migration source node for an environment (repeatable)"
            )

cliParser :: OA.Parser CLI
cliParser =
    CLI
        <$> OA.hsubparser
            ( OA.command "plan" (OA.info (pure CmdPlan) (OA.progDesc "Generate plan.json, state.json, and flake.nix"))
                <> OA.command "dns-gate" (OA.info (CmdDnsGate <$> nodeOpt <*> tokenOpt <*> zoneOpt <*> withDnsUpdateOpt) (OA.progDesc "Disable ACME/forceSSL for vhosts not pointing at node; add --with-dns-update to upsert Cloudflare records"))
                <> OA.command "deploy" (OA.info (CmdDeploy <$> nodeOpt <*> signingKeyFileOpt <*> remoteBuildOpt <*> skipMigrationsOpt <*> migrationSourceOpt <*> ignoreMigrationErrorsOpt) (OA.progDesc "Deploy via deploy-rs"))
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

lookupBool :: KM.Key -> KM.KeyMap A.Value -> Maybe Bool
lookupBool k o = case KM.lookup k o of
    Just (A.Bool b) -> Just b
    _ -> Nothing

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
data DeploymentStatus = NotAttempted | Skipped | Succeeded | Failed ExitCode
data EnvInfo = EnvInfo
    { userName :: Text
    , node :: Text
    , prevNode :: Maybe Text
    , migrateBackups :: [Text]
    , runtimeDir :: Text
    , vhosts :: [Text]
    , uid :: Maybe Integer
    , deploymentStatus :: DeploymentStatus
    }

-- Note: check the nix code that generates plan.json
-- (modules/entrypoints/provider/plan.nix)
-- The name key is always == userName. So, it's okay to use
-- userName here as the environment name/identifier.
toNamed :: [EnvInfo] -> [(Text, EnvInfo)]
toNamed = map (\e -> (e.userName, e))

uniqueNames :: [EnvInfo] -> [Text]
uniqueNames = S.toList . S.fromList . map (.userName)

toNodeNamed :: [EnvInfo] -> [(Text, EnvInfo)]
toNodeNamed = map (\e -> (e.node, e))

uniqueNodeNames :: [EnvInfo] -> [Text]
uniqueNodeNames = S.toList . S.fromList . map (.node)

instance A.FromJSON EnvInfo where
    parseJSON = A.withObject "EnvInfo" $ \o -> do
        hostenvObj <- o .: "hostenv"
        userName <- hostenvObj .: "userName"
        runtimeDir <- hostenvObj .: "runtimeDir"
        node <- o .: "node"
        prevNode <- o .:? "previousNode"
        migrateBackups <- o .:? "migrations" .!= []
        vhostsObj <- o .:? "virtualHosts" .!= (KM.empty :: KM.KeyMap A.Value)
        let vhosts = map (K.toText . fst) (KM.toList vhostsObj)
        uid <- o .:? "uid"
        pure $
            EnvInfo
                { userName = userName
                , node = node
                , prevNode = prevNode
                , migrateBackups = migrateBackups
                , runtimeDir = runtimeDir
                , vhosts = vhosts
                , uid = uid
                , deploymentStatus = NotAttempted
                }

data NodeConnection = NodeConnection
    { connHostname :: Text
    , connSshOpts :: [Text]
    }

instance A.FromJSON NodeConnection where
    parseJSON = A.withObject "NodeConnection" $ \o ->
        NodeConnection
            <$> o .: "hostname"
            <*> o .:? "sshOpts" .!= []

defaultNodeConnection :: Text -> Text -> NodeConnection
defaultNodeConnection hostenvHostname nodeName =
    NodeConnection
        { connHostname = nodeName <> "." <> hostenvHostname
        , connSshOpts = []
        }

lookupNodeConnection :: KM.KeyMap A.Value -> Text -> Maybe NodeConnection
lookupNodeConnection plan nodeName = do
    conns <- lookupObj (K.fromString "nodeConnections") plan
    raw <- KM.lookup (K.fromText nodeName) conns
    either (const Nothing) Just (iparseEither parseJSON raw)

nodeConnectionFor :: KM.KeyMap A.Value -> Text -> Text -> NodeConnection
nodeConnectionFor plan hostenvHostname nodeName =
    fromMaybe
        (defaultNodeConnection hostenvHostname nodeName)
        (lookupNodeConnection plan nodeName)

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
                    ("snapshot" : snap : rest)
                        | "saved" `elem` rest ->
                            let snapId = T.takeWhile isHexDigit snap
                             in if T.null snapId then Nothing else Just snapId
                    _ -> Nothing
     in listToMaybe (mapMaybe parseLine (reverse (T.lines out)))

parseMigrationSource :: Text -> Maybe (Text, Text)
parseMigrationSource raw =
    let (envName, rest) = T.breakOn "=" raw
        nodeName = T.drop 1 rest
     in if T.null envName || T.null rest || T.null nodeName
            then Nothing
            else Just (envName, nodeName)

duplicateValues :: [Text] -> [Text]
duplicateValues =
    S.toList . snd . foldl step (S.empty, S.empty)
  where
    step (seen, dups) value =
        if S.member value seen
            then (seen, S.insert value dups)
            else (S.insert value seen, dups)

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

runPlan :: IO ()
runPlan = do
    let dest = "generated" :: Text
    let planDest = dest <> "/plan.json"
    let stateDest = dest <> "/state.json"
    let flakeDest = dest <> "/flake.nix"
    Sh.mktree (fromString (T.unpack dest))

    stateExists <- Sh.testfile (fromString (T.unpack stateDest))
    unless stateExists $ do
        BLC.writeFile (T.unpack stateDest) "{}\n"
        _ <- Sh.procs "git" ["add", stateDest] Sh.empty
        pure ()

    system <- nixEvalRaw ["eval", "--impure", "--raw", "--expr", "builtins.currentSystem"]
    let planAttrBase = ".#lib.provider.planPaths." <> T.strip system
    planSource <- nixBuildPath (planAttrBase <> ".plan")
    stateSource <- nixBuildPath (planAttrBase <> ".state")
    flakeSource <- nixBuildPath (planAttrBase <> ".flake")

    planRaw <- BL.readFile (T.unpack planSource)
    BL.writeFile (T.unpack planDest) planRaw
    pretty <- Sh.strict $ Sh.inproc "jq" ["-S", ".", planDest] Sh.empty
    BL.writeFile (T.unpack planDest) (BL.fromStrict (TE.encodeUtf8 pretty))

    stateRaw <- BL.readFile (T.unpack stateSource)
    BL.writeFile (T.unpack stateDest) stateRaw

    flakeRaw <- BL.readFile (T.unpack flakeSource)
    BL.writeFile (T.unpack flakeDest) flakeRaw

    _ <- Sh.procs "git" ["add", dest] Sh.empty
    _ <- Sh.procs "nix" (nixCommonArgs ++ ["flake", "lock", "./" <> dest]) Sh.empty
    _ <- Sh.procs "git" ["add", dest] Sh.empty
    BLC.putStrLn ("✅ Infrastructure configuration written to " <> BLC.pack (T.unpack dest))
  where
    nixCommonArgs :: [Text]
    nixCommonArgs = ["--extra-experimental-features", "nix-command flakes"]

    nixEvalRaw :: [Text] -> IO Text
    nixEvalRaw args = T.strip <$> Sh.strict (Sh.inproc "nix" (nixCommonArgs ++ args) Sh.empty)

    nixBuildPath :: Text -> IO Text
    nixBuildPath attr =
        T.strip <$> Sh.strict (Sh.inproc "nix" (nixCommonArgs ++ ["build", "--no-link", "--print-out-paths", attr]) Sh.empty)

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

data SshTarget = SshTarget
    { targetUserHost :: Text
    , targetHost :: Text
    , targetSshOpts :: [Text]
    }

mkSshTarget :: Text -> NodeConnection -> SshTarget
mkSshTarget user conn =
    SshTarget
        { targetUserHost = user <> "@" <> conn.connHostname
        , targetHost = conn.connHostname
        , targetSshOpts = conn.connSshOpts
        }

runRemoteScript :: SshTarget -> Text -> IO ExitCode
runRemoteScript target script = do
    let runner =
            "tmp=$(mktemp /tmp/hostenv-provider-XXXXXX.sh); "
                <> "cat > \"$tmp\"; "
                <> "bash \"$tmp\"; "
                <> "status=$?; "
                <> "rm -f \"$tmp\"; "
                <> "exit $status"
    let sshArgs = ["-o", "BatchMode=yes"] ++ target.targetSshOpts ++ [target.targetUserHost, runner]
    (code, _out, _err) <- readProcessWithExitCode "ssh" (map T.unpack sshArgs) (T.unpack script)
    pure code

runRemoteScriptOutput :: SshTarget -> Text -> IO (ExitCode, Text, Text)
runRemoteScriptOutput target script = do
    let runner =
            "tmp=$(mktemp /tmp/hostenv-provider-XXXXXX.sh); "
                <> "cat > \"$tmp\"; "
                <> "bash \"$tmp\"; "
                <> "status=$?; "
                <> "rm -f \"$tmp\"; "
                <> "exit $status"
    let sshArgs = ["-o", "BatchMode=yes"] ++ target.targetSshOpts ++ [target.targetUserHost, runner]
    (code, out, err) <- readProcessWithExitCode "ssh" (map T.unpack sshArgs) (T.unpack script)
    pure (code, T.pack out, T.pack err)

runRemoteScriptStrict :: SshTarget -> Text -> IO Text
runRemoteScriptStrict target script = do
    let runner =
            "tmp=$(mktemp /tmp/hostenv-provider-XXXXXX.sh); "
                <> "cat > \"$tmp\"; "
                <> "bash \"$tmp\"; "
                <> "status=$?; "
                <> "rm -f \"$tmp\"; "
                <> "exit $status"
    let sshArgs = ["-o", "BatchMode=yes"] ++ target.targetSshOpts ++ [target.targetUserHost, runner]
    (code, out, err) <- readProcessWithExitCode "ssh" (map T.unpack sshArgs) (T.unpack script)
    case code of
        ExitSuccess -> pure (T.pack out)
        ExitFailure c ->
            error
                ( "ssh failed for "
                    <> T.unpack target.targetUserHost
                    <> " (exit "
                    <> show c
                    <> "): "
                    <> err
                )

runRemote :: SshTarget -> [Text] -> IO ExitCode
runRemote target args =
    runRemoteScript target (scriptForArgs args)

runRemoteStrict :: SshTarget -> [Text] -> IO Text
runRemoteStrict target args =
    runRemoteScriptStrict target (scriptForArgs args)

runRemoteOutput :: SshTarget -> [Text] -> IO (ExitCode, Text, Text)
runRemoteOutput target args =
    runRemoteScriptOutput target (scriptForArgs args)

data SigningKeyInfo = SigningKeyInfo
    { secretKeyPath :: Text
    , publicKeyPath :: Text
    , publicKey :: Text
    , wasGenerated :: Bool
    }

defaultSigningKeyPath :: Text
defaultSigningKeyPath = "secrets/signing/secret.key"

resolveSigningKeyPath :: Maybe Text -> IO Text
resolveSigningKeyPath mArg = do
    envPath <- Env.lookupEnv "HOSTENV_SIGNING_KEY_FILE"
    pure $
        fromMaybe
            defaultSigningKeyPath
            (mArg <|> (T.pack <$> envPath))

sanitizeKeyName :: Text -> Text
sanitizeKeyName =
    T.map (\c -> if isAlphaNum c || c `elem` ['.', '-', '_'] then c else '-')

signingKeyNameFor :: Text -> Text
signingKeyNameFor hostenvHostname =
    let normalized = sanitizeKeyName (T.toLower (T.strip hostenvHostname))
     in if T.null normalized
            then "hostenv-provider-1"
            else normalized <> "-hostenv-provider-1"

localCommandFailure :: Text -> [Text] -> ExitCode -> Text -> Text -> Text
localCommandFailure context args code out err =
    let out' = T.strip out
        err' = T.strip err
        detail =
            if T.null err'
                then out'
                else err'
     in context
            <> ": "
            <> T.unwords (map shellEscape args)
            <> " (exit "
            <> T.pack (show code)
            <> ")"
            <> if T.null detail then "" else " - " <> detail

convertSecretToPublic :: Text -> IO Text
convertSecretToPublic secret = do
    (code, out, err) <- readProcessWithExitCode "nix" ["key", "convert-secret-to-public"] (T.unpack secret <> "\n")
    case code of
        ExitSuccess ->
            let pub = T.strip (T.pack out)
             in if T.null pub
                    then error "hostenv-provider: failed to derive public signing key (empty output)"
                    else pure pub
        ExitFailure _ ->
            error (T.unpack (localCommandFailure "hostenv-provider: failed to derive public signing key" ["nix", "key", "convert-secret-to-public"] code (T.pack out) (T.pack err)))

chmodPath :: Text -> Text -> IO ()
chmodPath mode path = do
    (code, out, err) <- readProcessWithExitCode "chmod" [T.unpack mode, T.unpack path] ""
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ ->
            error (T.unpack (localCommandFailure "hostenv-provider: failed to set file permissions" ["chmod", mode, path] code (T.pack out) (T.pack err)))

ensureSigningKeyInfo :: Text -> Maybe Text -> IO SigningKeyInfo
ensureSigningKeyInfo hostenvHostname mKeyPath = do
    keyPath <- resolveSigningKeyPath mKeyPath
    let keyPathS = T.unpack keyPath
    let keyDirS = FP.takeDirectory keyPathS
    let pubPathS = keyDirS FP.</> "public.key"
    let pubPath = T.pack pubPathS
    keyExists <- Dir.doesFileExist keyPathS
    if keyExists
        then do
            secret <- T.strip . T.pack <$> readFile keyPathS
            when (T.null secret) $
                error ("hostenv-provider: signing key exists but is empty: " <> keyPathS)
            pub <- convertSecretToPublic secret
            Dir.createDirectoryIfMissing True keyDirS
            sidecarRes <- try (writeFile pubPathS (T.unpack pub <> "\n") >> chmodPath "0644" pubPath) :: IO (Either SomeException ())
            case sidecarRes of
                Left err ->
                    Sh.print ("hostenv-provider: warning: could not update public key sidecar at " <> pubPath <> ": " <> T.pack (displayException err))
                Right _ -> pure ()
            pure
                SigningKeyInfo
                    { secretKeyPath = keyPath
                    , publicKeyPath = pubPath
                    , publicKey = pub
                    , wasGenerated = False
                    }
        else do
            Dir.createDirectoryIfMissing True keyDirS
            when (keyDirS /= "." && keyDirS /= "") $
                chmodPath "0700" (T.pack keyDirS)
            let keyName = signingKeyNameFor hostenvHostname
            let generateArgs = ["key", "generate-secret", "--key-name", keyName]
            (genCode, genOut, genErr) <- readProcessWithExitCode "nix" (map T.unpack generateArgs) ""
            case genCode of
                ExitFailure _ ->
                    error (T.unpack (localCommandFailure "hostenv-provider: failed to generate signing key" ("nix" : generateArgs) genCode (T.pack genOut) (T.pack genErr)))
                ExitSuccess -> do
                    let secret = T.strip (T.pack genOut)
                    when (T.null secret) $
                        error "hostenv-provider: generated signing key is empty"
                    writeFile keyPathS (T.unpack secret <> "\n")
                    chmodPath "0600" keyPath
                    pub <- convertSecretToPublic secret
                    writeFile pubPathS (T.unpack pub <> "\n")
                    chmodPath "0644" pubPath
                    pure
                        SigningKeyInfo
                            { secretKeyPath = keyPath
                            , publicKeyPath = pubPath
                            , publicKey = pub
                            , wasGenerated = True
                            }

signInstallable :: SigningKeyInfo -> Text -> IO ExitCode
signInstallable keyInfo target = do
    let args =
            [ "store"
            , "sign"
            , "-r"
            , "-k"
            , keyInfo.secretKeyPath
            , "generated/.#" <> target
            ]
    Sh.print ("hostenv-provider: signing " <> target)
    Sh.proc "nix" args Sh.empty

printProviderLine :: Text -> IO ()
printProviderLine line =
    BLC.putStrLn (BLC.pack (T.unpack line))

printTrustedKeySetupHint :: Text -> IO ()
printTrustedKeySetupHint key =
    forM_ (trustedKeySetupLines key) $ \line ->
        printProviderLine ("hostenv-provider: " <> line)

planDeployUser :: KM.KeyMap A.Value -> Text
planDeployUser plan =
    fromMaybe "deploy" (lookupText (K.fromString "deployUser") plan)

planTrustedPublicKeys :: KM.KeyMap A.Value -> [Text]
planTrustedPublicKeys plan =
    case lookupObj (K.fromString "nixSigning") plan of
        Just signingObj -> lookupTextList (K.fromString "trustedPublicKeys") signingObj
        Nothing -> []

planNodeRemoteBuild :: KM.KeyMap A.Value -> KM.KeyMap A.Value
planNodeRemoteBuild plan =
    fromMaybe KM.empty (lookupObj (K.fromString "nodeRemoteBuild") plan)

nodeUsesRemoteBuild :: KM.KeyMap A.Value -> Text -> Bool
nodeUsesRemoteBuild remoteBuildMap nodeName =
    fromMaybe False (lookupBool (K.fromText nodeName) remoteBuildMap)

runNodeDeployPreflight :: (Text -> NodeConnection) -> Text -> Text -> Bool -> Maybe Text -> IO (Maybe Preflight.PreflightFailure)
runNodeDeployPreflight resolveNodeConnection deployUser nodeName requiresTrustedKey signingPublicKey = do
    let target = mkSshTarget deployUser (resolveNodeConnection nodeName)
    Preflight.runPreflight
        Preflight.DeployPreflightConfig
            { nodeName = nodeName
            , deployUser = deployUser
            , requiresTrustedKey = requiresTrustedKey
            , signingPublicKey = signingPublicKey
            , runRemote = runRemoteOutput target
            }

runMigrationBackup :: Text -> (Text -> NodeConnection) -> EnvInfo -> Text -> Text -> IO Text
runMigrationBackup deployUser nodeConnection envInfo prevNode backupName = do
    let sourceTarget = mkSshTarget deployUser (nodeConnection prevNode)
    let sourceHost = sourceTarget.targetHost
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
            state <- fmap parseState (runRemoteStrict sourceTarget (sudoArgs (loadStateCmd (unitFor backupName))))
            if state == "loaded"
                then pure backupName
                else case T.stripSuffix "-migrate" backupName of
                    Just baseName -> do
                        baseState <- fmap parseState (runRemoteStrict sourceTarget (sudoArgs (loadStateCmd (unitFor baseName))))
                        if baseState == "loaded"
                            then do
                                Sh.print
                                    ( "hostenv: migrate backup unit "
                                        <> unitFor backupName
                                        <> " not found on "
                                        <> sourceHost
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
    startExit <- runRemote sourceTarget (sudoArgs startCmd)
    case startExit of
        ExitSuccess -> pure ()
        ExitFailure code -> error ("migration backup failed for " <> T.unpack envInfo.userName <> ":" <> T.unpack backupName <> " (exit " <> show code <> ")")

    let wrapperPath = "~/.local/bin/restic-" <> effectiveName
    let wrapperCheckCmd = "test -x " <> wrapperPath
    wrapperExit <- runRemote sourceTarget (sudoArgs wrapperCheckCmd)
    if wrapperExit == ExitSuccess
        then do
            let snapshotCmdTagged =
                    "set -euo pipefail; "
                        <> wrapperPath
                        <> " snapshots --latest 1 --tag "
                        <> effectiveName
                        <> " --json"
            snapOutTagged <- runRemoteStrict sourceTarget (sudoArgs snapshotCmdTagged)
            case parseSnapshotId snapOutTagged of
                Just snap -> pure snap
                Nothing -> do
                    let snapshotCmdUntagged =
                            "set -euo pipefail; "
                                <> wrapperPath
                                <> " snapshots --latest 1 --json"
                    snapOutUntagged <- runRemoteStrict sourceTarget (sudoArgs snapshotCmdUntagged)
                    case parseSnapshotId snapOutUntagged of
                        Just snap -> do
                            Sh.print
                                ( "hostenv: warning: no tagged restic snapshot found for "
                                    <> effectiveName
                                    <> " on "
                                    <> sourceHost
                                    <> "; using latest untagged snapshot"
                                )
                            pure snap
                        Nothing -> error ("could not parse snapshot id for " <> T.unpack envInfo.userName <> ":" <> T.unpack backupName <> " (tagged or untagged)")
        else do
            Sh.print ("hostenv: restic wrapper missing for " <> effectiveName <> " on " <> sourceHost <> "; reading snapshot id from journal")
            let invocationCmd = userBusPrefix <> "systemctl --user show -p InvocationID --value " <> unit
            invOut <- runRemoteStrict sourceTarget (sudoArgs invocationCmd)
            let invocation = T.strip invOut
            let journalCmd =
                    userBusPrefix
                        <> if T.null invocation
                            then "journalctl --user -u " <> unit <> " -n 200 -o cat --no-pager || true"
                            else "journalctl --user _SYSTEMD_INVOCATION_ID=" <> invocation <> " -o cat --no-pager || true"
            logOut <- runRemoteStrict sourceTarget (sudoArgs journalCmd)
            case parseSnapshotIdFromJournal logOut of
                Just snap -> pure snap
                Nothing -> error ("could not parse snapshot id from journal for " <> T.unpack envInfo.userName <> ":" <> T.unpack backupName)

resolvePrevNode :: (Text -> Maybe Text) -> Text -> EnvInfo -> IO (Maybe Text)
resolvePrevNode explicitSourceFor hostenvHostname envInfo =
    case explicitSourceFor envInfo.userName of
        Just sourceNode -> do
            Sh.print ("hostenv: previous node for " <> envInfo.userName <> " forced via --migration-source: " <> sourceNode)
            pure (Just sourceNode)
        Nothing -> do
            discovered <- discoverPrevNodeFromDns hostenvHostname envInfo.userName (envInfo.vhosts)
            case discovered of
                Just node -> do
                    Sh.print ("hostenv: previous node for " <> envInfo.userName <> " discovered via DNS: " <> node)
                    pure (Just node)
                Nothing -> pure Nothing

writeRestorePlan :: Text -> (Text -> NodeConnection) -> EnvInfo -> Text -> [(Text, Text)] -> IO ()
writeRestorePlan deployUser nodeConnection envInfo prevNode snapshots = do
    let deployTarget = mkSshTarget deployUser (nodeConnection envInfo.node)
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
            deployTarget
            [ "bash"
            , "-lc"
            , dirCmd
            ]
    case dirExit of
        ExitSuccess -> pure ()
        ExitFailure code -> error ("failed to create restore dir on " <> T.unpack deployTarget.targetUserHost <> " (exit " <> show code <> ")")

    fileExit <- runRemote deployTarget ["bash", "-lc", "sudo install -m 0600 /dev/null " <> restorePath <> " && sudo chown " <> ownerSpec <> ":" <> groupSpec <> " " <> restorePath]
    case fileExit of
        ExitSuccess -> pure ()
        ExitFailure code -> error ("failed to create restore plan file on " <> T.unpack deployTarget.targetUserHost <> " (exit " <> show code <> ")")

    let payloadText = TE.decodeUtf8 (BL.toStrict (A.encode payload))
    let payloadTag = pickTag "HOSTENV_JSON" payloadText
    let writeCmd =
            T.unlines
                [ "sudo tee " <> shellEscape restorePath <> " >/dev/null <<'" <> payloadTag <> "'"
                , payloadText
                , payloadTag
                ]
    writeExit <- runRemote deployTarget ["bash", "-lc", writeCmd]
    case writeExit of
        ExitSuccess -> pure ()
        ExitFailure code -> error ("failed to write restore plan for " <> T.unpack envInfo.userName <> " (exit " <> show code <> ")")

runDeploy :: Maybe Text -> Maybe Text -> Bool -> [Text] -> [Text] -> Bool -> IO ()
runDeploy mNode mSigningKeyPath forceRemoteBuild skipMigrations migrationSourceSpecs ignoreMigrationErrors = do
    let planPath = "generated/plan.json"
    planExists <- Sh.testfile (fromString (T.unpack planPath))
    when (not planExists) $ do
        BLC.putStrLn "plan.json not found; deploy aborted"
        Sh.exit (ExitFailure 1)

    raw <- BL.readFile (T.unpack planPath)
    case A.eitherDecode' raw of
        Left err -> error err
        Right (plan :: KM.KeyMap A.Value) -> do
            let hostenvHostname = case lookupText (K.fromString "hostenvHostname") plan of
                    Nothing -> error "hostenvHostname missing from plan.json"
                    Just hostname -> hostname
            let resolveNodeConnection = nodeConnectionFor plan hostenvHostname
            let envs = fromMaybe KM.empty (lookupObj (K.fromString "environments") plan)
            migrationSources <-
                forM migrationSourceSpecs $ \raw ->
                    case parseMigrationSource raw of
                        Just parsed -> pure parsed
                        Nothing -> error ("invalid --migration-source value '" <> T.unpack raw <> "' (expected ENV=NODE)")
            let duplicateOverrides = duplicateValues (map fst migrationSources)
            unless (null duplicateOverrides) $
                error ("duplicate --migration-source overrides for: " <> T.unpack (T.intercalate ", " duplicateOverrides))

            -- Gets the @EnvInfo@s from JSON (taken from plan.json).
            envInfosSub <- forM (KM.elems envs) $ \env -> case iparseEither parseJSON env of
                -- Using lists here (so the type becomes @[[EnvInfo]]@) as
                -- they're easy to @concat@ later.
                Left err -> Sh.print ("hostenv-provider: error reading from plan JSON at '" <> T.pack (show $ fst err) <> " - '" <> T.pack (snd err) <> "'") >> pure []
                Right env_ -> pure [env_]

            -- Filter environments to node requested by the user.
            let envInfosFiltered =
                    case mNode of
                        Nothing -> concat envInfosSub
                        Just n -> filter (\e -> e.node == n) (concat envInfosSub)

            let skipHits =
                    filter
                        (\s -> any (\e -> e.userName == s) envInfosFiltered)
                        skipMigrations
            let skipMisses = skipMigrations \\ skipHits
            let sourceHits = filter (\(envName, _) -> any (\e -> e.userName == envName) envInfosFiltered) migrationSources
            let sourceMisses = map fst migrationSources \\ map fst sourceHits
            let sourceFor envName = lookup envName migrationSources

            -- Perform data migrations if the environment has moved node.
            case mNode of
                Nothing -> pure ()
                Just n -> Sh.print ("hostenv-provider: deploy filtered to node " <> n)
            Sh.print ("hostenv-provider: " <> T.pack (show (length envInfosFiltered)) <> " environment(s) considered")
            when (not (null skipHits)) $
                Sh.print ("hostenv-provider: skipping migrations for: " <> T.intercalate ", " skipHits)
            when (not (null skipMisses)) $
                Sh.print ("hostenv-provider: warning: skip-migrations targets not found: " <> T.intercalate ", " skipMisses)
            forM_ sourceHits $ \(envName, sourceNode) ->
                Sh.print ("hostenv-provider: migration source override " <> envName <> " -> " <> sourceNode)
            when (not (null sourceMisses)) $
                Sh.print ("hostenv-provider: warning: migration-source targets not found: " <> T.intercalate ", " sourceMisses)
            when ignoreMigrationErrors $
                Sh.print "hostenv-provider: warning: migration errors will be ignored; deployments may proceed with stale data"

            let targetNodes = uniqueNodeNames envInfosFiltered
            let deployUser = planDeployUser plan
            let remoteBuildMap = planNodeRemoteBuild plan
            let usesRemoteBuild nodeName = forceRemoteBuild || nodeUsesRemoteBuild remoteBuildMap nodeName
            let requiresLocalPush nodeName = not (usesRemoteBuild nodeName)
            let nodesRequiringLocalPush = filter requiresLocalPush targetNodes
            let trustedPublicKeys = planTrustedPublicKeys plan
            let localPushSet = S.fromList nodesRequiringLocalPush

            signingKeyInfo <-
                if null nodesRequiringLocalPush
                    then pure Nothing
                    else do
                        keyInfo <- ensureSigningKeyInfo hostenvHostname mSigningKeyPath
                        when keyInfo.wasGenerated $ do
                            Sh.print ("hostenv-provider: generated signing key at " <> keyInfo.secretKeyPath)
                            Sh.print ("hostenv-provider: wrote corresponding public key to " <> keyInfo.publicKeyPath)
                        pure (Just keyInfo)

            forM_ signingKeyInfo $ \keyInfo -> do
                when (null trustedPublicKeys) $ do
                    Sh.print "hostenv-provider: deployment aborted; provider.nixSigning.trustedPublicKeys is empty in generated/plan.json"
                    printTrustedKeySetupHint keyInfo.publicKey
                    Sh.exitWith (ExitFailure 1)
                when (keyInfo.publicKey `notElem` trustedPublicKeys) $ do
                    Sh.print "hostenv-provider: deployment aborted; local signing key is not present in provider.nixSigning.trustedPublicKeys"
                    Sh.print ("hostenv-provider: signing key public value: " <> keyInfo.publicKey)
                    printTrustedKeySetupHint keyInfo.publicKey
                    Sh.print "hostenv-provider: or pass --signing-key-file with a key that already matches trustedPublicKeys."
                    Sh.exitWith (ExitFailure 1)

            when forceRemoteBuild $
                Sh.print "hostenv-provider: --remote-build enabled; skipping local signing trust checks for all nodes"
            forM_ targetNodes $ \nodeName ->
                when (not forceRemoteBuild && not (S.member nodeName localPushSet)) $
                    Sh.print ("hostenv-provider: node " <> nodeName <> " uses remoteBuild=true; skipping local signing trust check")

            preflightFailures <- fmap catMaybes $
                forM targetNodes $ \nodeName -> do
                    let requiresTrustedKey = S.member nodeName localPushSet
                    runNodeDeployPreflight
                        resolveNodeConnection
                        deployUser
                        nodeName
                        requiresTrustedKey
                        ((.publicKey) <$> signingKeyInfo)

            when (not (null preflightFailures)) $ do
                forM_ preflightFailures $ \failure -> do
                    Sh.print ("hostenv-provider: deploy preflight failed on node " <> failure.failureNode <> ": " <> failure.failureReason)
                    forM_ failure.failureRemediation $ \line ->
                        printProviderLine ("hostenv-provider: remediation for " <> failure.failureNode <> ": " <> line)
                Sh.print "hostenv-provider: aborting deployment because one or more preflight checks failed"
                Sh.exitWith (ExitFailure 1)

            migrations <- fmap catMaybes $
                forM envInfosFiltered $ \envInfo -> do
                    if envInfo.userName `elem` skipMigrations || envInfo.migrateBackups == []
                        then pure Nothing
                        else do
                            prevNode <- resolvePrevNode sourceFor hostenvHostname envInfo
                            pure $ case prevNode of
                                Just prev | prev /= envInfo.node -> Just (envInfo, prev)
                                _ -> Nothing

            if null migrations
                then Sh.print "hostenv-provider: no migrations required"
                else do
                    Sh.print ("hostenv-provider: migrations required for " <> T.pack (show (length migrations)) <> " environment(s)")
                    forM_ migrations $ \(envInfo, prevNode) -> do
                        let backups = T.intercalate ", " envInfo.migrateBackups
                        Sh.print ("hostenv-provider: migrate backups for " <> envInfo.userName <> " from " <> prevNode <> " -> " <> envInfo.node <> " (" <> backups <> ")")
                    let runMigration (envInfo, prevNode) = do
                            snapshots <- forM envInfo.migrateBackups $ \backupName -> do
                                snap <- runMigrationBackup deployUser resolveNodeConnection envInfo prevNode backupName
                                pure (backupName, snap)
                            writeRestorePlan deployUser resolveNodeConnection envInfo prevNode snapshots
                            let snapshotPairs = map (\(name, sid) -> name <> "=" <> sid) snapshots
                            Sh.print ("hostenv-provider: restore plan written for " <> envInfo.userName <> " (snapshots: " <> T.intercalate ", " snapshotPairs <> ")")
                    if ignoreMigrationErrors
                        then do
                            failures <- fmap catMaybes $
                                forM migrations $ \(envInfo, prevNode) -> do
                                    res <- try (runMigration (envInfo, prevNode)) :: IO (Either SomeException ())
                                    case res of
                                        Right _ -> pure Nothing
                                        Left err -> do
                                            Sh.print ("hostenv-provider: warning: migration failed for " <> envInfo.userName <> ": " <> T.pack (displayException err))
                                            pure (Just envInfo.userName)
                            when (not (null failures)) $
                                Sh.print ("hostenv-provider: warning: ignored migration failures for " <> T.intercalate ", " failures)
                        else
                            forM_ migrations runMigration

            -- Perform deployment
            --
            -- Start with the system deployment, since deploy-rs tends to
            -- fail deployments where this is not run separately (if there are
            -- new user accounts/environments added to the server).

            -- `--skip-checks` used here as deploy-rs checks fail when the
            -- local and remote architectures differ. For example: when on an
            -- x86 machine deploying to an ARM server.
            -- @todo: we could add a `--skip-checks` parameter to this CLI
            -- and pass it through to deploy-rs.
            let deployArgs deployArgM =
                    [ "run"
                    , "nixpkgs#deploy-rs"
                    -- Thinking of adding `--inputs-from` here?
                    -- We have an organisation that starts with a number and
                    -- `--inputs-from` returns an error if we use it. In the
                    -- flake.nix's inputs we can just wrap it in quotes, but
                    -- those don't carry over when using this parameter.
                    , "--"
                    , "--skip-checks"
                    , "--checksigs"
                    ]
                        <> if forceRemoteBuild then ["--remote-build"] else []
                        <> case deployArgM of
                            Just dArg -> ["generated/.#" <> dArg]
                            Nothing -> ["generated/"]
            let deployGuard res args = when (res /= ExitSuccess) $ Sh.print ("hostenv-provider: deploying " <> T.unwords args <> "failed")

            -- System deployment of each node. Retains results, so we can
            -- cowardly refuse to deploy environments where the system deploy
            -- failed to complete.
            systemDeployRes <-
                forM targetNodes $ \nodeName -> do
                    let target = nodeName <> ".system"
                    signRes <-
                        if S.member nodeName localPushSet
                            then case signingKeyInfo of
                                Just keyInfo -> signInstallable keyInfo target
                                Nothing -> do
                                    Sh.print ("hostenv-provider: signing configuration missing for node " <> nodeName)
                                    pure (ExitFailure 1)
                            else pure ExitSuccess
                    if signRes /= ExitSuccess
                        then do
                            Sh.print ("hostenv-provider: failed to sign deployment target " <> target)
                            pure (nodeName, signRes)
                        else do
                            let args = deployArgs (Just target)
                            Sh.print ("hostenv-provider: running nix " <> T.unwords args)
                            res <- Sh.proc "nix" args Sh.empty
                            deployGuard res args
                            pure (nodeName, res)

            -- Environments deployment for the node.
            envDeployRes <- forM (toNamed envInfosFiltered) $ \(envName, envInfo) -> do
                let target = envInfo.node <> "." <> envInfo.userName
                let args = deployArgs (Just target)
                let systemStatus = case filter ((==) envInfo.node . fst) systemDeployRes of
                        [] -> Left "node not in system deployment list"
                        (_, ExitSuccess) : [] -> Right ExitSuccess
                        (_, code) : [] -> Left ("system deployment failed for node " <> envInfo.node <> " (exit " <> T.pack (show code) <> ")")
                        _moreThanOneNode -> Left "multiple matching nodes found in system deployment list"

                Sh.print ("hostenv-provider: running nix " <> T.unwords args)
                case systemStatus of
                    Left err -> Sh.print ("hostenv-provider: " <> err) >> pure (envName, ExitFailure 1)
                    Right ExitSuccess -> do
                        signRes <-
                            if S.member envInfo.node localPushSet
                                then case signingKeyInfo of
                                    Just keyInfo -> signInstallable keyInfo target
                                    Nothing -> do
                                        Sh.print ("hostenv-provider: signing configuration missing for node " <> envInfo.node)
                                        pure (ExitFailure 1)
                                else pure ExitSuccess
                        if signRes /= ExitSuccess
                            then do
                                Sh.print ("hostenv-provider: failed to sign deployment target " <> target)
                                pure (envName, signRes)
                            else do
                                res <- Sh.proc "nix" args Sh.empty
                                deployGuard res args
                                pure (envName, res)

            let failures = any (\(_, code) -> code /= ExitSuccess) envDeployRes
            when failures $ Sh.print "hostenv-provider: deployment completed with errors"
            Sh.exitWith $ if failures then ExitFailure 1 else ExitSuccess

-- -------- Main --------
main :: IO ()
main = do
    CLI cmd <- OA.execParser cliOpts
    case cmd of
        CmdPlan -> runPlan
        CmdDnsGate mNode mTok mZone withDnsUpdate -> runDnsGate mNode mTok mZone withDnsUpdate
        CmdDeploy mNode mSigningKeyPath forceRemoteBuild skipMigrations migrationSources ignoreMigrationErrors ->
            runDeploy mNode mSigningKeyPath forceRemoteBuild skipMigrations migrationSources ignoreMigrationErrors
