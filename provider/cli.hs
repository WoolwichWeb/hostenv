#!/usr/bin/env -S runghc
{-# LANGUAGE OverloadedStrings #-}

-- hostenv-provider CLI: plan | dns-gate | deploy
-- dns-gate ports the legacy scripts/postgen.hs DNS/ACME gate and Cloudflare upsert logic.

import Control.Monad (forM_, when)
import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Conversions (convertText)
import Data.Text.Encoding qualified as TE
import Distribution.Compat.Prelude qualified as Sh
import Options.Applicative qualified as OA
import System.Environment qualified as Env
import System.Exit (ExitCode (..))
import Turtle (FilePath, (<|>))
import Turtle qualified as Sh
import Prelude hiding (FilePath)

-- -------- CLI --------
data Command = CmdPlan | CmdDnsGate {cNode :: Maybe Text, cWrite :: Bool, cToken :: Maybe Text, cZone :: Maybe Text} | CmdDeploy {cNode :: Maybe Text}

data CLI = CLI {cliDest :: Text, cliCmd :: Command}

destOpt :: OA.Parser Text
destOpt =
    T.pack
        <$> OA.strOption
            ( OA.long "dest"
                <> OA.metavar "DIR"
                <> OA.value "generated"
                <> OA.showDefault
                <> OA.help "Output directory for generated plan/state/flake"
            )

nodeOpt :: OA.Parser (Maybe Text)
nodeOpt =
    OA.optional . fmap T.pack $
        OA.strOption
            ( OA.long "node"
                <> OA.short 'n'
                <> OA.metavar "NODE"
                <> OA.help "Restrict to a specific node"
            )

writeOpt :: OA.Parser Bool
writeOpt =
    OA.switch
        ( OA.long "write"
            <> OA.help "Write updated plan.json to DEST (otherwise prints to stdout)"
        )

tokenOpt :: OA.Parser (Maybe Text)
tokenOpt = OA.optional . fmap T.pack $ OA.strOption (OA.long "cf-token" <> OA.metavar "TOKEN" <> OA.help "Cloudflare API token (overrides CF_API_TOKEN env)")

zoneOpt :: OA.Parser (Maybe Text)
zoneOpt = OA.optional . fmap T.pack $ OA.strOption (OA.long "cf-zone" <> OA.metavar "ZONE" <> OA.help "Cloudflare zone id (overrides CF_ZONE_ID env)")

cliParser :: OA.Parser CLI
cliParser =
    CLI
        <$> destOpt
        <*> OA.hsubparser
            ( OA.command "plan" (OA.info (pure CmdPlan) (OA.progDesc "Generate plan/state/flake"))
                <> OA.command "dns-gate" (OA.info (CmdDnsGate <$> nodeOpt <*> writeOpt <*> tokenOpt <*> zoneOpt) (OA.progDesc "Disable ACME/forceSSL for vhosts not pointing at node; optional Cloudflare upsert"))
                <> OA.command "deploy" (OA.info (CmdDeploy <$> nodeOpt) (OA.progDesc "Deploy via deploy-rs"))
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

-- -------- JSON helpers --------
lookupText :: KM.Key -> KM.KeyMap A.Value -> Maybe Text
lookupText k o = case KM.lookup k o of
    Just (A.String t) -> Just t
    _ -> Nothing

lookupObj :: KM.Key -> KM.KeyMap A.Value -> Maybe (KM.KeyMap A.Value)
lookupObj k o = case KM.lookup k o of
    Just (A.Object x) -> Just x
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
disableAcmePaths envName vhostName root =
    let pEnvEnable = ["environments", envName, "virtualHosts", vhostName, "enableACME"]
        pEnvSSL = ["environments", envName, "virtualHosts", vhostName, "forceSSL"]
     in setBoolAt pEnvSSL False (setBoolAt pEnvEnable False root)

disableAcmeOnNode :: Text -> Text -> KM.KeyMap A.Value -> KM.KeyMap A.Value
disableAcmeOnNode nodeName vhostName root =
    let pNodeEnable = ["nodes", nodeName, "services", "nginx", "virtualHosts", vhostName, "enableACME"]
        pNodeSSL = ["nodes", nodeName, "services", "nginx", "virtualHosts", vhostName, "forceSSL"]
     in setBoolAt pNodeSSL False (setBoolAt pNodeEnable False root)

-- -------- Cloudflare helpers --------
data CFRecord = CFRecord
    { rId :: Text
    , rType :: Text
    , rName :: Text
    , rContent :: Text
    , rProxied :: Maybe Bool
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
        Right lst -> if lSuccess lst then pure (lResult lst) else pure []

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
    case filter ((== "CNAME") . rType) existing of
        (c : _) -> do
            let url = "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records/" <> rId c
            let body = "{\"type\":\"CNAME\",\"name\":\"" <> name <> "\",\"content\":\"" <> target <> "\",\"proxied\":false}"
            Sh.stdout $ Sh.inproc "curl" ["-sS", "-X", "PUT", "-H", "Authorization: Bearer " <> token, "-H", "Content-Type: application/json", "--data", body, url] Sh.empty
        [] -> do
            let url = "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records"
            let body = "{\"type\":\"CNAME\",\"name\":\"" <> name <> "\",\"content\":\"" <> target <> "\",\"proxied\":false}"
            Sh.stdout $ Sh.inproc "curl" ["-sS", "-X", "POST", "-H", "Authorization: Bearer " <> token, "-H", "Content-Type: application/json", "--data", body, url] Sh.empty

-- -------- DNS gate --------
runDnsGate :: Text -> Bool -> Maybe Text -> Maybe Text -> Maybe Text -> IO ()
runDnsGate dest writeOut mNode mTok mZone = do
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
            let hasCF = isJustPair cfTok cfZone
            plan' <- foldlM (processEnv hostenvHostname nodes hasCF cfTok cfZone) plan (KM.toList envs)
            if writeOut
                then do
                    let tmp = dest <> "/plan.json"
                    BL.writeFile (T.unpack tmp) (A.encode plan')
                    BLC.putStrLn "✅ dns-gate updated plan.json"
                else BLC.putStrLn (A.encode plan')
  where
    isJustPair (Just _) (Just _) = True
    isJustPair _ _ = False
    readFileTrim p = T.unpack . T.strip . T.pack <$> readFile p
    foldlM f z [] = pure z
    foldlM f z (x : xs) = f z x >>= \z' -> foldlM f z' xs
    processEnv hostenvHostname nodes hasCF cfTok cfZone acc (kEnv, vEnv) =
        case vEnv of
            A.Object envObj -> do
                let vhosts = fromMaybe KM.empty (lookupObj (K.fromString "virtualHosts") envObj)
                foldlM (processVhost hostenvHostname nodes hasCF cfTok cfZone (K.toText kEnv)) acc (KM.toList vhosts)
            _ -> pure acc
    processVhost hostenvHostname nodes hasCF cfTok cfZone envName planAcc (vhKey, _vhObj) = do
        let vhName = K.toText vhKey
        let envNode = case lookupObj (K.fromString "environments") planAcc >>= KM.lookup (K.fromText envName) of
                Just (A.Object o) -> lookupText (K.fromString "node") o
                _ -> Nothing
        let nodeName = fromMaybe "" (envNode <|> mNode)
        let expectedHost = if nodeName == "" then vhName else nodeName <> "." <> hostenvHostname
        ok <- dnsPointsTo vhName expectedHost
        planAcc' <-
            if ok
                then pure planAcc
                else do
                    when hasCF $
                        case (cfTok, cfZone) of
                            (Just t, Just z) -> cfUpsertCname (T.pack t) (T.pack z) vhName expectedHost >> pure ()
                            (Nothing, _) -> Sh.print "DNS setup ('dnsGate') failed: Cloudflare token was not provided"
                            (_, Nothing) -> Sh.print "DNS setup ('dnsGate') failed: Cloudflare zone was not provided"
                    let plan1 = disableAcmePaths envName vhName planAcc
                    let plan2 = disableAcmeOnNode nodeName vhName plan1
                    pure plan2
        pure planAcc'

-- -------- Plan (copy from provider package) --------
runPlan :: Text -> IO ()
runPlan dest = do
    Sh.export "HOSTENV_PROVIDER_OUT" dest
    _ <- Sh.procs "hostenv-provider-plan" [] Sh.empty
    pure ()

-- -------- Deploy wrapper --------
runDeploy :: Maybe Text -> IO ()
runDeploy mNode = do
    let nodeArg = maybe [] (\n -> ["-s", n]) mNode
    Sh.exit =<< Sh.proc "nix" (["run", "github:serokell/deploy-rs", "--", "--remote-build", ".#"] <> nodeArg) Sh.empty

-- -------- Main --------
main :: IO ()
main = do
    CLI dest cmd <- OA.execParser cliOpts
    case cmd of
        CmdPlan -> runPlan dest
        CmdDnsGate mNode writeOut mTok mZone -> runDnsGate dest writeOut mNode mTok mZone
        CmdDeploy mNode -> runDeploy mNode
