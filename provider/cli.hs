{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- hostenv-provider CLI: plan | dns-gate | comin-tokens
-- dns-gate ports the legacy scripts/postgen.hs DNS/ACME gate and Cloudflare upsert logic.

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, finally, try)
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
import Data.List (intersect, nub, (\\))
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Conversions (convertText)
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Distribution.Compat.Prelude qualified as Sh
import Hostenv.Provider.DnsBackoff (backoffDelays)
import Hostenv.Provider.DnsGateFilter (filterEnvironmentsByNode)
import Hostenv.Provider.PrevNodeDiscovery qualified as PrevNode
import Options.Applicative qualified as OA
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.Exit (ExitCode (..))
import System.FilePath qualified as FP
import System.IO (hClose, openTempFile, stderr)
import System.Process (readProcessWithExitCode)
import Turtle (FilePath, (<|>))
import Turtle qualified as Sh
import Prelude hiding (FilePath)

-- -------- CLI --------
data Command
    = CmdPlan {dryRun :: Bool}
    | CmdDnsGate {node :: Maybe Text, token :: Maybe Text, zone :: Maybe Text, withDnsUpdate :: Bool, dryRun :: Bool}
    | CmdCominTokens {dryRun :: Bool}

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

dryRunOpt :: OA.Parser Bool
dryRunOpt =
    OA.switch
        ( OA.long "dry-run"
            <> OA.help "Preview changes without mutating generated files or remote state"
        )


cliParser :: OA.Parser CLI
cliParser =
    CLI
        <$> OA.hsubparser
            ( OA.command "plan" (OA.info (CmdPlan <$> dryRunOpt) (OA.progDesc "Generate plan.json, state.json, and flake.nix"))
                <> OA.command "dns-gate" (OA.info (CmdDnsGate <$> nodeOpt <*> tokenOpt <*> zoneOpt <*> withDnsUpdateOpt <*> dryRunOpt) (OA.progDesc "Disable ACME/forceSSL for vhosts not pointing at node; add --with-dns-update to upsert Cloudflare records"))
                <> OA.command "comin-tokens" (OA.info (CmdCominTokens <$> dryRunOpt) (OA.progDesc "Generate missing comin node tokens in provider secrets"))
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

digRRRaw :: Maybe Text -> Text -> Text -> IO [Text]
digRRRaw mNameserver name rr = do
    let serverArgs = maybe [] (\nameserver -> ["@" <> T.unpack nameserver]) mNameserver
    let args = serverArgs <> ["+short", T.unpack name, T.unpack rr]
    (code, out, _err) <- readProcessWithExitCode "dig" args ""
    case code of
        ExitFailure _ -> pure []
        ExitSuccess ->
            pure $
                filter (not . T.null) $
                    map (stripDot . T.toLower . T.strip . T.pack) $
                        lines out

zoneCandidates :: Text -> [Text]
zoneCandidates name =
    let labels = filter (not . T.null) (T.splitOn "." (T.toLower (stripDot name)))
        labelCount = length labels
        indices =
            if labelCount < 2
                then []
                else [0 .. labelCount - 2]
     in map (\idx -> T.intercalate "." (drop idx labels)) indices

findAuthoritativeNameservers :: Text -> IO [Text]
findAuthoritativeNameservers name = go (zoneCandidates name)
  where
    go [] = pure []
    go (candidate : rest) = do
        nameservers <- digRRRaw Nothing candidate "NS"
        if null nameservers
            then go rest
            else pure nameservers

digRR :: Text -> Text -> IO [Text]
digRR name rr = do
    authoritativeNameservers <- findAuthoritativeNameservers name
    case authoritativeNameservers of
        [] -> digRRRaw Nothing name rr
        nameservers -> do
            answersByNameserver <- forM nameservers (\nameserver -> digRRRaw (Just nameserver) name rr)
            let nonEmptyAnswers = filter (not . null) answersByNameserver
            if null nonEmptyAnswers
                then digRRRaw Nothing name rr
                else pure (S.toList (S.fromList (concat nonEmptyAnswers)))

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

planNodeNames :: KM.KeyMap A.Value -> [Text]
planNodeNames plan =
    case lookupObj (K.fromString "nodes") plan of
        Nothing -> []
        Just nodesObj -> map (K.toText . fst) (KM.toList nodesObj)

planCominEnabled :: KM.KeyMap A.Value -> Bool
planCominEnabled plan =
    case lookupObj (K.fromString "comin") plan >>= lookupBool (K.fromString "enable") of
        Just enabled -> enabled
        Nothing -> False

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

disableAcmeOnNode :: Maybe Text -> Text -> KM.KeyMap A.Value -> KM.KeyMap A.Value
disableAcmeOnNode Nothing _ root = root
disableAcmeOnNode (Just nodeName) vhostName root =
    let pNodeEnable = ["nodes", nodeName, "services", "nginx", "virtualHosts", vhostName, "enableACME"]
        pNodeSSL = ["nodes", nodeName, "services", "nginx", "virtualHosts", vhostName, "forceSSL"]
     in setBoolAt pNodeSSL False (setBoolAt pNodeEnable False root)

prettyJson :: BL.ByteString -> IO BL.ByteString
prettyJson raw = do
    (code, out, err) <- readProcessWithExitCode "jq" ["-S", "."] (T.unpack (TE.decodeUtf8 (BL.toStrict raw)))
    case code of
        ExitSuccess -> pure (BL.fromStrict (TE.encodeUtf8 (T.pack out)))
        ExitFailure c ->
            error ("jq failed while formatting JSON (exit " <> show c <> "): " <> err)

withTempBytesFile :: String -> BL.ByteString -> (FilePath -> IO a) -> IO a
withTempBytesFile template content action = do
    tmpDir <- Dir.getTemporaryDirectory
    (tmpPath, handle) <- openTempFile tmpDir template
    BL.hPut handle content
    hClose handle
    action tmpPath
        `finally` do
            _ <- try (Dir.removeFile tmpPath) :: IO (Either SomeException ())
            pure ()

printUnifiedDiff :: Text -> BL.ByteString -> BL.ByteString -> IO Bool
printUnifiedDiff path oldContent newContent =
    withTempBytesFile "hostenv-provider-old-XXXXXX.json" oldContent $ \oldPath ->
        withTempBytesFile "hostenv-provider-new-XXXXXX.json" newContent $ \newPath -> do
            let oldLabel = T.unpack (path <> " (current)")
            let newLabel = T.unpack (path <> " (proposed)")
            (code, out, err) <- readProcessWithExitCode "diff" ["-u", "--label", oldLabel, "--label", newLabel, oldPath, newPath] ""
            case code of
                ExitSuccess -> pure False
                ExitFailure 1 -> do
                    BLC.putStr (BLC.pack out)
                    pure True
                ExitFailure c -> do
                    printProviderErr ("hostenv-provider: warning: failed to render diff for " <> path <> " (exit " <> T.pack (show c) <> ")")
                    when (not (null err)) $
                        printProviderErr ("hostenv-provider: diff stderr: " <> T.pack err)
                    pure True

printDiffAgainstFile :: Text -> BL.ByteString -> IO Bool
printDiffAgainstFile path proposedPretty = do
    pathExists <- Dir.doesFileExist (T.unpack path)
    if not pathExists
        then do
            printProvider ("hostenv-provider: dry-run: " <> path <> " does not exist; generated content would be:")
            BLC.putStr proposedPretty
            pure True
        else do
            currentRaw <- BL.readFile (T.unpack path)
            currentPretty <- prettyJson currentRaw
            if currentPretty == proposedPretty
                then do
                    printProvider ("hostenv-provider: dry-run: no changes for " <> path)
                    pure False
                else printUnifiedDiff path currentPretty proposedPretty

printProvider :: Text -> IO ()
printProvider = TIO.putStrLn

printProviderErr :: Text -> IO ()
printProviderErr = TIO.hPutStrLn stderr

data SecretsConfig = SecretsConfig
    { secretsEnabled :: Bool
    , secretsFilePath :: Maybe Text
    , secretsKeys :: [Text]
    }

instance A.FromJSON SecretsConfig where
    parseJSON = A.withObject "SecretsConfig" $ \o ->
        SecretsConfig
            <$> o .:? "enable" .!= False
            <*> o .:? "file"
            <*> o .:? "keys" .!= []

defaultSecretsConfig :: SecretsConfig
defaultSecretsConfig = SecretsConfig False Nothing []

data EnvSecretsConfig = EnvSecretsConfig
    { userName :: Text
    , organisation :: Text
    , project :: Text
    , root :: Text
    , safeEnvironmentName :: Text
    , projectSecretsCfg :: SecretsConfig
    , environmentSecretsCfg :: SecretsConfig
    }

instance A.FromJSON EnvSecretsConfig where
    parseJSON = A.withObject "EnvSecretsConfig" $ \o -> do
        hostenvObj <- o .: "hostenv"
        userName <- hostenvObj .: "userName"
        organisation <- hostenvObj .: "organisation"
        project <- hostenvObj .: "project"
        root <- hostenvObj .: "root"
        environmentName <- hostenvObj .:? "environmentName" .!= userName
        safeEnvironmentName <- hostenvObj .:? "safeEnvironmentName"
        projectScope <- hostenvObj .:? "projectSecrets" .!= defaultSecretsConfig
        environmentScope <- o .:? "secrets" .!= defaultSecretsConfig
        let safeName = fromMaybe environmentName safeEnvironmentName
        pure
            EnvSecretsConfig
                { userName = userName
                , organisation = organisation
                , project = project
                , root = root
                , safeEnvironmentName = safeName
                , projectSecretsCfg = projectScope
                , environmentSecretsCfg = environmentScope
                }

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

data CFError = CFError
    { cfErrorCode :: Maybe Int
    , cfErrorMessage :: Text
    }

instance A.FromJSON CFError where
    parseJSON = A.withObject "CFError" $ \v ->
        CFError
            <$> v .:? "code"
            <*> v .:? "message" .!= "unknown error"

data CFWrite = CFWrite
    { wSuccess :: Bool
    , wErrors :: [CFError]
    }

instance A.FromJSON CFWrite where
    parseJSON = A.withObject "CFWrite" $ \v ->
        CFWrite
            <$> v .: "success"
            <*> v .:? "errors" .!= []

formatCFError :: CFError -> Text
formatCFError cfErr =
    case cfErr.cfErrorCode of
        Just code -> "[" <> T.pack (show code) <> "] " <> cfErr.cfErrorMessage
        Nothing -> cfErr.cfErrorMessage

cfWriteFailureReason :: CFWrite -> Text
cfWriteFailureReason resp =
    if null resp.wErrors
        then "Cloudflare API reported success=false with no error details"
        else "Cloudflare API reported success=false: " <> T.intercalate "; " (map formatCFError resp.wErrors)

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

cfWriteCall :: [String] -> IO (Either Text ())
cfWriteCall curlArgs = do
    (code, out, err) <- readProcessWithExitCode "curl" curlArgs ""
    case code of
        ExitFailure c ->
            pure (Left ("curl failed with exit " <> T.pack (show c) <> ": " <> T.pack err))
        ExitSuccess ->
            let outBs = BL.fromStrict (TE.encodeUtf8 (T.pack out))
             in case A.eitherDecode' outBs of
                    Left decodeErr ->
                        pure (Left ("invalid Cloudflare response: " <> T.pack decodeErr))
                    Right (resp :: CFWrite) ->
                        if resp.wSuccess
                            then pure (Right ())
                            else pure (Left (cfWriteFailureReason resp))

cfDeleteRecord :: Text -> Text -> Text -> IO (Either Text ())
cfDeleteRecord token zoneId rid = do
    let url = "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records/" <> rid
    let curlArgs =
            [ "-sS"
            , "-X"
            , "DELETE"
            , "-H"
            , "Authorization: Bearer " <> T.unpack token
            , "-H"
            , "Content-Type: application/json"
            , T.unpack url
            ]
    cfWriteCall curlArgs

cfUpsertCname :: Text -> Text -> Text -> Text -> IO (Either Text ())
cfUpsertCname token zoneId name target = do
    existingRes <- try (cfListByName token zoneId name) :: IO (Either SomeException [CFRecord])
    existing <- case existingRes of
        Left err ->
            pure (Left ("failed to list existing DNS records: " <> T.pack (displayException err)))
        Right rows ->
            pure (Right rows)
    case existing of
        Left err -> pure (Left err)
        Right rows -> do
            let isConflictType rec = rec.rType `elem` ["A", "AAAA", "CNAME"]
            let conflicts = filter isConflictType rows
            let mAnchor =
                    case filter ((== "CNAME") . (.rType)) conflicts of
                        (c : _) -> Just c
                        [] ->
                            case conflicts of
                                (rec : _) -> Just rec
                                [] -> Nothing
            let staleConflicts =
                    case mAnchor of
                        Just anchor -> filter (\rec -> rec.id /= anchor.id) conflicts
                        Nothing -> []
            deleteResult <- foldlM deleteConflict (Right ()) staleConflicts
            case deleteResult of
                Left err ->
                    pure (Left err)
                Right () -> do
                    let (method, url) =
                            case mAnchor of
                                Just anchor ->
                                    ("PUT", "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records/" <> anchor.id)
                                Nothing ->
                                    ("POST", "https://api.cloudflare.com/client/v4/zones/" <> zoneId <> "/dns_records")
                    let body = "{\"type\":\"CNAME\",\"name\":\"" <> name <> "\",\"content\":\"" <> target <> "\",\"proxied\":false}"
                    let curlArgs =
                            [ "-sS"
                            , "-X"
                            , T.unpack method
                            , "-H"
                            , "Authorization: Bearer " <> T.unpack token
                            , "-H"
                            , "Content-Type: application/json"
                            , "--data"
                            , T.unpack body
                            , T.unpack url
                            ]
                    cfWriteCall curlArgs
  where
    foldlM f z [] = pure z
    foldlM f z (x : xs) = f z x >>= \z' -> foldlM f z' xs
    deleteConflict acc rec =
        case acc of
            Left err -> pure (Left err)
            Right () -> do
                deleteResp <- cfDeleteRecord token zoneId rec.id
                case deleteResp of
                    Left err ->
                        pure
                            ( Left
                                ( "failed to delete conflicting "
                                    <> rec.rType
                                    <> " record (id "
                                    <> rec.id
                                    <> ") before CNAME upsert: "
                                    <> err
                                )
                            )
                    Right () -> pure (Right ())

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

type DnsGateKey = (Text, Text)

data DnsGateItem = DnsGateItem
    { dgiEnvName :: Text
    , dgiNodeName :: Maybe Text
    , dgiVhostName :: Text
    , dgiDiscoveryHost :: Text
    , dgiExpectedHost :: Text
    }

data DnsUpsertEligibility = UpsertEligible | UpsertIneligible Text

data DnsUpsertOutcome
    = UpsertPlanned
    | UpsertSucceeded
    | UpsertFailed Text
    | UpsertUnavailable Text

data DnsGateMismatch = DnsGateMismatch
    { dgmItem :: DnsGateItem
    , dgmOutcome :: DnsUpsertOutcome
    }

dnsGateKey :: DnsGateItem -> DnsGateKey
dnsGateKey item = (item.dgiDiscoveryHost, item.dgiExpectedHost)

collectDnsGateItems :: Text -> KM.KeyMap A.Value -> [DnsGateItem]
collectDnsGateItems hostenvHostname envs =
    concatMap collectEnv (KM.toList envs)
  where
    collectEnv (kEnv, vEnv) =
        case vEnv of
            A.Object envObj ->
                let envName = K.toText kEnv
                    hostenvObj = fromMaybe KM.empty (lookupObj (K.fromString "hostenv") envObj)
                    envUserName = fromMaybe envName (lookupText (K.fromString "userName") hostenvObj)
                    nodeName = lookupText (K.fromString "node") envObj
                    canonicalDiscoveryHost = PrevNode.canonicalHostInDomain envUserName hostenvHostname
                    dnsHostFor vh =
                        if isSubdomainOf vh hostenvHostname
                            then vh
                            else canonicalDiscoveryHost
                    expectedHostFor vh = maybe vh (`PrevNode.canonicalHostInDomain` hostenvHostname) nodeName
                    vhosts = fromMaybe KM.empty (lookupObj (K.fromString "virtualHosts") envObj)
                 in map
                        ( \(vhKey, _) ->
                            let vhName = K.toText vhKey
                             in DnsGateItem
                                    { dgiEnvName = envName
                                    , dgiNodeName = nodeName
                                    , dgiVhostName = vhName
                                    , dgiDiscoveryHost = dnsHostFor vhName
                                    , dgiExpectedHost = expectedHostFor vhName
                                    }
                        )
                        (KM.toList vhosts)
            _ -> []

uniqueDnsGateItems :: [DnsGateItem] -> [DnsGateItem]
uniqueDnsGateItems =
    reverse . fst . foldl step ([], S.empty)
  where
    step (acc, seen) item =
        let key = dnsGateKey item
         in if S.member key seen
                then (acc, seen)
                else (item : acc, S.insert key seen)

classifyUpsertEligibility :: Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> DnsUpsertEligibility
classifyUpsertEligibility withDnsUpdate mToken mZoneId mZoneName vhName
    | not withDnsUpdate = UpsertIneligible "dns updates are disabled (use --with-dns-update)"
    | isNothing mToken = UpsertIneligible "Cloudflare token was not provided"
    | isNothing mZoneId = UpsertIneligible "Cloudflare zone was not provided"
    | isNothing mZoneName = UpsertIneligible "could not resolve Cloudflare zone name"
    | otherwise =
        case mZoneName of
            Just zoneName ->
                if isSubdomainOf vhName zoneName
                    then UpsertEligible
                    else UpsertIneligible ("host is outside Cloudflare zone " <> zoneName)
            Nothing -> UpsertIneligible "could not resolve Cloudflare zone name"

waitForDnsPropagation :: [DnsGateItem] -> IO (S.Set DnsGateKey, [DnsGateItem])
waitForDnsPropagation items = do
    (resolved0, unresolved0) <- partitionByDns items
    go resolved0 unresolved0 (backoffDelays 1 30 600)
  where
    partitionByDns recs = do
        checks <- forM recs $ \item -> do
            ok <- dnsPointsTo item.dgiDiscoveryHost item.dgiExpectedHost
            pure (item, ok)
        pure
            ( [ item
              | (item, True) <- checks
              ]
            , [ item
              | (item, False) <- checks
              ]
            )
    go resolved unresolved _ | null unresolved = pure (S.fromList (map dnsGateKey resolved), [])
    go resolved unresolved [] = pure (S.fromList (map dnsGateKey resolved), unresolved)
    go resolved unresolved (delaySec : restDelays) = do
        printProvider
            ( "hostenv-provider: waiting "
                <> T.pack (show delaySec)
                <> "s before rechecking DNS propagation for "
                <> T.pack (show (length unresolved))
                <> " vhost(s)"
            )
        threadDelay (delaySec * 1000000)
        (resolvedNow, unresolvedNow) <- partitionByDns unresolved
        go (resolved <> resolvedNow) unresolvedNow restDelays

-- -------- DNS gate --------
runDnsGate :: Maybe Text -> Maybe Text -> Maybe Text -> Bool -> Bool -> IO ()
runDnsGate mNode mTok mZone withDnsUpdate dryRun = do
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
            let envsAll = fromMaybe KM.empty (lookupObj (K.fromString "environments") plan)
            let envs = filterEnvironmentsByNode mNode envsAll
            mCfToken <- case mTok of
                Just t -> pure (Just t)
                Nothing -> case cfPlan >>= lookupText (K.fromString "apiTokenFile") of
                    Just path -> Just . T.pack <$> readFileTrim (T.unpack path)
                    Nothing -> fmap T.pack <$> Env.lookupEnv "CF_API_TOKEN"
            mCfZone <- case mZone of
                Just z -> pure (Just z)
                Nothing -> case cfPlan >>= lookupText (K.fromString "zoneId") of
                    Just z -> pure (Just z)
                    Nothing -> fmap T.pack <$> Env.lookupEnv "CF_ZONE_ID"
            mCfZoneName <- case (mCfToken, mCfZone) of
                (Just t, Just z) -> cfZoneName t z
                _ -> pure Nothing
            let hasCF = isJustPair mCfToken mCfZone
            when (withDnsUpdate && not hasCF) $
                printProviderErr $
                    "hostenv-provider: warning: --with-dns-update requested but Cloudflare credentials are missing (set both --cf-token/CF_API_TOKEN and --cf-zone/CF_ZONE_ID). "
                        <> "Continuing in check-only mode without DNS updates."

            let items = collectDnsGateItems hostenvHostname envs
            checked <- forM items $ \item -> do
                ok <- dnsPointsTo item.dgiDiscoveryHost item.dgiExpectedHost
                pure (item, ok)

            let mismatched =
                    [ item
                    | (item, False) <- checked
                    ]

            (mismatchOutcomesRev, cfDryRunCallsRev, _seenUpserts) <- foldlM (processMismatch mCfToken mCfZone mCfZoneName withDnsUpdate dryRun) ([], [], M.empty) mismatched
            let mismatchOutcomes = reverse mismatchOutcomesRev
            let cfDryRunCalls = reverse cfDryRunCallsRev

            let upsertedItems =
                    uniqueDnsGateItems
                        [ mismatch.dgmItem
                        | mismatch <- mismatchOutcomes
                        , case mismatch.dgmOutcome of
                            UpsertSucceeded -> True
                            _ -> False
                        ]

            when (not dryRun && not (null upsertedItems)) $
                printProvider
                    ( "hostenv-provider: waiting for DNS propagation for "
                        <> T.pack (show (length upsertedItems))
                        <> " vhost(s) with exponential backoff (max 30s, total 10m)"
                    )

            (propagatedKeys, timedOutItems) <-
                if dryRun
                    then pure (S.empty, [])
                    else waitForDnsPropagation upsertedItems

            when dryRun $
                when withDnsUpdate $
                    printProvider
                        "hostenv-provider: dry-run: propagation waiting is skipped; real runs wait up to 10 minutes before deciding ACME/SSL changes"

            forM_ timedOutItems $ \item ->
                printProviderErr
                    ( "hostenv-provider: warning: DNS did not propagate within 10 minutes for "
                        <> item.dgiDiscoveryHost
                        <> " -> "
                        <> item.dgiExpectedHost
                        <> " (while gating "
                        <> item.dgiVhostName
                        <> ")"
                    )

            let shouldDisable mismatch =
                    case mismatch.dgmOutcome of
                        UpsertPlanned -> False
                        UpsertSucceeded -> S.notMember (dnsGateKey mismatch.dgmItem) propagatedKeys
                        UpsertFailed _ -> True
                        UpsertUnavailable _ -> True

            let disableTargets =
                    [ mismatch
                    | mismatch <- mismatchOutcomes
                    , shouldDisable mismatch
                    ]

            forM_ disableTargets $ \mismatch ->
                case mismatch.dgmOutcome of
                    UpsertUnavailable reason ->
                        printProviderErr
                            ( "hostenv-provider: warning: disabling ACME/forceSSL for "
                                <> mismatch.dgmItem.dgiVhostName
                                <> " because DNS host "
                                <> mismatch.dgmItem.dgiDiscoveryHost
                                <> " does not point to "
                                <> mismatch.dgmItem.dgiExpectedHost
                                <> " and remediation is unavailable ("
                                <> reason
                                <> ")"
                            )
                    UpsertFailed reason ->
                        printProviderErr
                            ( "hostenv-provider: warning: disabling ACME/forceSSL for "
                                <> mismatch.dgmItem.dgiVhostName
                                <> " because Cloudflare upsert failed ("
                                <> reason
                                <> ")"
                            )
                    UpsertSucceeded ->
                        printProviderErr
                            ( "hostenv-provider: warning: disabling ACME/forceSSL for "
                                <> mismatch.dgmItem.dgiVhostName
                                <> " because DNS propagation for host "
                                <> mismatch.dgmItem.dgiDiscoveryHost
                                <> " did not complete in time"
                            )
                    UpsertPlanned -> pure ()

            let applyDisable planAcc mismatch =
                    let item = mismatch.dgmItem
                        plan1 = disableAcmePaths item.dgiEnvName item.dgiVhostName planAcc
                     in disableAcmeOnNode item.dgiNodeName item.dgiVhostName plan1

            let plan' = foldl applyDisable plan disableTargets
            planPretty <- prettyJson (A.encode plan')
            if dryRun
                then do
                    _ <- printDiffAgainstFile planPath planPretty
                    when withDnsUpdate $
                        if null cfDryRunCalls
                            then printProvider "hostenv-provider: dry-run: no Cloudflare DNS API calls would be made"
                            else do
                                printProvider "hostenv-provider: dry-run: Cloudflare DNS API calls that would be made:"
                                mapM_ (printProvider . ("hostenv-provider:   " <>)) cfDryRunCalls
                    printProvider "hostenv-provider: dry-run: generated/plan.json was not modified"
                else do
                    existingPretty <- prettyJson raw
                    if existingPretty == planPretty
                        then printProvider "hostenv-provider: dns-gate made no plan changes"
                        else do
                            let tmpPath = dest <> "/plan.json.tmp"
                            BL.writeFile (T.unpack tmpPath) planPretty
                            Sh.mv (fromString (T.unpack tmpPath)) (fromString (T.unpack planPath))
                            BLC.putStrLn "✅ dns-gate updated plan.json"
  where
    isJustPair (Just _) (Just _) = True
    isJustPair _ _ = False
    readFileTrim p = T.unpack . T.strip . T.pack <$> readFile p
    foldlM f z [] = pure z
    foldlM f z (x : xs) = f z x >>= \z' -> foldlM f z' xs
    processMismatch mCfToken mCfZone mCfZoneName withDnsUpdate dryRun (outcomes, cfOps, seenUpserts) item =
        let key = dnsGateKey item
         in case M.lookup key seenUpserts of
                Just cachedOutcome ->
                    pure
                        ( DnsGateMismatch item cachedOutcome : outcomes
                        , cfOps
                        , seenUpserts
                        )
                Nothing ->
                    case classifyUpsertEligibility withDnsUpdate mCfToken mCfZone mCfZoneName item.dgiDiscoveryHost of
                        UpsertIneligible reason ->
                            let outcome = UpsertUnavailable reason
                             in pure
                                    ( DnsGateMismatch item outcome : outcomes
                                    , cfOps
                                    , M.insert key outcome seenUpserts
                                    )
                        UpsertEligible ->
                            case (mCfToken, mCfZone) of
                                (Just token, Just zoneId) ->
                                    if dryRun
                                        then
                                            let outcome = UpsertPlanned
                                             in pure
                                                    ( DnsGateMismatch item outcome : outcomes
                                                    , ("upsert CNAME " <> item.dgiDiscoveryHost <> " -> " <> item.dgiExpectedHost <> " in zone " <> zoneId) : cfOps
                                                    , M.insert key outcome seenUpserts
                                                    )
                                        else do
                                            printProvider
                                                ( "hostenv-provider: upserting Cloudflare CNAME "
                                                    <> item.dgiDiscoveryHost
                                                    <> " -> "
                                                    <> item.dgiExpectedHost
                                                )
                                            upsertResult <- cfUpsertCname token zoneId item.dgiDiscoveryHost item.dgiExpectedHost
                                            case upsertResult of
                                                Right () ->
                                                    let outcome = UpsertSucceeded
                                                     in pure
                                                            ( DnsGateMismatch item outcome : outcomes
                                                            , cfOps
                                                            , M.insert key outcome seenUpserts
                                                            )
                                                Left err ->
                                                    let outcome = UpsertFailed err
                                                     in pure
                                                            ( DnsGateMismatch item outcome : outcomes
                                                            , cfOps
                                                            , M.insert key outcome seenUpserts
                                                            )
                                _ ->
                                    let outcome = UpsertUnavailable "Cloudflare credentials are missing"
                                     in pure
                                            ( DnsGateMismatch item outcome : outcomes
                                            , cfOps
                                            , M.insert key outcome seenUpserts
                                            )

isInsideGitWorkTree :: IO Bool
isInsideGitWorkTree = do
    probeResult <- try (readProcessWithExitCode "git" ["rev-parse", "--is-inside-work-tree"] "") :: IO (Either SomeException (ExitCode, String, String))
    case probeResult of
        Left _ -> pure False
        Right (code, out, _err) -> pure (code == ExitSuccess && T.strip (T.pack out) == "true")

isTrackedByGit :: Text -> IO Bool
isTrackedByGit path = do
    (code, _out, _err) <-
        readProcessWithExitCode
            "git"
            ["ls-files", "--error-unmatch", "--", T.unpack path]
            ""
    pure (code == ExitSuccess)

ensureTrackedInGit :: [Text] -> IO ()
ensureTrackedInGit paths = do
    insideGit <- isInsideGitWorkTree
    when insideGit $
        forM_ (nub paths) $ \path -> do
            exists <- Dir.doesFileExist (T.unpack path)
            when exists $ do
                tracked <- isTrackedByGit path
                unless tracked $ do
                    printProvider ("hostenv-provider: tracking new generated file " <> path)
                    addRes <- Sh.proc "git" ["add", path] Sh.empty
                    when (addRes /= ExitSuccess) $
                        error ("hostenv-provider: failed to add " <> T.unpack path <> " to git index")

runPlan :: Bool -> IO ()
runPlan dryRun = do
    let dest = "generated" :: Text
    let planDest = dest <> "/plan.json"
    let stateDest = dest <> "/state.json"
    let flakeDest = dest <> "/flake.nix"
    let flakeLockDest = dest <> "/flake.lock"

    unless dryRun $ do
        Sh.mktree (fromString (T.unpack dest))
        stateExists <- Sh.testfile (fromString (T.unpack stateDest))
        unless stateExists $ do
            BLC.writeFile (T.unpack stateDest) "{}\n"
            ensureTrackedInGit [stateDest]

    system <- nixEvalRaw ["eval", "--impure", "--raw", "--expr", "builtins.currentSystem"]
    let planAttrBase = ".#lib.provider.planPaths." <> T.strip system
    planSource <- nixBuildPath (planAttrBase <> ".plan")

    planRaw <- BL.readFile (T.unpack planSource)
    planPretty <- prettyJson planRaw
    if dryRun
        then do
            _ <- printDiffAgainstFile planDest planPretty
            printProvider "hostenv-provider: dry-run: generated/plan.json was not modified"
        else do
            stateSource <- nixBuildPath (planAttrBase <> ".state")
            flakeSource <- nixBuildPath (planAttrBase <> ".flake")

            BL.writeFile (T.unpack planDest) planPretty

            stateRaw <- BL.readFile (T.unpack stateSource)
            BL.writeFile (T.unpack stateDest) stateRaw

            flakeRaw <- BL.readFile (T.unpack flakeSource)
            BL.writeFile (T.unpack flakeDest) flakeRaw

            ensureTrackedInGit [planDest, stateDest, flakeDest]
            _ <- Sh.procs "nix" (nixCommonArgs ++ ["flake", "update", "--flake", "./" <> dest]) Sh.empty
            ensureTrackedInGit [flakeLockDest]
            BLC.putStrLn ("✅ Infrastructure configuration written to " <> BLC.pack (T.unpack dest))
  where
    nixCommonArgs :: [Text]
    nixCommonArgs = ["--extra-experimental-features", "nix-command flakes"]

    nixEvalRaw :: [Text] -> IO Text
    nixEvalRaw args = T.strip <$> Sh.strict (Sh.inproc "nix" (nixCommonArgs ++ args) Sh.empty)

    nixBuildPath :: Text -> IO Text
    nixBuildPath attr =
        T.strip <$> Sh.strict (Sh.inproc "nix" (nixCommonArgs ++ ["build", "--no-link", "--print-out-paths", attr]) Sh.empty)

decodeJsonObject :: Text -> Text -> KM.KeyMap A.Value

decodeJsonObject context raw =
    case A.eitherDecode' (BL.fromStrict (TE.encodeUtf8 raw)) of
        Right (A.Object obj) -> obj
        Right _ -> error ("hostenv-provider: expected JSON object for " <> T.unpack context)
        Left decodeErr ->
            error
                ( "hostenv-provider: failed to parse JSON for "
                    <> T.unpack context
                    <> ": "
                    <> decodeErr
                )

readSecretsObject :: Text -> IO (KM.KeyMap A.Value)
readSecretsObject path = do
    let pathString = T.unpack path
    exists <- Dir.doesFileExist pathString
    unless exists $
        error ("hostenv-provider: secrets file not found: " <> pathString)

    (decryptCode, decryptOut, decryptErr) <-
        readProcessWithExitCode
            "sops"
            ["--decrypt", "--output-type", "json", pathString]
            ""
    case decryptCode of
        ExitSuccess ->
            pure (decodeJsonObject path (T.pack decryptOut))
        ExitFailure _ -> do
            (yamlCode, yamlOut, yamlErr) <-
                readProcessWithExitCode
                    "yq"
                    ["-o=json", ".", pathString]
                    ""
            case yamlCode of
                ExitSuccess -> do
                    let parsed = decodeJsonObject path (T.pack yamlOut)
                    if KM.member (K.fromString "sops") parsed
                        then
                            error
                                ( "hostenv-provider: could not decrypt SOPS file "
                                    <> pathString
                                    <> " ("
                                    <> decryptErr
                                    <> ")"
                                )
                        else pure parsed
                ExitFailure _ ->
                    error
                        ( "hostenv-provider: failed to read secrets file "
                            <> pathString
                            <> " as encrypted or plaintext YAML (sops: "
                            <> decryptErr
                            <> "; yq: "
                            <> yamlErr
                            <> ")"
                        )

data SecretAssignment = SecretAssignment
    { bucket :: Text
    , key :: Text
    , value :: A.Value
    , source :: Text
    }

data CollectedSecrets = CollectedSecrets
    { assignments :: [SecretAssignment]
    , manifestKeys :: [Text]
    }

resolveHostenvConfigRoot :: Text -> IO Text
resolveHostenvConfigRoot root = do
    let rootString = T.unpack root
    let hostenvRoot = rootString FP.</> ".hostenv"
    exists <- Dir.doesDirectoryExist hostenvRoot
    pure (T.pack (if exists then hostenvRoot else rootString))

resolveScopePath :: Text -> SecretsConfig -> Text -> Text
resolveScopePath hostenvConfigRoot scope defaultRel =
    case scope.secretsFilePath of
        Just path
            | not (T.null (T.strip path)) ->
                if FP.isAbsolute (T.unpack path)
                    then path
                    else T.pack (T.unpack hostenvConfigRoot FP.</> T.unpack path)
        _ -> T.pack (T.unpack hostenvConfigRoot FP.</> T.unpack defaultRel)

collectAssignments :: Text -> Text -> Text -> Text -> SecretsConfig -> Text -> IO CollectedSecrets
collectAssignments scopeLabel envUser bucket hostenvConfigRoot scope defaultRel =
    if not scope.secretsEnabled
        then pure (CollectedSecrets [] [])
        else do
            let path = resolveScopePath hostenvConfigRoot scope defaultRel
            obj <- readSecretsObject path
            let selectedKeys =
                    if null scope.secretsKeys
                        then map K.toText (KM.keys obj)
                        else scope.secretsKeys
            assignments <- forM selectedKeys $ \secretKey ->
                case KM.lookup (K.fromText secretKey) obj of
                    Just secretValue ->
                        pure
                            SecretAssignment
                                { bucket
                                , key = secretKey
                                , value = secretValue
                                , source = path
                                }
                    Nothing ->
                        error
                            ( "hostenv-provider: declared "
                                <> T.unpack scopeLabel
                                <> " secret key '"
                                <> T.unpack secretKey
                                <> "' for environment '"
                                <> T.unpack envUser
                                <> "' is missing from "
                                <> T.unpack path
                                <> ". Run 'hostenv secrets' to scaffold keys."
                            )
            pure
                CollectedSecrets
                    { assignments
                    , manifestKeys = nub selectedKeys
                    }

consolidateAssignments :: [SecretAssignment] -> [(Text, Text, A.Value)]
consolidateAssignments assignments =
    let
        go acc [] = acc
        go acc (assignment : rest) =
            let key = (assignment.bucket, assignment.key)
             in case M.lookup key acc of
                    Nothing ->
                        go (M.insert key (assignment.value, assignment.source) acc) rest
                    Just (existingValue, existingSource) ->
                        if existingValue == assignment.value
                            then go acc rest
                            else
                                error
                                    ( "hostenv-provider: conflicting values for secret "
                                        <> T.unpack assignment.bucket
                                        <> "/"
                                        <> T.unpack assignment.key
                                        <> " from "
                                        <> T.unpack existingSource
                                        <> " and "
                                        <> T.unpack assignment.source
                                    )
     in
        map
            (\((bucket, secretKey), (secretValue, _)) -> (bucket, secretKey, secretValue))
            (M.toList (go M.empty assignments))

applySecretAssignments :: KM.KeyMap A.Value -> [(Text, Text, A.Value)] -> KM.KeyMap A.Value
applySecretAssignments = foldl applySecret
  where
    applySecret secretsRoot (bucket, secretKey, secretValue) =
        let bucketKey = K.fromText bucket
            secretKey' = K.fromText secretKey
            newBucketValue = A.Object (KM.singleton secretKey' secretValue)
         in case KM.lookup bucketKey secretsRoot of
                Nothing ->
                    KM.insert bucketKey newBucketValue secretsRoot
                Just (A.Object bucketObject) ->
                    KM.insert bucketKey (A.Object (KM.insert secretKey' secretValue bucketObject)) secretsRoot
                Just _ ->
                    error
                        ( "hostenv-provider: cannot write secret "
                            <> T.unpack bucket
                            <> "/"
                            <> T.unpack secretKey
                            <> " because bucket '"
                            <> T.unpack bucket
                            <> "' is not an object in provider secrets"
                        )

manifestKey :: K.Key
manifestKey = K.fromString "__hostenv_selected_keys"

manifestKeyRegex :: Text
manifestKeyRegex = "^" <> K.toText manifestKey <> "$"

-- Adds environment and project secrets to a KeyMap under key @manifestKey@.
--
-- Stores hostenv project secrets in JSON objects.
applyManifestKey :: [(Text, [Text], [Text])] -> KM.KeyMap A.Value -> KM.KeyMap A.Value
applyManifestKey scopeSelections mergedSecrets =
    let
        keysToObj (envUser, projectKeys, environmentKeys) =
            ( K.fromText envUser
            , A.object
                [ "project" .= projectKeys
                , "environment" .= environmentKeys
                ]
            )
        scopedObj = A.Object (KM.fromList (map keysToObj scopeSelections))
     in
        KM.insert manifestKey scopedObj mergedSecrets

ageRecipients :: Text -> IO [Text]
ageRecipients providerSecretsPath = do
    (code, out, err) <-
        readProcessWithExitCode
            "yq"
            ["-r", ".sops.age | .[] | .recipient", T.unpack providerSecretsPath]
            ""
    case code of
        ExitFailure _ ->
            error
                ( "hostenv-provider: failed to read age recipients from "
                    <> T.unpack providerSecretsPath
                    <> ": "
                    <> err
                )
        ExitSuccess ->
            pure
                ( nub
                    ( filter
                        (not . T.null)
                        (map T.strip (T.lines (T.pack out)))
                    )
                )

mergedSecretsPath :: Text
mergedSecretsPath = "generated/secrets.merged.yaml"

providerSecretsPath :: Text
providerSecretsPath = "secrets/provider.yaml"

writeMergedSecrets :: [Text] -> KM.KeyMap A.Value -> IO ()
writeMergedSecrets recipients mergedSecrets = do
    let mergedPathString = T.unpack mergedSecretsPath
    let recipientsCsv = T.intercalate "," recipients
    Dir.createDirectoryIfMissing True "generated"

    tmpDir <- Dir.getTemporaryDirectory
    (tmpPath, handle) <- openTempFile tmpDir "hostenv-provider-secrets-XXXXXX.json"
    BL.hPut handle (A.encode (A.Object mergedSecrets))
    hClose handle

    (encryptCode, encryptOut, encryptErr) <-
        readProcessWithExitCode
            "sops"
            [ "--encrypt"
            , -- Passing `--config /dev/null` to sops prevents provider config
              -- restricting its `creation_rules` in ways that exclude
              "--config"
            , "/dev/null"
            , "--input-type"
            , "json"
            , "--output-type"
            , "yaml"
            , -- `--unencrypted-regex` tells SOPS to not encrypt anything under
              -- `__hostenv_selected_keys`. This isn't a security risk, as this
              -- key only contains metadata of which keys are selected per
              -- environment / project.
              "--unencrypted-regex"
            , T.unpack manifestKeyRegex
            , "--age"
            , T.unpack recipientsCsv
            , tmpPath
            ]
            ""
    _ <- try (Dir.removeFile tmpPath) :: IO (Either SomeException ())

    case encryptCode of
        ExitSuccess ->
            writeFile mergedPathString encryptOut
        ExitFailure _ ->
            error
                ( "hostenv-provider: failed to encrypt "
                    <> mergedPathString
                    <> ": "
                    <> encryptErr
                )

writeProviderSecrets :: [Text] -> KM.KeyMap A.Value -> IO ()
writeProviderSecrets recipients secretsObj = do
    let providerPathString = T.unpack providerSecretsPath
    let recipientsCsv = T.intercalate "," recipients

    tmpDir <- Dir.getTemporaryDirectory
    (tmpPath, handle) <- openTempFile tmpDir "hostenv-provider-provider-secrets-XXXXXX.json"
    BL.hPut handle (A.encode (A.Object secretsObj))
    hClose handle

    (encryptCode, encryptOut, encryptErr) <-
        readProcessWithExitCode
            "sops"
            [ "--encrypt"
            , "--config"
            , "/dev/null"
            , "--input-type"
            , "json"
            , "--output-type"
            , "yaml"
            , "--age"
            , T.unpack recipientsCsv
            , tmpPath
            ]
            ""
    _ <- try (Dir.removeFile tmpPath) :: IO (Either SomeException ())

    case encryptCode of
        ExitSuccess ->
            writeFile providerPathString encryptOut
        ExitFailure _ ->
            error
                ( "hostenv-provider: failed to encrypt "
                    <> providerPathString
                    <> ": "
                    <> encryptErr
                )

prepareMergedSecrets :: [EnvSecretsConfig] -> IO ()
prepareMergedSecrets envSecretsConfigs = do
    providerSecretsExists <- Dir.doesFileExist (T.unpack providerSecretsPath)
    unless providerSecretsExists $
        error ("hostenv-provider: provider secrets file not found: " <> T.unpack providerSecretsPath)

    selectionsAndAssignments <-
        forM envSecretsConfigs $ \envCfg -> do
            hostenvConfigRoot <- resolveHostenvConfigRoot envCfg.root
            projectSecrets <-
                collectAssignments
                    "project"
                    envCfg.userName
                    (envCfg.organisation <> "_" <> envCfg.project)
                    hostenvConfigRoot
                    envCfg.projectSecretsCfg
                    "secrets/project.yaml"

            envSecrets <-
                collectAssignments
                    "environment"
                    envCfg.userName
                    envCfg.userName
                    hostenvConfigRoot
                    envCfg.environmentSecretsCfg
                    ("secrets/" <> envCfg.safeEnvironmentName <> ".yaml")

            pure
                ( envCfg.userName
                , projectSecrets.manifestKeys
                , envSecrets.manifestKeys
                , projectSecrets.assignments <> envSecrets.assignments
                )

    let scopeSelections =
            map
                (\(envUser, projectKeys, environmentKeys, _assignmentsRaw) -> (envUser, projectKeys, environmentKeys))
                selectionsAndAssignments
    let assignments =
            consolidateAssignments
                (concatMap (\(_envUser, _projectKeys, _environmentKeys, assignmentsRaw) -> assignmentsRaw) selectionsAndAssignments)

    Dir.createDirectoryIfMissing True "generated"
    if null assignments
        then do
            Dir.copyFile (T.unpack providerSecretsPath) (T.unpack mergedSecretsPath)
            printProvider ("hostenv-provider: wrote " <> mergedSecretsPath <> " from provider secrets (no project/env secret overrides)")
        else do
            baseSecrets <- readSecretsObject providerSecretsPath
            let mergedSecrets = applyManifestKey scopeSelections (applySecretAssignments baseSecrets assignments)
            recipients <- ageRecipients providerSecretsPath
            when (null recipients) $
                error
                    ( "hostenv-provider: no age recipients found in "
                        <> T.unpack providerSecretsPath
                        <> ". Add recipients under sops.age[].recipient."
                    )
            writeMergedSecrets recipients mergedSecrets
            printProvider
                ( "hostenv-provider: merged "
                    <> T.pack (show (length assignments))
                    <> " project/environment secret key(s) into "
                    <> mergedSecretsPath
                )
    ensureTrackedInGit [mergedSecretsPath]

runCominTokens :: Bool -> IO ()
runCominTokens dryRun = do
    let planPath = "generated/plan.json"
    planExists <- Dir.doesFileExist planPath
    unless planExists $
        error "hostenv-provider: plan.json not found; run 'hostenv-provider plan' first"

    raw <- BL.readFile planPath
    plan <-
        case A.eitherDecode' raw of
            Left err -> error err
            Right (parsed :: KM.KeyMap A.Value) -> pure parsed

    let nodes = planNodeNames plan
    when (null nodes) $
        error "hostenv-provider: no nodes found in generated/plan.json"

    secretsObj <- readSecretsObject providerSecretsPath
    let tokensValue = KM.lookup (K.fromString "comin_node_tokens") secretsObj
    let existingTokens =
            case tokensValue of
                Nothing -> M.empty
                Just (A.Object obj) ->
                    M.fromList
                        [ (K.toText key, T.strip token)
                        | (key, A.String token) <- KM.toList obj
                        , T.strip token /= ""
                        ]
                Just _ ->
                    error "hostenv-provider: comin_node_tokens must be a mapping of node -> token"
    let missingNodes = filter (\nodeName -> M.notMember nodeName existingTokens) nodes

    if null missingNodes
        then printProvider ("hostenv-provider: comin node tokens already present in " <> providerSecretsPath)
        else
            if dryRun
                then
                    printProvider
                        ( "hostenv-provider: dry-run: would generate comin node tokens for "
                            <> T.intercalate ", " missingNodes
                            <> " (" <> providerSecretsPath <> ")"
                        )
                else do
                    newTokens <- forM missingNodes $ \nodeName -> do
                        token <- generateCominToken
                        pure (nodeName, token)
                    let mergedTokens = M.union existingTokens (M.fromList newTokens)
                    let tokensObj =
                            A.Object
                                ( KM.fromList
                                    [ (K.fromText nodeName, A.String token)
                                    | (nodeName, token) <- M.toList mergedTokens
                                    ]
                                )
                    let updatedSecrets = KM.insert (K.fromString "comin_node_tokens") tokensObj secretsObj
                    recipients <- ageRecipients providerSecretsPath
                    when (null recipients) $
                        error
                            ( "hostenv-provider: no age recipients found in "
                                <> T.unpack providerSecretsPath
                                <> ". Add recipients under sops.age[].recipient."
                            )
                    writeProviderSecrets recipients updatedSecrets
                    printProvider
                        ( "hostenv-provider: wrote "
                            <> T.pack (show (length missingNodes))
                            <> " comin node token(s) to "
                            <> providerSecretsPath
                        )
  where
    generateCominToken = do
        (code, out, err) <- readProcessWithExitCode "openssl" ["rand", "-hex", "32"] ""
        case code of
            ExitSuccess -> pure (T.strip (T.pack out))
            ExitFailure _ ->
                error ("hostenv-provider: failed to generate comin node token: " <> err)

-- -------- Main --------
main :: IO ()
main = do
    CLI cmd <- OA.execParser cliOpts
    case cmd of
        CmdPlan dryRun -> runPlan dryRun
        CmdDnsGate mNode mTok mZone withDnsUpdate dryRun -> runDnsGate mNode mTok mZone withDnsUpdate dryRun
        CmdCominTokens dryRun -> runCominTokens dryRun
