{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Hostenv.Provider.Service
  ( verifyGitHubSignature
  , verifyGitLabToken
  , nodesForProject
  , projectForHash
  , projectHashFor
  , nodesForProjectWithDnsWith
  , renderProjectInputs
  , renderFlakeTemplate
  , renderGitCredentials
  , GitlabSecrets(..)
  , readGitlabSecrets
  , WebhookConfig(..)
  , ProjectRef(..)
  , CommandSpec(..)
  , CommandOutput(..)
  , CommandError(..)
  , NodeAction(..)
  , NodeIntent(..)
  , WebhookUpdateStatus(..)
  , WebhookResult(..)
  , WebhookStage(..)
  , renderWebhookStage
  , WebhookError(..)
  , CommandRunner
  , PlanLoader
  , StageNotifier
  , runWebhookWith
  , renderDeployIntentDocument
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM)
import Data.Aeson (Value (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (foldl', intersect, nub, sort, sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..), die)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import "crypton" Crypto.Hash (SHA256)
import "crypton" Crypto.MAC.HMAC (HMAC (..), hmac)
import Hostenv.Provider.PrevNodeDiscovery qualified as PrevNode


constantTimeEq :: BS.ByteString -> BS.ByteString -> Bool
constantTimeEq a b = BA.constEq a b

verifyGitHubSignature :: BS.ByteString -> BL.ByteString -> BS.ByteString -> Bool
verifyGitHubSignature secret rawBody header =
  let headerLower = BSC.map toLower header
      prefix = "sha256="
   in case BSC.stripPrefix prefix headerLower of
        Nothing -> False
        Just sigHex ->
          let digest = hmac secret (BL.toStrict rawBody) :: HMAC SHA256
              expected = BAE.convertToBase BAE.Base16 (BA.convert digest :: BS.ByteString)
           in constantTimeEq expected sigHex

verifyGitLabToken :: BS.ByteString -> BS.ByteString -> Bool
verifyGitLabToken secret token = constantTimeEq secret token

lookupText :: KM.Key -> KM.KeyMap Value -> Maybe Text
lookupText key obj = case KM.lookup key obj of
  Just (String t) -> Just t
  _ -> Nothing

lookupObj :: KM.Key -> KM.KeyMap Value -> Maybe (KM.KeyMap Value)
lookupObj key obj = case KM.lookup key obj of
  Just (Object o) -> Just o
  _ -> Nothing

lookupTextList :: KM.Key -> KM.KeyMap Value -> [Text]
lookupTextList key obj = case KM.lookup key obj of
  Just (Array arr) -> mapMaybe asText (toList arr)
  _ -> []
  where
    asText (String t) = Just t
    asText _ = Nothing

nodesForProject :: Text -> Text -> BL.ByteString -> Either Text [Text]
nodesForProject org project raw = do
  envs <- decodeEnvironments raw
  let matches =
        mapMaybe extractEnv (KM.toList envs)
      nodes = sort (nub (map envNode matches))
      deps =
        [ (envNode info, prev)
        | info <- matches
        , Just prev <- [info.envPrevNode]
        , prev /= info.envNode
        ]
  Right (orderNodes nodes deps)
  where
    extractEnv (_kEnv, vEnv) = case vEnv of
      Object envObj -> do
        hostenvObj <- lookupObj (K.fromString "hostenv") envObj
        org' <- lookupText (K.fromString "organisation") hostenvObj
        project' <- lookupText (K.fromString "project") hostenvObj
        if org' == org && project' == project
          then do
            node <- lookupText (K.fromString "node") envObj
            let prevNode = lookupText (K.fromString "previousNode") envObj
            pure (EnvNodeInfo node prevNode)
          else Nothing
      _ -> Nothing

nodesForProjectWithDns :: Text -> Text -> BL.ByteString -> IO (Either Text [Text])
nodesForProjectWithDns = nodesForProjectWithDnsWith dnsPointsTo

nodesForProjectWithDnsWith :: (Text -> Text -> IO Bool) -> Text -> Text -> BL.ByteString -> IO (Either Text [Text])
nodesForProjectWithDnsWith pointsTo org project raw = do
  case decodePlanRoot raw of
    Left err -> pure (Left err)
    Right root ->
      case decodeEnvironmentsFromRoot root of
        Left err -> pure (Left err)
        Right envs -> do
          let hostenvHostname = lookupText (K.fromString "hostenvHostname") root
          matches <- fmap catMaybes $ forM (KM.toList envs) $ \(kEnv, vEnv) ->
            case vEnv of
              Object envObj -> do
                hostenvObj <- pure (lookupObj (K.fromString "hostenv") envObj)
                let envName = K.toText kEnv
                let envUserName = fromMaybe envName (hostenvObj >>= lookupText (K.fromString "userName"))
                case hostenvObj of
                  Just hostenvObj' -> do
                    case (lookupText (K.fromString "organisation") hostenvObj', lookupText (K.fromString "project") hostenvObj') of
                      (Just org', Just project') | org' == org && project' == project -> do
                        case lookupText (K.fromString "node") envObj of
                          Just node -> do
                            let prevNode = lookupText (K.fromString "previousNode") envObj
                            let migrateBackups = lookupTextList (K.fromString "migrations") envObj
                            pure (Just (envUserName, node, prevNode, migrateBackups))
                          Nothing -> pure Nothing
                      _ -> pure Nothing
                  Nothing -> pure Nothing
              _ -> pure Nothing
          let rootNodeNames =
                case lookupObj (K.fromString "nodes") root of
                  Nothing -> []
                  Just nodesObj -> map (K.toText . fst) (KM.toList nodesObj)
          let discoveryNodes =
                S.toList $
                  S.fromList $
                    filter (not . T.null) (rootNodeNames <> map (\(_, node, _, _) -> node) matches)

          resolvedMatches <- forM matches $ \(envUserName, node, prevNode, migrateBackups) -> do
            resolvedPrev <-
              if null migrateBackups
                then pure (Right prevNode)
                else resolvePrevNodeFromDnsWith pointsTo hostenvHostname discoveryNodes envUserName node prevNode
            pure ((\prev -> EnvNodeInfo node prev) <$> resolvedPrev)

          case sequence resolvedMatches of
            Left err -> pure (Left err)
            Right resolvedInfos -> do
              let nodes = sort (nub (map envNode resolvedInfos))
              let deps =
                    [ (envNode info, prev)
                    | info <- resolvedInfos
                    , Just prev <- [info.envPrevNode]
                    , prev /= info.envNode
                    ]
              pure (Right (orderNodes nodes deps))

data EnvNodeInfo = EnvNodeInfo
  { envNode :: Text
  , envPrevNode :: Maybe Text
  }

orderNodes :: [Text] -> [(Text, Text)] -> [Text]
orderNodes nodes deps =
  let uniqueNodes = sort (nub nodes)
      nodeSet = S.fromList uniqueNodes
      deps' =
        nub
          [ (a, b)
          | (a, b) <- deps
          , a /= b
          , S.member a nodeSet
          , S.member b nodeSet
          ]
      outgoing = M.fromListWith (<>) [ (a, [b]) | (a, b) <- deps' ]
      indeg0 = M.fromList [ (n, 0 :: Int) | n <- uniqueNodes ]
      indeg = foldl' (\m (_a, b) -> M.adjust (+ 1) b m) indeg0 deps'
      zeros0 = S.fromList [ n | n <- uniqueNodes, M.findWithDefault 0 n indeg == 0 ]
      (ordered, _indegFinal) = topo outgoing indeg zeros0 []
      orderedSet = S.fromList ordered
      remaining = [ n | n <- uniqueNodes, S.notMember n orderedSet ]
   in if length ordered == length uniqueNodes
        then ordered
        else ordered <> remaining
  where
    topo outMap indegMap zeros acc =
      case S.minView zeros of
        Nothing -> (acc, indegMap)
        Just (n, zeros') ->
          let outs = M.findWithDefault [] n outMap
              (zeros'', indeg') = foldl' step (zeros', indegMap) outs
           in topo outMap indeg' zeros'' (acc <> [n])
    step (z, m) dest =
      let newVal = (M.findWithDefault 0 dest m) - 1
          m' = M.insert dest newVal m
          z' = if newVal == 0 then S.insert dest z else z
       in (z', m')

projectForHash :: Text -> BL.ByteString -> Either Text ProjectRef
projectForHash hash raw = do
  envs <- decodeEnvironments raw
  let matches =
        [ ProjectRef org project
        | (_kEnv, vEnv) <- KM.toList envs
        , Just (org, project, projHash) <- [extractHostenv vEnv]
        , projHash == hash
        ]
      uniqueMatches = nub matches
  case uniqueMatches of
    [] -> Left "webhook hash not found in plan.json"
    [ref] -> Right ref
    _ -> Left "webhook hash maps to multiple projects"
  where
    extractHostenv (Object envObj) = do
      hostenvObj <- lookupObj (K.fromString "hostenv") envObj
      org <- lookupText (K.fromString "organisation") hostenvObj
      project <- lookupText (K.fromString "project") hostenvObj
      projHash <- lookupText (K.fromString "projectNameHash") hostenvObj
      pure (org, project, projHash)
    extractHostenv _ = Nothing

projectHashFor :: Text -> Text -> BL.ByteString -> Either Text Text
projectHashFor org project raw = do
  envs <- decodeEnvironments raw
  let matches =
        [ ProjectCandidate envName envType projHash
        | (kEnv, vEnv) <- KM.toList envs
        , let envName = K.toText kEnv
        , Just (envOrg, envProject, projHash, envType) <- [extractCandidate vEnv]
        , envOrg == org
        , envProject == project
        ]
  case chooseCandidate matches of
    Nothing -> Left "project not found in plan.json"
    Just candidate -> Right candidate.candidateHash
  where
    extractCandidate (Object envObj) = do
      hostenvObj <- lookupObj (K.fromString "hostenv") envObj
      envOrg <- lookupText (K.fromString "organisation") hostenvObj
      envProject <- lookupText (K.fromString "project") hostenvObj
      projHash <- lookupText (K.fromString "projectNameHash") hostenvObj
      let envType = lookupText (K.fromString "type") envObj
      pure (envOrg, envProject, projHash, envType)
    extractCandidate _ = Nothing

data ProjectCandidate = ProjectCandidate
  { candidateEnvName :: Text
  , candidateEnvType :: Maybe Text
  , candidateHash :: Text
  } deriving (Eq, Show)

chooseCandidate :: [ProjectCandidate] -> Maybe ProjectCandidate
chooseCandidate [] = Nothing
chooseCandidate candidates =
  let production = filter (\c -> c.candidateEnvType == Just "production") candidates
      mainEnv = filter (\c -> c.candidateEnvName == "main") candidates
      masterEnv = filter (\c -> c.candidateEnvName == "master") candidates
      pick xs = case sortOn (\c -> c.candidateEnvName) xs of
        [] -> Nothing
        (c:_) -> Just c
   in case () of
        _ | not (null production) -> pick production
          | not (null mainEnv) -> pick mainEnv
          | not (null masterEnv) -> pick masterEnv
          | otherwise -> pick candidates

decodeEnvironments :: BL.ByteString -> Either Text (KM.KeyMap Value)
decodeEnvironments raw = do
  root <- decodePlanRoot raw
  decodeEnvironmentsFromRoot root

decodePlanRoot :: BL.ByteString -> Either Text (KM.KeyMap Value)
decodePlanRoot raw =
  case A.eitherDecode' raw of
    Left err -> Left (T.pack err)
    Right (Object root) -> Right root
    Right _ -> Left "plan.json root is not an object"

decodeEnvironmentsFromRoot :: KM.KeyMap Value -> Either Text (KM.KeyMap Value)
decodeEnvironmentsFromRoot root =
  case lookupObj (K.fromString "environments") root of
    Nothing -> Left "plan.json missing environments object"
    Just envs -> Right envs

stripDot :: Text -> Text
stripDot = T.dropWhileEnd (== '.')

digRRRaw :: Maybe Text -> Text -> Text -> IO [Text]
digRRRaw mNameserver name rr = do
  let serverArgs = maybe [] (\nameserver -> ["@" <> T.unpack nameserver]) mNameserver
  let args = serverArgs <> ["+short", T.unpack name, T.unpack rr]
  res <- try (readProcessWithExitCode "dig" args "") :: IO (Either IOException (ExitCode, String, String))
  case res of
    Left _ -> pure []
    Right (ExitSuccess, out, _) ->
      let toLine = stripDot . T.toLower . T.strip . T.pack
       in pure (filter (not . T.null) (map toLine (lines out)))
    Right _ -> pure []

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
    go (candidate:rest) = do
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

digAddrs :: Text -> IO [Text]
digAddrs name = do
  a4 <- digRR name "A"
  a6 <- digRR name "AAAA"
  pure (a4 <> a6)

dnsPointsTo :: Text -> Text -> IO Bool
dnsPointsTo vhost expectedHost = do
  let expectHostNorm = T.toLower expectedHost
  cn <- digRR vhost "CNAME"
  if expectHostNorm `elem` cn
    then pure True
    else do
      expIPs <- digAddrs expectedHost
      vhIPs <- digAddrs vhost
      pure (not (null (expIPs `intersect` vhIPs)))

resolvePrevNodeFromDns :: Maybe Text -> [Text] -> Text -> Text -> Maybe Text -> IO (Either Text (Maybe Text))
resolvePrevNodeFromDns = resolvePrevNodeFromDnsWith dnsPointsTo

resolvePrevNodeFromDnsWith :: (Text -> Text -> IO Bool) -> Maybe Text -> [Text] -> Text -> Text -> Maybe Text -> IO (Either Text (Maybe Text))
resolvePrevNodeFromDnsWith pointsTo hostenvHostname discoveryNodes envName currentNode prevNode =
  case prevNode of
    Just prev -> pure (Right (Just prev))
    Nothing ->
      case hostenvHostname of
        Nothing -> pure (Right Nothing)
        Just host -> do
          matchedNodes <- PrevNode.discoverMatchingNodes pointsTo host envName discoveryNodes
          let resolution = PrevNode.resolvePrevNodeFromMatches currentNode matchedNodes
          let envHost = PrevNode.canonicalHostInDomain envName host
          case resolution of
            PrevNode.PrevNodeResolved node -> pure (Right (Just node))
            PrevNode.PrevNodeSkip -> pure (Right Nothing)
            PrevNode.PrevNodeAmbiguousFatal nodes ->
              pure
                ( Left
                    ( "previous-node discovery for "
                        <> envName
                        <> " via "
                        <> envHost
                        <> " is ambiguous: matched nodes "
                        <> T.intercalate ", " nodes
                        <> " (current node: "
                        <> currentNode
                        <> "). Set previousNode explicitly."
                    )
                )

renderProjectInputs :: [(Text, Text)] -> Text
renderProjectInputs inputs =
  if null inputs
    then "    # (no client projects)"
    else T.intercalate "\n" (map renderInput inputs)
  where
    renderInput (name, url) =
      T.concat ["    ", name, ".url = \"", url, "\";"]

renderFlakeTemplate :: Text -> Text -> Either Text Text
renderFlakeTemplate template inputBlock =
  let marker = "{{HOSTENV_PROJECT_INPUTS}}"
   in if T.isInfixOf marker template
        then Right (T.replace marker inputBlock template)
        else Left "flake template missing {{HOSTENV_PROJECT_INPUTS}} marker"

renderGitCredentials :: [(Text, Text)] -> Text
renderGitCredentials entries =
  let lines' = map (uncurry injectToken) entries
   in T.intercalate "\n" lines' <> "\n"

injectToken :: Text -> Text -> Text
injectToken url token =
  case T.stripPrefix "https://" url of
    Just rest -> "https://oauth2:" <> token <> "@" <> rest
    Nothing ->
      case T.stripPrefix "http://" url of
        Just rest -> "http://oauth2:" <> token <> "@" <> rest
        Nothing -> url


data GitlabSecrets = GitlabSecrets
  { gitlabClientId :: Text
  , gitlabClientSecret :: Text
  } deriving (Eq, Show)

readGitlabSecrets :: FilePath -> IO GitlabSecrets
readGitlabSecrets path = do
  raw <- T.pack <$> readFile path
  let pairs = map parseLine (T.lines raw)
  let kvs = catMaybes pairs
  let lookupKey keys = listToMaybe [ v | (k, v) <- kvs, k `elem` keys ]
  case (lookupKey ["client_id", "GITLAB_CLIENT_ID"], lookupKey ["client_secret", "GITLAB_CLIENT_SECRET"]) of
    (Just cid, Just secret) -> pure (GitlabSecrets cid secret)
    _ -> die "GitLab secrets file missing client_id/client_secret"
  where
    parseLine line =
      let trimmed = T.strip line
       in if trimmed == "" || T.isPrefixOf "#" trimmed
            then Nothing
            else case T.breakOn "=" trimmed of
              (k, v) | v /= "" -> Just (T.strip k, T.strip (T.drop 1 v))
              _ -> Nothing


data WebhookConfig = WebhookConfig
  { whWorkDir :: FilePath
  , whPlanPath :: FilePath
  } deriving (Eq, Show)

data ProjectRef = ProjectRef
  { prOrg :: Text
  , prProject :: Text
  } deriving (Eq, Show)


data CommandSpec = CommandSpec
  { cmdName :: Text
  , cmdArgs :: [Text]
  , cmdCwd :: FilePath
  } deriving (Eq, Show)


data CommandOutput = CommandOutput
  { outStdout :: Text
  , outStderr :: Text
  } deriving (Eq, Show)


data CommandError = CommandError
  { errSpec :: CommandSpec
  , errExit :: ExitCode
  , errStdout :: Text
  , errStderr :: Text
  } deriving (Eq, Show)


data NodeAction = NodeAction
  { op :: Text
  , user :: Text
  , fromNode :: Maybe Text
  , toNode :: Maybe Text
  , migrations :: [Text]
  } deriving (Eq, Show)

instance A.ToJSON NodeAction where
  toJSON action =
    A.object
      [ "op" A..= action.op
      , "user" A..= action.user
      , "fromNode" A..= action.fromNode
      , "toNode" A..= action.toNode
      , "migrations" A..= action.migrations
      ]

data NodeIntent = NodeIntent
  { node :: Text
  , actions :: [NodeAction]
  } deriving (Eq, Show)

instance A.ToJSON NodeIntent where
  toJSON intent =
    A.object
      [ "node" A..= intent.node
      , "actions" A..= intent.actions
      ]

data WebhookUpdateStatus
  = WebhookUpdateCommitted
  | WebhookUpdateNoop
  deriving (Eq, Show)

instance A.ToJSON WebhookUpdateStatus where
  toJSON status =
    case status of
      WebhookUpdateCommitted -> A.String "committed"
      WebhookUpdateNoop -> A.String "noop"

data WebhookResult = WebhookResult
  { nodes :: [Text]
  , intents :: [NodeIntent]
  , commitSha :: Text
  , updateStatus :: WebhookUpdateStatus
  } deriving (Eq, Show)

instance A.ToJSON WebhookResult where
  toJSON r =
    A.object
      [ "nodes" A..= r.nodes
      , "intents" A..= r.intents
      , "commitSha" A..= r.commitSha
      , "update" A..= r.updateStatus
      ]


data WebhookStage
  = StageSyncRepo
  | StageUpdateFlake
  | StagePlan
  | StageDnsGate
  | StageLoadPlan
  | StageResolveNodes
  | StageDeriveIntents
  | StageWriteIntent
  | StageFinalizeRepo
  deriving (Eq, Show)

renderWebhookStage :: WebhookStage -> Text
renderWebhookStage stage =
  case stage of
    StageSyncRepo -> "sync_repository"
    StageUpdateFlake -> "update_flake"
    StagePlan -> "generate_plan"
    StageDnsGate -> "dns_gate"
    StageLoadPlan -> "load_plan"
    StageResolveNodes -> "resolve_nodes"
    StageDeriveIntents -> "derive_intents"
    StageWriteIntent -> "write_deploy_intent"
    StageFinalizeRepo -> "finalize_repository"

data WebhookError
  = WebhookCommandError WebhookStage CommandError
  | WebhookPlanError WebhookStage Text
  deriving (Eq, Show)


type CommandRunner = CommandSpec -> IO (Either CommandError CommandOutput)

type PlanLoader = IO BL.ByteString

type StageNotifier = WebhookStage -> IO ()

runWebhookWith :: StageNotifier -> CommandRunner -> PlanLoader -> WebhookConfig -> ProjectRef -> IO (Either WebhookError WebhookResult)
runWebhookWith notifyStage runner loadPlan cfg ref = do
  previousPlan <- readExistingPlan cfg.whPlanPath
  let inputName = ref.prOrg <> "__" <> ref.prProject
  step StageSyncRepo (CommandSpec "git" ["pull", "--rebase", "--autostash"] (cfg.whWorkDir)) >>= \case
    Left err -> pure (Left err)
    Right _ ->
      step StageUpdateFlake (CommandSpec "nix" ["flake", "update", inputName] (cfg.whWorkDir)) >>= \case
        Left err -> pure (Left err)
        Right _ ->
          step StagePlan (CommandSpec "nix" ["run", ".#hostenv-provider", "--", "plan"] (cfg.whWorkDir)) >>= \case
            Left err -> pure (Left err)
            Right _ ->
              step StageDnsGate (CommandSpec "nix" ["run", ".#hostenv-provider", "--", "dns-gate"] (cfg.whWorkDir)) >>= \case
                Left err -> pure (Left err)
                Right _ -> do
                  notifyStage StageLoadPlan
                  planRaw <- loadPlan
                  notifyStage StageResolveNodes
                  orderedNodesResult <- nodesForProjectWithDns ref.prOrg ref.prProject planRaw
                  case orderedNodesResult of
                    Left err -> pure (Left (WebhookPlanError StageResolveNodes err))
                    Right orderedNodes -> do
                      notifyStage StageDeriveIntents
                      case deriveNodeIntents previousPlan planRaw ref of
                        Left err -> pure (Left (WebhookPlanError StageDeriveIntents err))
                        Right intentsByNode -> do
                          notifyStage StageWriteIntent
                          writeIntentResult <- writeDeployIntent cfg ref orderedNodes intentsByNode
                          case writeIntentResult of
                            Left err -> pure (Left (WebhookPlanError StageWriteIntent err))
                            Right intentsOrdered -> do
                              finalizeResult <- finalizeRepoUpdate notifyStage runner cfg ref
                              case finalizeResult of
                                Left err -> pure (Left err)
                                Right (commitSha, committed) ->
                                  let updateStatus = if committed then WebhookUpdateCommitted else WebhookUpdateNoop
                                   in pure
                                        ( Right
                                            WebhookResult
                                              { nodes = map (.node) intentsOrdered
                                              , intents = intentsOrdered
                                              , commitSha = commitSha
                                              , updateStatus = updateStatus
                                              }
                                        )

  where
    step stage spec = do
      notifyStage stage
      res <- runner spec
      case res of
        Left e -> pure (Left (WebhookCommandError stage e))
        Right _ -> pure (Right ())

data EnvSnapshot = EnvSnapshot
  { userName :: Text
  , node :: Text
  , migrations :: [Text]
  , payload :: Value
  }

projectEnvironments :: ProjectRef -> BL.ByteString -> Either Text (M.Map Text EnvSnapshot)
projectEnvironments ref raw = do
  envs <- decodeEnvironments raw
  pure $
    foldl'
      (\acc (envKey, envValue) ->
        case extract envKey envValue of
          Nothing -> acc
          Just envSnapshot -> M.insert envSnapshot.userName envSnapshot acc)
      M.empty
      (KM.toList envs)
  where
    extract envKey value =
      case value of
        Object envObj -> do
          hostenvObj <- lookupObj (K.fromString "hostenv") envObj
          envOrg <- lookupText (K.fromString "organisation") hostenvObj
          envProject <- lookupText (K.fromString "project") hostenvObj
          if envOrg == ref.prOrg && envProject == ref.prProject
            then do
              envNode <- lookupText (K.fromString "node") envObj
              let envUser = fromMaybe (K.toText envKey) (lookupText (K.fromString "userName") hostenvObj)
              let envMigrations = lookupTextList (K.fromString "migrations") envObj
              pure
                EnvSnapshot
                  { userName = envUser
                  , node = envNode
                  , migrations = envMigrations
                  , payload = value
                  }
            else
              Nothing
        _ -> Nothing

deriveNodeIntents :: Maybe BL.ByteString -> BL.ByteString -> ProjectRef -> Either Text (M.Map Text [NodeAction])
deriveNodeIntents previousPlan currentPlan ref = do
  let oldSnapshots =
        case previousPlan of
          Nothing -> Right M.empty
          Just previousRaw ->
            case projectEnvironments ref previousRaw of
              Left _ -> Right M.empty
              Right parsed -> Right parsed
  oldSnapshots' <- oldSnapshots
  newSnapshots <- projectEnvironments ref currentPlan
  let allUsers = S.toList (S.union (M.keysSet oldSnapshots') (M.keysSet newSnapshots))
  pure (foldl' (deriveUserActions oldSnapshots' newSnapshots) M.empty allUsers)

deriveUserActions :: M.Map Text EnvSnapshot -> M.Map Text EnvSnapshot -> M.Map Text [NodeAction] -> Text -> M.Map Text [NodeAction]
deriveUserActions oldSnapshots newSnapshots intents userName =
  case (M.lookup userName oldSnapshots, M.lookup userName newSnapshots) of
    (Nothing, Just newEnv) ->
      addAction intents newEnv.node (NodeAction "activate" userName Nothing (Just newEnv.node) [])
    (Just oldEnv, Nothing) ->
      addAction intents oldEnv.node (NodeAction "deactivate" userName (Just oldEnv.node) Nothing [])
    (Just oldEnv, Just newEnv) ->
      if oldEnv.node /= newEnv.node
        then case newEnv.migrations of
          [] ->
            let moved =
                  addAction intents oldEnv.node (NodeAction "deactivate" userName (Just oldEnv.node) (Just newEnv.node) [])
             in addAction moved newEnv.node (NodeAction "activate" userName (Just oldEnv.node) (Just newEnv.node) [])
          migrations ->
            let withBackup =
                  addAction intents oldEnv.node (NodeAction "backup" userName (Just oldEnv.node) (Just newEnv.node) migrations)
                withDeactivate =
                  addAction withBackup oldEnv.node (NodeAction "deactivate" userName (Just oldEnv.node) (Just newEnv.node) [])
                withRestore =
                  addAction withDeactivate newEnv.node (NodeAction "restore" userName (Just oldEnv.node) (Just newEnv.node) migrations)
             in addAction withRestore newEnv.node (NodeAction "activate" userName (Just oldEnv.node) (Just newEnv.node) [])
        else if oldEnv.payload /= newEnv.payload
          then addAction intents newEnv.node (NodeAction "reload" userName (Just newEnv.node) (Just newEnv.node) [])
          else intents
    (Nothing, Nothing) -> intents

addAction :: M.Map Text [NodeAction] -> Text -> NodeAction -> M.Map Text [NodeAction]
addAction intents nodeName action =
  M.insertWith (\new old -> old <> new) nodeName [action] intents

renderDeployIntentDocument :: ProjectRef -> [NodeIntent] -> A.Value
renderDeployIntentDocument ref ordered =
  A.object
    [ "schemaVersion" A..= (1 :: Int)
    , "project" A..= A.object ["org" A..= ref.prOrg, "project" A..= ref.prProject]
    , "forceSystemRestart" A..= False
    , "nodes" A..=
        M.fromList
          [ (intent.node, A.object ["schemaVersion" A..= (1 :: Int), "actions" A..= intent.actions])
          | intent <- ordered
          ]
    ]

writeDeployIntent
  :: WebhookConfig
  -> ProjectRef
  -> [Text]
  -> M.Map Text [NodeAction]
  -> IO (Either Text [NodeIntent])
writeDeployIntent cfg ref orderedNodes intentsByNode = do
  let ordered =
        let keys = M.keys intentsByNode
            remaining = filter (`notElem` orderedNodes) (sort keys)
            nodeOrder = orderedNodes <> remaining
         in mapMaybe (\n -> fmap (\actions -> NodeIntent n actions) (M.lookup n intentsByNode)) nodeOrder
  let intentDocument = renderDeployIntentDocument ref ordered
  let generatedDir = cfg.whWorkDir </> "generated"
  createDirectoryIfMissing True generatedDir
  BL.writeFile (generatedDir </> "deploy-intent.json") (A.encode intentDocument)
  pure (Right ordered)

finalizeRepoUpdate :: StageNotifier -> CommandRunner -> WebhookConfig -> ProjectRef -> IO (Either WebhookError (Text, Bool))
finalizeRepoUpdate notifyStage runner cfg ref = do
  notifyStage StageFinalizeRepo
  step (CommandSpec "git" ["add", "-A", "generated"] cfg.whWorkDir) >>= \case
    Left err -> pure (Left err)
    Right _ ->
      step (CommandSpec "git" ["add", "-A", "flake.nix", "flake.lock"] cfg.whWorkDir) >>= \case
        Left err -> pure (Left err)
        Right _ -> do
          statusResult <- runner (CommandSpec "git" ["status", "--porcelain", "--", "generated", "flake.nix", "flake.lock"] cfg.whWorkDir)
          case statusResult of
            Left err -> pure (Left (WebhookCommandError StageFinalizeRepo err))
            Right statusOut -> do
              let hasChanges = T.strip statusOut.outStdout /= ""
              pushedResult <-
                if not hasChanges
                  then pure (Right False)
                  else do
                    let message = "hostenv-provider: deploy intent for " <> ref.prOrg <> "/" <> ref.prProject
                    step (CommandSpec "git" ["commit", "-m", message] cfg.whWorkDir) >>= \case
                      Left err -> pure (Left err)
                      Right _ -> pure (Right True)
              case pushedResult of
                Left err -> pure (Left err)
                Right pushed -> do
                  headResult <- runner (CommandSpec "git" ["rev-parse", "HEAD"] cfg.whWorkDir)
                  case headResult of
                    Left err -> pure (Left (WebhookCommandError StageFinalizeRepo err))
                    Right out ->
                      pure (Right (T.strip out.outStdout, pushed))
  where
    step spec = do
      res <- runner spec
      case res of
        Left e -> pure (Left (WebhookCommandError StageFinalizeRepo e))
        Right _ -> pure (Right ())

readExistingPlan :: FilePath -> IO (Maybe BL.ByteString)
readExistingPlan path = do
  result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
  case result of
    Left _ -> pure Nothing
    Right bytes -> pure (Just bytes)
