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
  , DeployResult(..)
  , WebhookResult(..)
  , WebhookError(..)
  , CommandRunner
  , PlanLoader
  , runWebhookWith
  ) where

import Control.Exception (IOException, try)
import Control.Monad (foldM, forM)
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
import Data.List (foldl', intersect, nub, sort, sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..), die)
import System.Process (readProcessWithExitCode)

import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC (HMAC (..), hmac)
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
nodesForProjectWithDns org project raw = do
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
                            pure (Just (envUserName, node, prevNode))
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
                    filter (not . T.null) (rootNodeNames <> map (\(_, node, _) -> node) matches)

          resolvedMatches <- forM matches $ \(envUserName, node, prevNode) -> do
            resolvedPrev <- resolvePrevNodeFromDns hostenvHostname discoveryNodes envUserName node prevNode
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

digRR :: Text -> Text -> IO [Text]
digRR name rr = do
  let args = ["+short", T.unpack name, T.unpack rr]
  res <- try (readProcessWithExitCode "dig" args "") :: IO (Either IOException (ExitCode, String, String))
  case res of
    Left _ -> pure []
    Right (ExitSuccess, out, _) ->
      let toLine = stripDot . T.toLower . T.strip . T.pack
       in pure (filter (not . T.null) (map toLine (lines out)))
    Right _ -> pure []

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
resolvePrevNodeFromDns hostenvHostname discoveryNodes envName currentNode prevNode =
  case prevNode of
    Just prev -> pure (Right (Just prev))
    Nothing ->
      case hostenvHostname of
        Nothing -> pure (Right Nothing)
        Just host -> do
          matchedNodes <- PrevNode.discoverMatchingNodes dnsPointsTo host envName discoveryNodes
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


data DeployResult = DeployResult
  { deployNode :: Text
  , deploySuccess :: Bool
  , deployStdout :: Text
  , deployStderr :: Text
  } deriving (Eq, Show)

instance A.ToJSON DeployResult where
  toJSON d =
    A.object
      [ "node" A..= d.deployNode
      , "success" A..= d.deploySuccess
      , "stdout" A..= d.deployStdout
      , "stderr" A..= d.deployStderr
      ]


data WebhookResult = WebhookResult
  { webhookNodes :: [Text]
  , webhookDeploys :: [DeployResult]
  , webhookOk :: Bool
  } deriving (Eq, Show)

instance A.ToJSON WebhookResult where
  toJSON r =
    A.object
      [ "ok" A..= r.webhookOk
      , "nodes" A..= r.webhookNodes
      , "deployments" A..= r.webhookDeploys
      ]


data WebhookError
  = WebhookCommandError CommandError
  | WebhookPlanError Text
  deriving (Eq, Show)


type CommandRunner = CommandSpec -> IO (Either CommandError CommandOutput)

type PlanLoader = IO BL.ByteString

runWebhookWith :: CommandRunner -> PlanLoader -> WebhookConfig -> ProjectRef -> IO (Either WebhookError WebhookResult)
runWebhookWith runner loadPlan cfg ref = do
  let inputName = ref.prOrg <> "__" <> ref.prProject
  step runner (CommandSpec "nix" ["flake", "update", inputName] (cfg.whWorkDir)) >>= \case
    Left err -> pure (Left err)
    Right _ ->
      step runner (CommandSpec "nix" ["run", ".#hostenv-provider", "--", "plan"] (cfg.whWorkDir)) >>= \case
        Left err -> pure (Left err)
        Right _ ->
          step runner (CommandSpec "nix" ["run", ".#hostenv-provider", "--", "dns-gate"] (cfg.whWorkDir)) >>= \case
            Left err -> pure (Left err)
            Right _ -> do
              planRaw <- loadPlan
              nodesForProjectWithDns ref.prOrg ref.prProject planRaw >>= \case
                Left err -> pure (Left (WebhookPlanError err))
                Right nodes -> do
                  deploys <- foldM (deployNode runner cfg) [] nodes
                  let ok = all (\d -> d.deploySuccess) deploys
                  pure (Right (WebhookResult nodes deploys ok))
  where
    step run spec = do
      res <- run spec
      case res of
        Left e -> pure (Left (WebhookCommandError e))
        Right _ -> pure (Right ())

    deployNode run config acc node = do
      res <- run (CommandSpec "nix" ["run", ".#hostenv-provider", "--", "deploy", "--node", node] (config.whWorkDir))
      case res of
        Right out ->
          pure (acc ++ [DeployResult node True out.outStdout out.outStderr])
        Left err ->
          pure (acc ++ [DeployResult node False err.errStdout err.errStderr])
