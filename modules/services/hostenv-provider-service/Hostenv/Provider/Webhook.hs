{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Webhook
  ( webhookHandler
  , loadPlan
  , WebhookAccepted(..)
  , runWebhookDeployJob
  , shouldWaitForCallbacks
  , persistIntentsActionsAndPushWith
  , chooseFinalResult
  ) where

import Control.Exception (finally, IOException, try)
import Control.Monad (forM, forM_, unless, when, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Only (..), fromOnly, query)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesFileExist, doesPathExist, listDirectory, removeFile, removePathForcibly)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, hGetLine, hIsEOF)
import Data.Time (formatTime, getCurrentTime, defaultTimeLocale)
import System.Posix.Files (createSymbolicLink)
import System.Process (readProcessWithExitCode)
import System.Process (CreateProcess(..), StdStream(CreatePipe), createProcess, getProcessExitCode, interruptProcessGroupOf, proc, waitForProcess)
import Servant

import Hostenv.Provider.Command (commandErrorText, exitCodeToInt, renderCommand, runCommandWithEnv)
import Hostenv.Provider.Config (AppConfig(..), DeployConfig(..))
import Hostenv.Provider.DB (DeployCredential(..), appendDeployEvent, loadDeployCredentialByHash, saveDeployActions, saveDeployIntents, withDb)
import Hostenv.Provider.Gitlab (GitlabDeployToken(..), NixGitlabTokenType(..), appendNixAccessTokenConfig, createProjectDeployToken, ensureProjectCredential, renderGitlabError, revokeProjectToken)
import Hostenv.Provider.Http (ErrorResponse(..), errorWithBody)
import Hostenv.Provider.Jobs (JobLogger(..), JobOutcome(..), JobRuntime, NewJob(..), enqueueJob)
import Hostenv.Provider.Repo (RepoStatus(..))
import Hostenv.Provider.Service
  ( CommandError(..)
  , CommandSpec(..)
  , CommandOutput(..)
  , NodeAction(..)
  , NodeIntent(..)
  , ProjectRef(..)
  , WebhookConfig(..)
  , WebhookError(..)
  , WebhookUpdateStatus(..)
  , WebhookResult(..)
  , WebhookStage(..)
  , renderWebhookStage
  , projectForHash
  , runWebhookWith
  , verifyGitHubSignature
  , verifyGitLabToken
  )
import Hostenv.Provider.Util (pickFirstExisting, readSecret)

data WebhookAccepted = WebhookAccepted
  { accepted :: Bool
  , jobId :: Text
  } deriving (Eq, Show)

instance A.ToJSON WebhookAccepted where
  toJSON response =
    A.object
      [ "accepted" A..= response.accepted
      , "jobId" A..= response.jobId
      ]

loadPlan :: AppConfig -> IO BL.ByteString
loadPlan cfg =
  let AppConfig { appWebhookConfig = WebhookConfig { whPlanPath = planPath } } = cfg
   in BL.readFile planPath

webhookHandler :: JobRuntime -> IORef RepoStatus -> AppConfig -> Text -> Maybe Text -> Maybe Text -> BL.ByteString -> Handler WebhookAccepted
webhookHandler runtime repoStatusRef cfg hash mHubSig mGitlabToken rawBody = do
  repoStatus <- liftIO (readIORef repoStatusRef)
  case repoStatus of
    RepoMissing ->
      throwError
        ( errorWithBody
            err503
            (ErrorResponse "provider repository is not initialized; bootstrap it via the dashboard first" Nothing Nothing Nothing Nothing)
        )
    RepoReady -> pure ()
  planRaw <- liftIO (loadPlan cfg)
  projectRef <-
    case projectForHash hash planRaw of
      Left msg ->
        if msg == "webhook hash not found in plan.json"
          then throwError err404
          else throwError (errorWithBody err500 (ErrorResponse msg Nothing Nothing Nothing Nothing))
      Right ref -> pure ref
  secretInfo <- liftIO (resolveSecret cfg hash projectRef)
  verifyWebhook secretInfo mHubSig mGitlabToken rawBody
  queuedJobId <- liftIO $ enqueueJob runtime $
    NewJob
      { kind = "webhook_deploy"
      , requestedByUserId = Nothing
      , projectId = Nothing
      , payload =
          A.object
            [ "hash" A..= hash
            , "org" A..= projectRef.prOrg
            , "project" A..= projectRef.prProject
            ]
      , run = runWebhookDeployJob cfg hash projectRef
      }
  pure (WebhookAccepted True queuedJobId)

runWebhookDeployJob :: AppConfig -> Text -> ProjectRef -> JobLogger -> IO (Either Text JobOutcome)
runWebhookDeployJob cfg hash projectRef logger = do
  logger.logInfo ("Running webhook deployment for hash " <> hash)
  logger.logInfo "webhook stage: load_project_credential"
  deployCredResult <- withDb cfg (\conn -> loadDeployCredentialByHash cfg conn hash)
  case deployCredResult of
    Left err -> pure (Left err)
    Right maybeCred -> do
      existingNixConfig <- fmap (fmap T.pack) (lookupEnv "NIX_CONFIG")
      case maybeCred of
        Nothing -> do
          logger.logInfo "webhook stage: execute_pipeline"
          executePipeline existingNixConfig
        Just cred -> do
          logger.logInfo "webhook stage: refresh_project_credential"
          refreshedCredResult <- ensureProjectCredential cfg cred
          case refreshedCredResult of
            Left err -> pure (Left (renderGitlabError err))
            Right refreshedCred -> do
              let DeployCredential { host = deployHost, accessToken = deployAccessToken, repoId = deployRepoId } = refreshedCred
              logger.logInfo "webhook stage: create_deploy_token"
              deployTokenResult <- createProjectDeployToken cfg deployHost deployAccessToken deployRepoId
              case deployTokenResult of
                Left err -> pure (Left (renderGitlabError err))
                Right deployToken -> do
                  let scopedNixConfig = appendNixAccessTokenConfig existingNixConfig deployHost NixGitlabPAT deployToken.value
                  revokeErrRef <- newIORef Nothing
                  logger.logInfo "webhook stage: execute_pipeline"
                  pipelineResult <-
                    executePipeline (Just scopedNixConfig)
                      `finally` do
                        logger.logInfo "webhook stage: revoke_deploy_token"
                        revokeResult <- revokeProjectToken cfg deployHost deployAccessToken deployRepoId deployToken.id
                        case revokeResult of
                          Left msg -> writeIORef revokeErrRef (Just msg)
                          Right _ -> pure ()
                  revokeErr <- readIORef revokeErrRef
                  case pipelineResult of
                    Left err -> pure (Left err)
                    Right outcome -> do
                      case revokeErr of
                        Nothing -> pure ()
                        Just revokeMsg -> logger.logError ("failed to revoke deploy token: " <> renderGitlabError revokeMsg)
                      pure (Right outcome)
  where
    executePipeline :: Maybe Text -> IO (Either Text JobOutcome)
    executePipeline mNixConfig = do
      let AppConfig { appWebhookConfig = webhookCfg } = cfg
          envVars = maybe [] (\cfgText -> [("NIX_CONFIG", cfgText)]) mNixConfig
      supersededBeforePipeline <- isSupersededWebhookJob cfg logger.jobId hash
      case supersededBeforePipeline of
        Left err -> pure (Left err)
        Right () -> do
          result <-
            runWebhookWith
              (\stage -> logger.logInfo ("webhook stage: " <> renderWebhookStage stage))
              (runCommandWithSupersedeGuard cfg envVars logger.jobId hash)
              (loadPlan cfg)
              webhookCfg
              projectRef
          case result of
            Left err -> pure (Left (renderWebhookError err))
            Right okResult -> do
              let baseCommitSha = okResult.commitSha
              let updateStatus = okResult.updateStatus
              let intents = okResult.intents
              let DeployConfig deployEnabled _ = cfg.appDeploy
              let shouldWait = shouldWaitForCallbacks updateStatus deployEnabled intents
              let pushCommittedUpdate = do
                    pushResult <- runCommandWithSupersedeGuard cfg envVars logger.jobId hash (CommandSpec "git" ["push"] webhookCfg.whWorkDir)
                    pure
                      ( case pushResult of
                          Left cmdErr -> Left (renderWebhookError (WebhookCommandError StageFinalizeRepo cmdErr))
                          Right _ -> Right ()
                      )
              case updateStatus of
                WebhookUpdateCommitted -> do
                  buildResult <-
                    if null intents
                      then pure (Right emptyArtifacts)
                      else buildArtifactsForIntents cfg logger envVars webhookCfg logger.jobId hash intents
                  case buildResult of
                    Left err -> pure (Left err)
                    Right artifacts -> do
                      commitResult <- commitPreparedUpdate cfg envVars webhookCfg logger.jobId hash projectRef
                      case commitResult of
                        Left err -> pure (Left err)
                        Right commitSha ->
                          if shouldWait
                            then do
                              case renderPayloadsWithArtifacts commitSha artifacts intents of
                                Left err -> pure (Left err)
                                Right payloads -> do
                                  persistAndPushResult <-
                                    persistIntentsActionsAndPushWith
                                      (saveDeployIntents cfg logger.jobId commitSha payloads)
                                      (saveDeployActions cfg logger.jobId payloads)
                                      (forM_ intents (\intent -> appendDeployEvent cfg logger.jobId intent.node "queued" (Just "intent") (Just "Deploy intent queued") Nothing >> pure ()))
                                      pushCommittedUpdate
                                  case persistAndPushResult of
                                    Left err -> pure (Left err)
                                    Right () -> pure (Right (JobWaiting ("Webhook deployment prepared at " <> commitSha <> " and waiting for node callbacks")))
                            else do
                              pushResult <- pushCommittedUpdate
                              case pushResult of
                                Left err -> pure (Left err)
                                Right () -> pure (Right (JobComplete ("Webhook deployment prepared and pushed at " <> commitSha)))
                WebhookUpdateNoop ->
                  pure
                    ( Right
                        (JobComplete ("Webhook deployment intent updated without new commit (HEAD " <> baseCommitSha <> ")"))
                    )


shouldWaitForCallbacks :: WebhookUpdateStatus -> Bool -> [NodeIntent] -> Bool
shouldWaitForCallbacks updateStatus deployEnabled intents =
  updateStatus == WebhookUpdateCommitted && deployEnabled && not (null intents)

persistIntentsActionsAndPushWith :: IO () -> IO () -> IO () -> IO (Either Text ()) -> IO (Either Text ())
persistIntentsActionsAndPushWith persistIntents persistActions appendQueuedEvents pushCommit = do
  persistIntents
  persistActions
  appendQueuedEvents
  pushCommit

data BuiltArtifacts = BuiltArtifacts
  { systemPaths :: Map.Map Text Text
  , envPaths :: Map.Map Text Text
  }

emptyArtifacts :: BuiltArtifacts
emptyArtifacts = BuiltArtifacts Map.empty Map.empty

buildArtifactsForIntents :: AppConfig -> JobLogger -> [(Text, Text)] -> WebhookConfig -> Text -> Text -> [NodeIntent] -> IO (Either Text BuiltArtifacts)
buildArtifactsForIntents cfg logger envVars webhookCfg jobId hash intents = do
  supersededBeforeBuild <- isSupersededWebhookJob cfg logger.jobId hash
  case supersededBeforeBuild of
    Left err -> pure (Left err)
    Right () -> do
      changedNodeNamesResult <- loadChangedNodes cfg webhookCfg intents
      case changedNodeNamesResult of
        Left err -> pure (Left err)
        Right changedNodeNames -> do
          nodePairsResult <-
            mapM
              (\nodeName ->
                fmap (fmap (\storePath -> (nodeName, storePath)))
                  (buildAttrOutPathWithGuard cfg envVars webhookCfg jobId hash ("./generated#node-" <> nodeName)))
              changedNodeNames
          case sequence nodePairsResult of
            Left err -> pure (Left err)
            Right nodePairs -> do
              let requiredUsers =
                    sort
                      ( nub
                          [ action.user
                          | intent <- intents
                          , action <- intent.actions
                          , requiresStorePath (T.toLower action.op)
                          ]
                      )
              envPairsResult <-
                mapM
                  (\userName ->
                    fmap (fmap (\storePath -> (userName, storePath)))
                      (buildAttrOutPathWithGuard cfg envVars webhookCfg jobId hash ("./generated#env-" <> userName)))
                  requiredUsers
              case sequence envPairsResult of
                Left err -> pure (Left err)
                Right envPairs -> do
                  let builtStorePaths = map snd (nodePairs ++ envPairs)
                  signResult <- maybeSignArtifacts cfg logger envVars webhookCfg jobId hash builtStorePaths
                  case signResult of
                    Left err -> pure (Left err)
                    Right () -> do
                      rootResult <- addGcRoots cfg jobId hash nodePairs envPairs
                      case rootResult of
                        Left err -> pure (Left err)
                        Right () ->
                          pure
                            ( Right
                                BuiltArtifacts
                                  { systemPaths = Map.fromList nodePairs
                                  , envPaths = Map.fromList envPairs
                                  }
                            )

renderPayloadsWithArtifacts :: Text -> BuiltArtifacts -> [NodeIntent] -> Either Text [(Text, A.Value)]
renderPayloadsWithArtifacts commitSha artifacts intents =
  sequence (map (buildNodePayload commitSha artifacts.systemPaths artifacts.envPaths) intents)

loadChangedNodes :: AppConfig -> WebhookConfig -> [NodeIntent] -> IO (Either Text [Text])
loadChangedNodes cfg webhookCfg intents = do
  currentPlanRaw <- loadPlan cfg
  let currentNodesResult = decodePlanNodes currentPlanRaw
  previousPlanResult <- loadPreviousPlanFromGit webhookCfg.whWorkDir
  case (currentNodesResult, previousPlanResult) of
    (Left err, _) -> pure (Left err)
    (Right _, Left err) -> pure (Left err)
    (Right currentNodes, Right mPreviousRaw) ->
      case mPreviousRaw of
        Nothing -> pure (Right (sort (nub (map (.node) intents))))
        Just previousRaw ->
          case decodePlanNodes previousRaw of
            Left err -> pure (Left err)
            Right previousNodes ->
              let changed =
                    [ nodeName
                    | nodeName <- sort (nub (map (.node) intents))
                    , Map.lookup nodeName currentNodes /= Map.lookup nodeName previousNodes
                    ]
               in pure (Right changed)

decodePlanNodes :: BL.ByteString -> Either Text (Map.Map Text A.Value)
decodePlanNodes raw =
  case A.eitherDecode' raw of
    Left err -> Left ("failed to decode plan.json for node diffing: " <> T.pack err)
    Right (A.Object root) ->
      case KM.lookup (K.fromString "nodes") root of
        Just (A.Object nodesObj) ->
          Right (Map.fromList [(K.toText key, value) | (key, value) <- KM.toList nodesObj])
        _ -> Right Map.empty
    Right _ -> Left "plan.json root is not an object"

loadPreviousPlanFromGit :: FilePath -> IO (Either Text (Maybe BL.ByteString))
loadPreviousPlanFromGit workDir = do
  result <- try (readProcessWithExitCode "git" ["-C", workDir, "show", "HEAD^:generated/plan.json"] "") :: IO (Either IOException (ExitCode, String, String))
  pure
    ( case result of
        Right (ExitSuccess, stdoutText, _) -> Right (Just (BL.fromStrict (TE.encodeUtf8 (T.pack stdoutText))))
        Right (_, _, stderrText) ->
          let stderrLower = T.toLower (T.pack stderrText)
           in if "invalid object name" `T.isInfixOf` stderrLower || "path 'generated/plan.json' does not exist" `T.isInfixOf` stderrLower
                then Right Nothing
                else Left ("failed to read previous plan from git: " <> T.pack stderrText)
        Left err -> Left ("failed to read previous plan from git: " <> T.pack (show err))
    )

commitPreparedUpdate :: AppConfig -> [(Text, Text)] -> WebhookConfig -> Text -> Text -> ProjectRef -> IO (Either Text Text)
commitPreparedUpdate cfg envVars webhookCfg jobId hash projectRef = do
  superseded <- isSupersededWebhookJob cfg jobId hash
  case superseded of
    Left err -> pure (Left err)
    Right () -> do
      let message = "hostenv-provider: deploy intent for " <> projectRef.prOrg <> "/" <> projectRef.prProject
      commitResult <- runCommandWithEnv cfg envVars (CommandSpec "git" ["commit", "-m", message] webhookCfg.whWorkDir)
      case commitResult of
        Left err -> pure (Left ("failed to commit prepared provider update: " <> commandErrorText err))
        Right _ -> do
          headResult <- runCommandWithEnv cfg envVars (CommandSpec "git" ["rev-parse", "HEAD"] webhookCfg.whWorkDir)
          pure
            ( case headResult of
                Left err -> Left ("failed to resolve committed provider revision: " <> commandErrorText err)
                Right out -> Right (T.strip out.outStdout)
            )

buildNodePayload :: Text -> Map.Map Text Text -> Map.Map Text Text -> NodeIntent -> Either Text (Text, A.Value)
buildNodePayload commitSha systemPaths envPaths intent = do
  renderedActions <- mapM (buildActionPayload envPaths) intent.actions
  let baseFields =
        [ "schemaVersion" A..= (1 :: Int)
        , "commitSha" A..= commitSha
        , "actions" A..= renderedActions
        ]
      fieldsWithSystem =
        case Map.lookup intent.node systemPaths of
          Just storePath -> baseFields <> ["systemPath" A..= storePath]
          Nothing -> baseFields
  pure
    ( intent.node
    , A.object fieldsWithSystem
    )

buildActionPayload :: Map.Map Text Text -> NodeAction -> Either Text A.Value
buildActionPayload envPaths action =
  let baseObject =
        case A.toJSON action of
          A.Object obj -> obj
          _ -> KM.empty
      opName = T.toLower action.op
   in if requiresStorePath opName
        then
          case Map.lookup action.user envPaths of
            Nothing -> Left ("missing built environment store path for user " <> action.user)
            Just storePath -> Right (A.Object (KM.insert (K.fromString "storePath") (A.String storePath) baseObject))
        else Right (A.Object baseObject)

requiresStorePath :: Text -> Bool
requiresStorePath opName = opName `elem` ["activate", "reload", "backup", "restore"]

buildAttrOutPathWithGuard :: AppConfig -> [(Text, Text)] -> WebhookConfig -> Text -> Text -> Text -> IO (Either Text Text)
buildAttrOutPathWithGuard cfg envVars webhookCfg jobId hash attrRef = do
  superseded <- isSupersededWebhookJob cfg jobId hash
  case superseded of
    Left err -> pure (Left err)
    Right () -> do
      commandResult <- runCancelableBuildCommand cfg envVars jobId hash (CommandSpec "nix" ["build", "--no-link", "--print-out-paths", attrRef] webhookCfg.whWorkDir)
      pure
        ( case commandResult of
            Left err -> Left (renderWebhookError (WebhookCommandError StageFinalizeRepo err))
            Right output ->
              let paths = filter (\line -> line /= "") (map T.strip (T.lines output.outStdout))
               in case paths of
                    (storePath:_) -> Right storePath
                    [] -> Left ("nix build produced no output paths for " <> attrRef)
        )

maybeSignArtifacts :: AppConfig -> JobLogger -> [(Text, Text)] -> WebhookConfig -> Text -> Text -> [Text] -> IO (Either Text ())
maybeSignArtifacts cfg logger envVars webhookCfg jobId hash storePaths = do
  superseded <- isSupersededWebhookJob cfg jobId hash
  case superseded of
    Left err -> pure (Left err)
    Right () -> do
      if null storePaths
        then pure (Right ())
        else do
          let signingRequired = cfg.appDeploy.enable
          currentUser <- lookupEnv "USER"
          case currentUser of
            Nothing ->
              if signingRequired
                then pure (Left "cache signing is required but USER is unset")
                else pure (Right ())
            Just userName -> do
              let keyPath = "/run/secrets/" <> userName <> "/cache_signing_key"
              keyExists <- doesFileExist keyPath
              if not keyExists
                then
                  if signingRequired
                    then pure (Left ("cache signing key not found at " <> T.pack keyPath))
                    else pure (Right ())
                else do
                  logger.logInfo "webhook stage: sign_artifacts"
                  signResults <-
                    forM
                      storePaths
                      (\storePath ->
                        runCommandWithEnv
                          cfg
                          envVars
                          (CommandSpec "nix" ["store", "sign", "--key-file", T.pack keyPath, "--recursive", storePath] webhookCfg.whWorkDir))
                  pure
                    ( case [err | Left err <- signResults] of
                        (firstErr:_) -> Left (renderWebhookError (WebhookCommandError StageFinalizeRepo firstErr))
                        [] -> Right ()
                    )

addGcRoots :: AppConfig -> Text -> Text -> [(Text, Text)] -> [(Text, Text)] -> IO (Either Text ())
addGcRoots cfg jobId hash systemPairs envPairs = do
  superseded <- isSupersededWebhookJob cfg jobId hash
  case superseded of
    Left err -> pure (Left err)
    Right () -> do
      systemResults <- mapM (\(name, storePath) -> addGcRoot cfg "system" name storePath) systemPairs
      envResults <- mapM (\(name, storePath) -> addGcRoot cfg "env" name storePath) envPairs
      pure
        ( case [err | Left err <- systemResults ++ envResults] of
            (firstErr:_) -> Left firstErr
            [] -> Right ()
        )

addGcRoot :: AppConfig -> FilePath -> Text -> Text -> IO (Either Text ())
addGcRoot cfg kind entryName storePath = do
  if not (isSafeGcRootName entryName)
    then pure (Left ("unsafe gc-root entry name: " <> entryName))
    else do
      let rootDir = cfg.appDataDir </> "gc-roots" </> kind
          historyDir = cfg.appDataDir </> "gc-roots-history" </> kind </> T.unpack entryName
          currentLink = rootDir </> T.unpack entryName
      createDirectoryIfMissing True rootDir
      createDirectoryIfMissing True historyDir
      now <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale "%Y%m%d%H%M%S" now
          historyLink = historyDir </> timestamp
      removePathIfPresent historyLink
      historyResult <- try (createSymbolicLink (T.unpack storePath) historyLink) :: IO (Either IOException ())
      case historyResult of
        Left err -> pure (Left ("failed to create gc-root history link: " <> T.pack (show err)))
        Right () -> do
          entries <- sort <$> listDirectory historyDir
          let keepCount = 5
              dropCount = max 0 (length entries - keepCount)
              staleEntries = take dropCount entries
          forM_ staleEntries (\entry -> removePathIfPresent (historyDir </> entry))
          removePathIfPresent currentLink
          currentResult <- runCommandWithEnv cfg [] (CommandSpec "nix-store" ["--add-root", T.pack currentLink, "--indirect", "--realise", storePath] cfg.appDataDir)
          pure
            ( case currentResult of
                Left err -> Left ("failed to add gc-root link: " <> commandErrorText err)
                Right _ -> Right ()
            )

isSafeGcRootName :: Text -> Bool
isSafeGcRootName name =
  let trimmed = T.strip name
      isAllowed ch =
        (ch >= 'a' && ch <= 'z')
          || (ch >= '0' && ch <= '9')
          || ch == '-'
          || ch == '_'
          || ch == '.'
          || ch == ':'
   in trimmed /= ""
        && trimmed /= "."
        && trimmed /= ".."
        && T.all isAllowed trimmed

removePathIfPresent :: FilePath -> IO ()
removePathIfPresent path = do
  exists <- doesPathExist path
  when exists $ do
    removeFileResult <- try (removeFile path) :: IO (Either IOException ())
    case removeFileResult of
      Right () -> pure ()
      Left _ -> removePathForcibly path

runCommandWithSupersedeGuard :: AppConfig -> [(Text, Text)] -> Text -> Text -> CommandSpec -> IO (Either CommandError CommandOutput)
runCommandWithSupersedeGuard cfg envVars jobId hash spec = do
  superseded <- isSupersededWebhookJob cfg jobId hash
  case superseded of
    Left msg ->
      pure
        ( Left
            CommandError
              { errSpec = spec
              , errExit = commandFailureExitCode
              , errStdout = ""
              , errStderr = msg
              }
        )
    Right () -> runCommandWithEnv cfg envVars spec

runCancelableBuildCommand :: AppConfig -> [(Text, Text)] -> Text -> Text -> CommandSpec -> IO (Either CommandError CommandOutput)
runCancelableBuildCommand cfg envVars jobId hash spec = do
  baseEnv <- getEnvironment
  let mergedEnv = applyEnvOverrides baseEnv envVars
      process =
        (proc (T.unpack spec.cmdName) (map T.unpack spec.cmdArgs))
          { cwd = Just spec.cmdCwd
          , env = Just mergedEnv
          , std_out = CreatePipe
          , std_err = CreatePipe
          , create_group = True
          }
  (_, mStdout, mStderr, ph) <- createProcess process
  outRef <- newIORef ([] :: [Text])
  errRef <- newIORef ([] :: [Text])
  doneStdout <- newEmptyMVar
  doneStderr <- newEmptyMVar
  case mStdout of
    Nothing -> putMVar doneStdout ()
    Just stdoutHandle ->
      void (forkIO (readHandleLines stdoutHandle (\line -> appendLine outRef line) `finally` putMVar doneStdout ()))
  case mStderr of
    Nothing -> putMVar doneStderr ()
    Just stderrHandle ->
      void (forkIO (readHandleLines stderrHandle (\line -> appendLine errRef line) `finally` putMVar doneStderr ()))
  code <- waitWithSupersedeGuard ph
  takeMVar doneStdout
  takeMVar doneStderr
  out <- renderLines <$> readIORef outRef
  err <- renderLines <$> readIORef errRef
  pure
    ( case code of
        ExitSuccess -> Right (CommandOutput out err)
        _ -> Left (CommandError spec code out err)
    )
  where
    waitWithSupersedeGuard ph = do
      superseded <- isSupersededWebhookJob cfg jobId hash
      case superseded of
        Left _ -> do
          interruptProcessGroupOf ph
          _ <- waitForProcess ph
          pure commandFailureExitCode
        Right () -> do
          mCode <- getProcessExitCode ph
          case mCode of
            Just code -> pure code
            Nothing -> do
              threadDelay 1000000
              waitWithSupersedeGuard ph

applyEnvOverrides :: [(String, String)] -> [(Text, Text)] -> [(String, String)]
applyEnvOverrides base overrides =
  let overridePairs = map (\(k, v) -> (T.unpack k, T.unpack v)) overrides
      baseMap = Map.fromList base
      overrideMap = Map.fromList overridePairs
   in Map.toList (Map.union overrideMap baseMap)

appendLine :: IORef [Text] -> Text -> IO ()
appendLine ref line =
  writeIORef ref =<< ((<> [line]) <$> readIORef ref)

renderLines :: [Text] -> Text
renderLines linesSoFar =
  if null linesSoFar
    then ""
    else T.unlines linesSoFar

readHandleLines :: Handle -> (Text -> IO ()) -> IO ()
readHandleLines streamHandle onLine = do
  eof <- hIsEOF streamHandle
  if eof
    then hClose streamHandle
    else do
      line <- T.pack <$> hGetLine streamHandle
      onLine line
      readHandleLines streamHandle onLine

isSupersededWebhookJob :: AppConfig -> Text -> Text -> IO (Either Text ())
isSupersededWebhookJob cfg jobId hash =
  withDb cfg $ \conn -> do
    rows <-
      query
        conn
        "SELECT EXISTS (SELECT 1 FROM jobs newer JOIN jobs current ON current.id = ? WHERE newer.kind = 'webhook_deploy' AND newer.id <> current.id AND newer.status IN ('queued','running','waiting') AND newer.payload ->> 'hash' = ? AND newer.created_at > current.created_at)"
        (jobId, hash)
    pure
      ( case rows of
          (Only True:_) -> Left ("webhook deployment superseded by newer job for hash " <> hash)
          _ -> Right ()
      )

commandFailureExitCode :: ExitCode
commandFailureExitCode = ExitFailure 1


-- Webhook signature resolution

data SecretInfo = SecretInfo
  { configured :: Bool
  , value :: Maybe BS.ByteString
  }

verifyWebhook :: SecretInfo -> Maybe Text -> Maybe Text -> BL.ByteString -> Handler ()
verifyWebhook secretInfo mHubSig mGitlabToken rawBody = do
  let hasGitHub = mHubSig /= Nothing
  let hasGitLab = mGitlabToken /= Nothing
  case secretInfo.value of
    Nothing ->
      if hasGitHub || hasGitLab
        then throwError (errorWithBody err401 (ErrorResponse "webhook secret not configured" Nothing Nothing Nothing Nothing))
        else throwError (errorWithBody err401 (ErrorResponse "webhook secret not configured" Nothing Nothing Nothing Nothing))
    Just secret ->
      case (mHubSig, mGitlabToken) of
        (Nothing, Nothing) ->
          throwError (errorWithBody err401 (ErrorResponse "missing webhook signature" Nothing Nothing Nothing Nothing))
        _ -> do
          let githubOk = maybe False (\sigText -> verifyGitHubSignature secret rawBody (TE.encodeUtf8 sigText)) mHubSig
          let gitlabOk = maybe False (\tokText -> verifyGitLabToken secret (TE.encodeUtf8 tokText)) mGitlabToken
          unless (githubOk || gitlabOk) $
            throwError (errorWithBody err401 (ErrorResponse "invalid webhook signature" Nothing Nothing Nothing Nothing))

resolveSecret :: AppConfig -> Text -> ProjectRef -> IO SecretInfo
resolveSecret cfg hash ref = do
  let AppConfig
        { appDbConnString = dbConn
        , appWebhookSecretsDir = secretsDir
        , appWebhookSecretFile = secretFile
        } = cfg
  dbSecret <- case dbConn of
    Nothing -> pure Nothing
    Just _ -> withDb cfg $ \conn -> do
      rows <- query conn "SELECT secret FROM webhooks JOIN projects ON webhooks.project_id = projects.id WHERE projects.default_env_hash = ?" (Only hash)
      pure (listToMaybe (map fromOnly rows))
  case dbSecret of
    Just secret -> pure (SecretInfo True (Just (TE.encodeUtf8 secret)))
    Nothing -> do
      fromDir <- case secretsDir of
        Nothing -> pure Nothing
        Just dir -> do
          let ProjectRef { prOrg = org, prProject = project } = ref
          let byHash = dir </> T.unpack hash
          let byProject = dir </> T.unpack (org <> "__" <> project)
          pickFirstExisting [byHash, byProject]
      case fromDir of
        Just secret -> pure (SecretInfo True (Just secret))
        Nothing ->
          case secretFile of
            Nothing -> pure (SecretInfo False Nothing)
            Just path -> do
              secret <- readSecret path
              pure (SecretInfo True (Just secret))


-- Webhook error -> HTTP

serverError :: WebhookError -> ServerError
serverError err =
  case err of
    WebhookPlanError stage msg ->
      errorWithBody
        err500
        (ErrorResponse ("webhook stage " <> renderWebhookStage stage <> " failed: " <> msg) Nothing Nothing Nothing Nothing)
    WebhookCommandError stage cmdErr ->
      let CommandError spec exitCodeRaw stdoutText stderrText = cmdErr
          cmd = renderCommand spec
          exitCode = exitCodeToInt exitCodeRaw
          response = ErrorResponse ("webhook stage " <> renderWebhookStage stage <> " failed") (Just cmd) exitCode (Just stdoutText) (Just stderrText)
       in errorWithBody err500 response

chooseFinalResult :: Either WebhookError WebhookResult -> Maybe Text -> Either (Either WebhookError Text) WebhookResult
chooseFinalResult primaryResult mRevokeError =
  case primaryResult of
    Left primaryErr -> Left (Left primaryErr)
    Right successResult ->
      case mRevokeError of
        Just revokeErr -> Left (Right revokeErr)
        Nothing -> Right successResult

renderWebhookError :: WebhookError -> Text
renderWebhookError err =
  case err of
    WebhookPlanError stage msg ->
      "webhook stage " <> renderWebhookStage stage <> " failed: " <> msg
    WebhookCommandError stage cmdErr ->
      "webhook stage " <> renderWebhookStage stage <> " failed: " <> commandErrorText cmdErr
