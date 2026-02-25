{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hostenv.Provider.Jobs
  ( JobRuntime
  , JobSummary(..)
  , jobSummaryStatus
  , JobEvent(..)
  , JobLogger(..)
  , JobOutcome(..)
  , NewJob(..)
  , ensureJobSchema
  , startJobRuntime
  , enqueueJob
  , loadRecentJobs
  , loadJobById
  , loadJobEventsSince
  , duplicateBroadcastChannel
  , publishJobUpdate
  , markJobFailedFromDeploy
  , markJobSucceededFromDeploy
  ) where

import AddressableContent.Address (defaultScheme, renderAddr)
import AddressableContent.Store (MonadCAS (putBytes))
import AddressableContent.Store.Postgres (PostgresStore (..), initPostgresStore, runPostgresCAS)
import AddressableContent.Types (uriText)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TChan, TQueue, TVar, atomically, dupTChan, newTChanIO, newTQueueIO, newTVarIO, readTQueue, readTVar, writeTChan, writeTQueue, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forever, void, when)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (..), execute, execute_, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)

import Hostenv.Provider.Command (CommandStream (..), withCommandLineLogger)
import Hostenv.Provider.Config (AppConfig(..))
import Hostenv.Provider.DB (withDb)
import Hostenv.Provider.Util (randomToken)

data JobSummary = JobSummary
  { id :: Text
  , kind :: Text
  , status :: Text
  , createdAt :: UTCTime
  , startedAt :: Maybe UTCTime
  , finishedAt :: Maybe UTCTime
  , errorSummary :: Maybe Text
  } deriving (Eq, Show)

instance FromRow JobSummary where
  fromRow =
    JobSummary
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance A.ToJSON JobSummary where
  toJSON summary =
    A.object
      [ "id" A..= summary.id
      , "kind" A..= summary.kind
      , "status" A..= summary.status
      , "createdAt" A..= summary.createdAt
      , "startedAt" A..= summary.startedAt
      , "finishedAt" A..= summary.finishedAt
      , "errorSummary" A..= summary.errorSummary
      ]

jobSummaryStatus :: JobSummary -> Text
jobSummaryStatus summary = summary.status

data JobEvent = JobEvent
  { seq :: Int64
  , timestamp :: UTCTime
  , stream :: Text
  , level :: Maybe Text
  , line :: Maybe Text
  , addr :: Text
  , path :: Text
  } deriving (Eq, Show)

instance FromRow JobEvent where
  fromRow =
    JobEvent
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance A.ToJSON JobEvent where
  toJSON event =
    A.object
      [ "seq" A..= event.seq
      , "ts" A..= event.timestamp
      , "stream" A..= event.stream
      , "level" A..= event.level
      , "line" A..= event.line
      , "addr" A..= event.addr
      , "path" A..= event.path
      ]

data JobLogger = JobLogger
  { jobId :: Text
  , logStdout :: Text -> IO ()
  , logStderr :: Text -> IO ()
  , logInfo :: Text -> IO ()
  , logError :: Text -> IO ()
  }

data NewJob = NewJob
  { kind :: Text
  , requestedByUserId :: Maybe Int
  , projectId :: Maybe Int
  , payload :: A.Value
  , run :: JobLogger -> IO (Either Text JobOutcome)
  }

data JobOutcome
  = JobComplete Text
  | JobWaiting Text
  deriving (Eq, Show)

data QueuedJob = QueuedJob
  { id :: Text
  , definition :: NewJob
  }

data JobRuntime = JobRuntime
  { cfg :: AppConfig
  , queue :: TQueue QueuedJob
  , broadcast :: TChan A.Value
  }

ensureJobSchema :: AppConfig -> IO ()
ensureJobSchema cfg = withDb cfg $ \conn -> do
  initPostgresStore conn
  let ddl =
        [ "CREATE TABLE IF NOT EXISTS jobs (id TEXT PRIMARY KEY, kind TEXT NOT NULL, status TEXT NOT NULL, requested_by_user_id INTEGER REFERENCES users(id) ON DELETE SET NULL, project_id INTEGER REFERENCES projects(id) ON DELETE SET NULL, payload JSONB NOT NULL DEFAULT '{}'::jsonb, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), started_at TIMESTAMPTZ, waiting_at TIMESTAMPTZ, finished_at TIMESTAMPTZ, error_summary TEXT);"
        , "ALTER TABLE jobs ADD COLUMN IF NOT EXISTS waiting_at TIMESTAMPTZ;"
        , "CREATE TABLE IF NOT EXISTS job_events (job_id TEXT NOT NULL REFERENCES jobs(id) ON DELETE CASCADE, seq BIGINT NOT NULL, ts TIMESTAMPTZ NOT NULL DEFAULT now(), stream TEXT NOT NULL, level TEXT, addr TEXT NOT NULL, path TEXT NOT NULL UNIQUE, line TEXT, meta JSONB NOT NULL DEFAULT '{}'::jsonb, PRIMARY KEY (job_id, seq));"
        , "CREATE INDEX IF NOT EXISTS jobs_status_created_idx ON jobs (status, created_at DESC);"
        , "CREATE INDEX IF NOT EXISTS job_events_job_seq_idx ON job_events (job_id, seq);"
        , "CREATE INDEX IF NOT EXISTS job_events_path_idx ON job_events (path);"
        ]
  mapM_ (execute_ conn) ddl
  _ <- execute_ conn "UPDATE jobs SET status = 'failed', finished_at = now(), error_summary = COALESCE(error_summary, 'worker restarted before completion') WHERE status = 'running'"
  pure ()

startJobRuntime :: AppConfig -> IO JobRuntime
startJobRuntime cfg = do
  queue <- newTQueueIO
  broadcast <- newTChanIO
  let runtime = JobRuntime { cfg = cfg, queue = queue, broadcast = broadcast }
  void (forkIO (workerLoop runtime))
  void (forkIO (waitingMonitor runtime))
  void (forkIO (cleanupLoop runtime))
  pure runtime

enqueueJob :: JobRuntime -> NewJob -> IO Text
enqueueJob runtime jobDef = do
  jobId <- newJobId
  let payloadText = TE.decodeUtf8 (BL.toStrict (A.encode jobDef.payload))
  withDb runtime.cfg $ \conn -> do
    _ <- execute conn
      "INSERT INTO jobs (id, kind, status, requested_by_user_id, project_id, payload) VALUES (?, ?, 'queued', ?, ?, ?::jsonb)"
      (jobId, jobDef.kind, jobDef.requestedByUserId, jobDef.projectId, payloadText)
    pure ()
  atomically (writeTQueue runtime.queue (QueuedJob jobId jobDef))
  publishStatus runtime jobId "queued" Nothing
  pure jobId

loadRecentJobs :: AppConfig -> IO [JobSummary]
loadRecentJobs cfg = withDb cfg $ \conn ->
  query conn
    "SELECT id, kind, status, created_at, started_at, finished_at, error_summary FROM jobs ORDER BY created_at DESC LIMIT 25"
    ()

loadJobById :: AppConfig -> Text -> IO (Maybe JobSummary)
loadJobById cfg jobId = withDb cfg $ \conn -> do
  rows <- query conn
    "SELECT id, kind, status, created_at, started_at, finished_at, error_summary FROM jobs WHERE id = ? LIMIT 1"
    (Only jobId)
  case rows of
    (job:_) -> pure (Just job)
    _ -> pure Nothing

loadJobEventsSince :: AppConfig -> Text -> Int64 -> IO [JobEvent]
loadJobEventsSince cfg jobId afterSeq = withDb cfg $ \conn ->
  query conn
    "SELECT seq, ts, stream, level, line, addr, path FROM job_events WHERE job_id = ? AND seq > ? ORDER BY seq"
    (jobId, afterSeq)

duplicateBroadcastChannel :: JobRuntime -> IO (TChan A.Value)
duplicateBroadcastChannel runtime = atomically (dupTChan runtime.broadcast)

publishJobUpdate :: JobRuntime -> A.Value -> IO ()
publishJobUpdate runtime = publish runtime

markJobFailedFromDeploy :: JobRuntime -> Text -> Text -> IO ()
markJobFailedFromDeploy runtime jobId message = do
  updated <- withDb runtime.cfg $ \conn ->
    execute conn
      "UPDATE jobs SET status = 'failed', finished_at = now(), error_summary = ? WHERE id = ? AND status IN ('queued','running','waiting')"
      (message, jobId)
  when (updated > 0) $ do
    finalizePendingDeployActions runtime.cfg jobId "failed" (Just message)
    publishStatus runtime jobId "failed" (Just message)

markJobSucceededFromDeploy :: JobRuntime -> Text -> IO ()
markJobSucceededFromDeploy runtime jobId = do
  updated <- withDb runtime.cfg $ \conn ->
    execute conn
      "UPDATE jobs SET status = 'succeeded', finished_at = now(), error_summary = NULL WHERE id = ? AND status = 'waiting'"
      (Only jobId)
  when (updated > 0) $
    publishStatus runtime jobId "succeeded" Nothing

workerLoop :: JobRuntime -> IO ()
workerLoop runtime = forever $ do
  queuedJob <- atomically (readTQueue runtime.queue)
  runQueuedJob runtime queuedJob

runQueuedJob :: JobRuntime -> QueuedJob -> IO ()
runQueuedJob runtime queuedJob = do
  let jobId = queuedJob.id
  let jobDef = queuedJob.definition
  withDb runtime.cfg $ \conn -> do
    _ <- execute conn
      "UPDATE jobs SET status = 'running', started_at = now(), error_summary = NULL WHERE id = ?"
      (Only jobId)
    pure ()
  publishStatus runtime jobId "running" Nothing
  seqVar <- newTVarIO 0
  let appendEvent stream level line = do
        nextSeq <- atomically $ do
          currentSeq <- readTVar seqVar
          let newSeq = currentSeq + 1
          writeTVar seqVar newSeq
          pure newSeq
        insertJobEvent runtime.cfg jobId nextSeq stream level line
        publishEvent runtime jobId nextSeq stream level line
      logger =
        JobLogger
          { jobId = jobId
          , logStdout = appendEvent "stdout" Nothing
          , logStderr = appendEvent "stderr" Nothing
          , logInfo = appendEvent "app" (Just "info")
          , logError = appendEvent "app" (Just "error")
          }
      commandLogger stream line =
        case stream of
          CommandStdout -> logger.logStdout line
          CommandStderr -> logger.logStderr line

  runResult <- try (withCommandLineLogger commandLogger (jobDef.run logger)) :: IO (Either SomeException (Either Text JobOutcome))
  case runResult of
    Left exception -> do
      let message = T.pack (show exception)
      logger.logError message
      markJobFailed runtime jobId message
    Right (Left errText) -> do
      logger.logError errText
      markJobFailed runtime jobId errText
    Right (Right outcome) ->
      case outcome of
        JobComplete successMsg -> do
          when (T.strip successMsg /= "") (logger.logInfo successMsg)
          markJobSucceeded runtime jobId
        JobWaiting waitingMsg -> do
          when (T.strip waitingMsg /= "") (logger.logInfo waitingMsg)
          markJobWaiting runtime jobId

cleanupLoop :: JobRuntime -> IO ()
cleanupLoop runtime = forever $ do
  let cleanupMinutes = max 1 runtime.cfg.appJobsCleanupIntervalMins
  threadDelay (cleanupMinutes * 60 * 1000000)
  cleanupOldJobs runtime.cfg

waitingMonitor :: JobRuntime -> IO ()
waitingMonitor runtime = forever $ do
  let checkSeconds = max 1 runtime.cfg.appJobsWaitInterval
  threadDelay (checkSeconds * 1000000)
  timeoutWaitingJobs runtime

timeoutWaitingJobs :: JobRuntime -> IO ()
timeoutWaitingJobs runtime = do
  let timeoutMinutes = max 1 runtime.cfg.appJobsWaitTimeoutMins
  timedOutIds <- withDb runtime.cfg $ \conn -> do
    rows <- query conn
      "UPDATE jobs SET status = 'failed', finished_at = now(), error_summary = 'Timed out waiting for node callbacks' WHERE status = 'waiting' AND COALESCE(waiting_at, started_at, created_at) < now() - (? * INTERVAL '1 minute') RETURNING id"
      (Only timeoutMinutes)
    pure [jobId | Only jobId <- rows]
  mapM_
    (\jobId -> do
      finalizePendingDeployActions runtime.cfg jobId "timed_out" (Just "Timed out waiting for node callbacks")
      publishStatus runtime jobId "failed" (Just "Timed out waiting for node callbacks")
    )
    timedOutIds

cleanupOldJobs :: AppConfig -> IO ()
cleanupOldJobs cfg = withDb cfg $ \conn -> do
  let retentionDays = max 1 cfg.appJobsRetentionDays
  _ <- execute conn
    "DELETE FROM jobs WHERE finished_at IS NOT NULL AND finished_at < now() - (? * INTERVAL '1 day')"
    (Only retentionDays)
  _ <- execute_ conn
    "DELETE FROM deploy_actions a WHERE NOT EXISTS (SELECT 1 FROM jobs j WHERE j.id = a.job_id)"
  _ <- execute_ conn
    "DELETE FROM deploy_node_events e WHERE NOT EXISTS (SELECT 1 FROM jobs j WHERE j.id = e.job_id)"
  _ <- execute_ conn
    "DELETE FROM deploy_intents i WHERE NOT EXISTS (SELECT 1 FROM jobs j WHERE j.id = i.job_id)"
  _ <- execute_ conn
    "DELETE FROM cas_objects c WHERE NOT EXISTS (SELECT 1 FROM job_events e WHERE e.addr = c.addr)"
  pure ()

markJobFailed :: JobRuntime -> Text -> Text -> IO ()
markJobFailed runtime jobId message = do
  finalizePendingDeployActions runtime.cfg jobId "failed" (Just message)
  withDb runtime.cfg $ \conn -> do
    _ <- execute conn
      "UPDATE jobs SET status = 'failed', finished_at = now(), error_summary = ? WHERE id = ?"
      (message, jobId)
    pure ()
  publishStatus runtime jobId "failed" (Just message)

markJobSucceeded :: JobRuntime -> Text -> IO ()
markJobSucceeded runtime jobId = do
  withDb runtime.cfg $ \conn -> do
    _ <- execute conn
      "UPDATE jobs SET status = 'succeeded', finished_at = now(), error_summary = NULL WHERE id = ?"
      (Only jobId)
    pure ()
  publishStatus runtime jobId "succeeded" Nothing

markJobWaiting :: JobRuntime -> Text -> IO ()
markJobWaiting runtime jobId = do
  withDb runtime.cfg $ \conn -> do
    _ <- execute conn
      "UPDATE jobs SET status = 'waiting', waiting_at = now(), finished_at = NULL, error_summary = NULL WHERE id = ?"
      (Only jobId)
    pure ()
  publishStatus runtime jobId "waiting" Nothing

insertJobEvent :: AppConfig -> Text -> Int64 -> Text -> Maybe Text -> Text -> IO ()
insertJobEvent cfg jobId seqNumber streamName levelName lineText = do
  now <- getCurrentTime
  withDb cfg $ \conn -> do
    addr <- storeLine conn lineText
    let path = "/jobs/" <> jobId <> "/events/" <> T.pack (show seqNumber)
    _ <- execute conn
      "INSERT INTO job_events (job_id, seq, ts, stream, level, addr, path, line, meta) VALUES (?, ?, ?, ?, ?, ?, ?, ?, '{}'::jsonb)"
      (jobId, seqNumber, now, streamName, levelName, addr, path, lineText)
    pure ()

storeLine :: Connection -> Text -> IO Text
storeLine conn lineText = do
  let store = PostgresStore conn defaultScheme
  addr <- runPostgresCAS store (putBytes (TE.encodeUtf8 lineText))
  pure (uriText (renderAddr addr))

publishStatus :: JobRuntime -> Text -> Text -> Maybe Text -> IO ()
publishStatus runtime jobId statusText mError =
  publish runtime
    ( A.object
        [ "type" A..= ("job_status" :: Text)
        , "jobId" A..= jobId
        , "status" A..= statusText
        , "error" A..= mError
        ]
    )

publishEvent :: JobRuntime -> Text -> Int64 -> Text -> Maybe Text -> Text -> IO ()
publishEvent runtime jobId seqNumber streamName levelName lineText =
  publish runtime
    ( A.object
        [ "type" A..= ("job_event" :: Text)
        , "jobId" A..= jobId
        , "seq" A..= seqNumber
        , "stream" A..= streamName
        , "level" A..= levelName
        , "line" A..= lineText
        ]
    )

publish :: JobRuntime -> A.Value -> IO ()
publish runtime value = atomically (writeTChan runtime.broadcast value)

newJobId :: IO Text
newJobId = do
  token <- randomToken 16
  let normalized = T.take 32 (token <> T.replicate 32 "0")
      chunk n offset = T.take n (T.drop offset normalized)
  pure
    ( T.intercalate
        "-"
        [ chunk 8 0
        , chunk 4 8
        , chunk 4 12
        , chunk 4 16
        , chunk 12 20
        ]
    )

finalizePendingDeployActions :: AppConfig -> Text -> Text -> Maybe Text -> IO ()
finalizePendingDeployActions cfg jobId finalStatus mMessage =
  withDb cfg $ \conn -> do
    _ <- execute conn
      "UPDATE deploy_actions SET status = ?, message = COALESCE(?, message), started_at = COALESCE(started_at, now()), finished_at = COALESCE(finished_at, now()), updated_at = now() WHERE job_id = ? AND status IN ('queued','waiting','running')"
      (finalStatus, mMessage, jobId)
    pure ()
