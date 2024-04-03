-- | Test the client functions.

module TestPoll
     ( tests
     ) where

import           Colog

import           Control.Monad
import           Control.Monad.Catch               ( MonadCatch, MonadThrow )
import           Control.Monad.Reader              ( runReaderT )

import qualified Data.Text                         as T

import           Database.Beam

import           Haskbike.API.APIEntity
import qualified Haskbike.API.Client               as C
import           Haskbike.API.MockServer           ( MockServerLog (MockServerLogNever), mockServerBaseUrl,
                                                     runMockServer )
import           Haskbike.AppEnv
import           Haskbike.CLI.Options              ( PollOptions (..), PopulateStatusChangesOpt (..) )
import           Haskbike.CLI.Poll
import           Haskbike.Database.BikeShare
import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Operations      ( lockTableAndSleep, queryRowCount )
import           Haskbike.Database.Test.Utils

import           Prelude                           hiding ( log, unwords )

import           Test.Tasty
import           Test.Tasty.HUnit

import           UnliftIO


tests :: TestTree
tests = testGroup "Polling tests"
  [ testCase "Poll API"      pollApi
  , testCase "Poll mock API" pollMock
  , testCase "Queued records are inserted after table is unlocked" pollMockAndTestQueueing
  ]


-- | Test polling the PBSC API.
pollApi :: IO ()
pollApi = withTempDbM Silent migrateDB $
  void $ timeout (5 * 1000000) (pollClient (PollOptions NeverPopulate))

-- | Test polling the mock API.
pollMock :: IO ()
pollMock = withTempDbM Silent migrateDB $ do
  -- Update default environment to poll local mock server.
  env <- ask
  let envMock = env { envBaseUrl = mockServerBaseUrl }
  liftIO $ runReaderT (unAppM doPoll) envMock
  where
    doPoll = void $ timeout (5 * 1000000) (pollClient (PollOptions NeverPopulate))

{- | Poll mock info API and queue records for insertion, but lock info table.
Checks that queued records are all inserted after table is unlocked.
-}
pollMockAndTestQueueing :: IO ()
pollMockAndTestQueueing = withTempDbM Silent migrateDB $ do
  -- Update default environment to poll local mock server.
  env <- ask
  let envMock = env { envBaseUrl = mockServerBaseUrl, envMinSeverity = Info, envLogDatabase = False }
  liftIO $ runReaderT (unAppM doPoll) envMock
  where
    doPoll :: AppM ()
    doPoll = void (concurrently (timeout (10 * 1000000) (runMockServer MockServerLogNever 8082))
                                (concurrently (lockInfoTable 5)
                                              pollInfoOnly
                                )
                  )

lockInfoTable :: (HasEnv env m, MonadUnliftIO m, MonadCatch m)
              => Int -> m ()
lockInfoTable duration = do
  logWarning . T.pack $ "Locking " <> tableName <> " for " <> show duration <> " seconds."
  lockTableAndSleep tableName duration
  where tableName = "station_information"

pollInfoOnly :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m) => m ()
pollInfoOnly = do
  -- Create insertion and polling threads.
  logInfo "Initializing insertion and polling threads."
  lastUpdated  <- liftIO $ newTVarIO 0
  queue        <- liftIO (newTBQueueIO 240)
  -- Run each action 5 times.
  insertThreadInfo <- (async . replicateM_ iterations) (insertThread StationInformationEP queue)
  pollThreadInfo   <- (async . replicateM_ iterations) (pollThread StationInformationEP C.stationInformation lastUpdated queue)

  -- Start both threads and wait for both to finish.
  _ <- waitBoth insertThreadInfo pollThreadInfo
  logInfo "Polling done"

  infoRowCount <- queryRowCount bikeshareStationInformation
  liftIO $ assertEqual "Inserted all blocked station information." (iterations * 1000) infoRowCount
  where
    iterations = 5
