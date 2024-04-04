{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- | Poll the API for status updates, inserting results in database as needed.
module Haskbike.CLI.Poll
     ( dispatchPoll
     , makePollThreadPair
     , pollClient
     ) where

import           Colog                             ( logError, logInfo, logWarning )

import           Control.Exception                 ( throw )
import           Control.Monad                     ( forever, void )
import           Control.Monad.Catch               ( MonadCatch, MonadThrow )

import qualified Data.Text                         as T

import           Haskbike.API.APIEntity
import qualified Haskbike.API.Client               as C
import           Haskbike.API.ResponseWrapper
import           Haskbike.AppEnv
import           Haskbike.CLI.Options              ( PollOptions (..), PopulateStatusChangesOpt (..) )
import           Haskbike.Database.BikeShare       ( bikeshareStationStatusChanges )
import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Operations      ( populateChangedStationStatusTable, queryRowCount )

import           Prelude                           hiding ( log )

import           Servant.Client                    ( ClientM )

import           TextShow                          ( showt )

import           UnliftIO


-- | Dispatch CLI arguments to the poller.
dispatchPoll :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
             => PollOptions
             -> m ()
dispatchPoll opts = pollClient opts `catch` handlePollException


-- | Handle a polling exception.
handlePollException :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                    => PollException
                    -> m ()
handlePollException e = logError ("Poll exception: " <> (T.pack . show) e) >> throw e


makePollThreadPair :: (HasEnv env m, APIPersistable apiType dbType, MonadUnliftIO m, MonadCatch m)
                   => EndpointQueried -> ClientM (ResponseWrapper apiType) -> m (Async a1, Async a2)
makePollThreadPair endpoint endpointFn = do
  lastUpdated  <- liftIO (newTVarIO 0)
  queue        <- liftIO (newTBQueueIO queueDepth)
  newInsertThread <- (async . forever) (insertThread endpoint queue)
  newPollThread   <- (async . forever) (pollThread endpoint endpointFn lastUpdated queue)
  pure (newInsertThread, newPollThread)
  where
    -- Queue depth is set to hold two hours worth of data.
    queueDepth = 2 * secondsInHour `div` averagePollSeconds
    secondsInHour = 3600
    averagePollSeconds = 30

pollClient :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
           => PollOptions -> m ()
pollClient pollOptions = do
  -- Populate status changes table if switch was enabled.
  void (populateStatusChanges (optPollPopulateStatusChanges pollOptions))

  logInfo "Initializing insertion and polling threads."
  (insertThreadInfo,    pollThreadInfo)    <- makePollThreadPair StationInformationEP C.stationInformation
  (insertThreadStatus,  pollThreadStatus)  <- makePollThreadPair StationStatusEP      C.stationStatus
  (insertThreadSysInfo, pollThreadSysInfo) <- makePollThreadPair SystemInformationEP  C.systemInformation
  let insertThreads = [ insertThreadInfo, insertThreadStatus, insertThreadSysInfo ]
  let pollThreads   = [ pollThreadInfo,   pollThreadStatus,   pollThreadSysInfo   ]


  -- All threads managed by pollClient.
  let threads = insertThreads ++ pollThreads

  -- Start concurrent operations and wait until any one of them finishes (which should not happen).
  -- If any thread finishes, we cancel the others as there is an implied dependency between them.
  _ <- waitAnyCancel threads

  logWarning "Polling and insertion threads terminated."
  pure ()

-- | Populate the station status changes table from the content of the station status table.
populateStatusChanges :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                      => PopulateStatusChangesOpt -> m Int
populateStatusChanges AlwaysPopulate = doPopulateStatusChanges
populateStatusChanges NeverPopulate  = pure 0
populateStatusChanges AutoPopulate   = do
  logInfo "Querying number of station status changes rows"
  -- If the table is empty, populate it.
  numRows >>= \case
    0     -> logInfo (logRowsMessage    0) >> doPopulateStatusChanges
    rows  -> logInfo (logRowsMessage rows) >> pure rows
  where
    numRows = queryRowCount bikeshareStationStatusChanges

    logRowsMessage :: Int -> T.Text
    logRowsMessage 0    = "Station status changes table is empty."
    logRowsMessage rows = "Station status changes table has " <> showt rows <> " rows; not populating table."

doPopulateStatusChanges :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                        => m Int
doPopulateStatusChanges = do
  logWarning "Populating station status changes table from station status table."
  statusChanges <- withPostgres populateChangedStationStatusTable
  logInfo $ "Populated " <> (T.pack . show . length) statusChanges <> " rows in station status changes table."
  pure (length statusChanges)
