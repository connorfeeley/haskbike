{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- | Poll the API for status updates, inserting results in database as needed.
module Haskbike.CLI.Poll
     ( dispatchPoll
     , pollClient
     ) where

import           Colog                             ( logInfo, logWarning )

import           Control.Monad                     ( forever, void )
import           Control.Monad.Catch               ( MonadCatch, MonadThrow )

import qualified Data.Text                         as T

import           Haskbike.API.APIEntity
import qualified Haskbike.API.Client               as C
import           Haskbike.AppEnv
import           Haskbike.CLI.Options              ( PollOptions (..), PopulateStatusChangesOpt (..) )
import           Haskbike.Database.BikeShare       ( bikeshareStationStatusChanges )
import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Operations      ( populateChangedStationStatusTable, queryRowCount )

import           Prelude                           hiding ( log )

import           TextShow                          ( showt )

import           UnliftIO


-- | Dispatch CLI arguments to the poller.
dispatchPoll :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
             => PollOptions
             -> m ()
dispatchPoll = pollClient


pollClient :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
           => PollOptions -> m ()
pollClient pollOptions = do
  -- runReaderT (unServerAppM app) senv
  -- Initialize TVars for per-thread last updated time.
  (statusLastUpdated, infoLastUpdated, sysInfoLastUpdated) <- liftIO $
    (,,) <$> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0

  infoQueue    <- liftIO newTQueueIO
  statusQueue  <- liftIO newTQueueIO
  sysInfoQueue <- liftIO newTQueueIO


  -- Populate status changes table if switch was enabled.
  void (populateStatusChanges (optPollPopulateStatusChanges pollOptions))

  -- Set up insertion threads before doing initial fetch.
  logInfo "Initializing insertion threads."
  insertThreadInfo     <- (async . forever) (insertThread StationInformationEP infoQueue)
  insertThreadStatus   <- (async . forever) (insertThread StationStatusEP      statusQueue)
  insertThreadSysInfo  <- (async . forever) (insertThread SystemInformationEP  sysInfoQueue)
  let insertThreads = [ insertThreadInfo, insertThreadStatus, insertThreadSysInfo ]

  logInfo "Fetching from API once."
  -- runPollReader pollEnv $ fetchAndPersist StationInformationEP stationInformation firstUpdate infoQueue
  fetchAndPersist StationInformationEP stationInformation firstUpdate infoQueue
  fetchAndPersist StationStatusEP      stationStatus      firstUpdate statusQueue
  fetchAndPersist SystemInformationEP  systemInformation  firstUpdate sysInfoQueue

  logInfo "Initializing polling threads."
  pollThreadInfo    <- (async . forever) (pollThread StationInformationEP C.stationInformation infoLastUpdated    infoQueue)
  pollThreadStatus  <- (async . forever) (pollThread StationStatusEP      C.stationStatus      statusLastUpdated  statusQueue)
  pollThreadSysInfo <- (async . forever) (pollThread SystemInformationEP  C.systemInformation  sysInfoLastUpdated sysInfoQueue)
  let pollThreads = [ pollThreadInfo, pollThreadStatus, pollThreadSysInfo ]

  -- All threads managed by pollClient.
  let threads = insertThreads ++ pollThreads

  -- Start concurrent operations and wait until any one of them finishes (which should not happen).
  -- If any thread finishes, we cancel the others as there is an implied dependency between them.
  _ <- waitAnyCancel threads

  logWarning "Polling and insertion threads terminated."
  pure ()
  where
    firstUpdate = 0 -- Use Unix epoch for initial lastUpdated.

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
