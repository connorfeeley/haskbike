-- | Poll the API for status updates, inserting results in database as needed.
module CLI.Poll
     ( dispatchPoll
     , pollClient
     ) where

import           API.APIEntity
import           API.Client

import           AppEnv

import           CLI.Options                        ( PollOptions (..), PopulateStatusChangesOpt (..) )

import           Colog                              ( logInfo, logWarning )

import           Control.Monad                      ( forever, void )

import qualified Data.Text                          as T

import           Database.BikeShare                 ( bikeshareStationStatusChanges )
import           Database.BikeShare.EndpointQueried
import           Database.BikeShare.Operations      ( populateChangedStationStatusTable, queryRowCount )

import           Prelude                            hiding ( log )

import           UnliftIO


-- | Dispatch CLI arguments to the poller.
dispatchPoll :: PollOptions
             -> AppM ()
dispatchPoll = pollClient


pollClient :: PollOptions -> AppM ()
pollClient pollOptions = do
    -- Initialize TVars for per-thread last updated time.
    (statusLastUpdated, infoLastUpdated, sysInfoLastUpdated) <- liftIO $
      (,,) <$> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0

    -- Populate status changes table if switch was enabled.
    void (populateStatusChanges (optPollPopulateStatusChanges pollOptions))

    logInfo "Fetching from API once."
    fetchAndPersist StationInformationEP stationInformation firstUpdate
    fetchAndPersist StationStatusEP      stationStatus      firstUpdate
    fetchAndPersist SystemInformationEP  systemInformation  firstUpdate

    logInfo "Initializing polling and handling threads."
    -- The polling threads.
    infoPollingThread    <- (async . forever) (pollThread StationInformationEP stationInformation infoLastUpdated)
    statusPollingThread  <- (async . forever) (pollThread StationStatusEP      stationStatus      statusLastUpdated)
    sysInfoPollingThread <- (async . forever) (pollThread SystemInformationEP  systemInformation  sysInfoLastUpdated)

    -- Start concurrent operations and wait until any one of them finishes (which should not happen).
    -- If any thread finishes, we cancel the others as there is an implied dependency between them.
    _ <- waitAnyCancel [ infoPollingThread,  statusPollingThread,  sysInfoPollingThread ]

    logWarning "Polling threads terminated."
    pure ()

    where firstUpdate = 0

-- | Populate the station status changes table from the content of the station status table.
populateStatusChanges :: PopulateStatusChangesOpt -> AppM Int
populateStatusChanges AlwaysPopulate = doPopulateStatusChanges
populateStatusChanges NeverPopulate  = pure 0
populateStatusChanges AutoPopulate   = do
  logInfo "Querying number of station status changes rows"
  -- If the table is empty, populate it.
  numRows >>= \case
    0     -> logInfo (logRowsMessage    0) >> doPopulateStatusChanges
    rows  -> logInfo (logRowsMessage rows) >> pure rows
  where
    numRows :: AppM Int
    numRows = queryRowCount bikeshareStationStatusChanges

    logRowsMessage :: Int -> T.Text
    logRowsMessage 0    = "Station status changes table is empty."
    logRowsMessage rows = "Station status changes table has " <> (T.pack . show) rows <> " rows; not populating table."

doPopulateStatusChanges :: AppM Int
doPopulateStatusChanges = do
  logWarning "Populating station status changes table from station status table."
  statusChanges <- withPostgres populateChangedStationStatusTable
  logInfo $ "Populated " <> (T.pack . show . length) statusChanges <> " rows in station status changes table."
  pure (length statusChanges)
