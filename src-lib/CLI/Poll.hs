-- | Poll the API for status updates, inserting results in database as needed.
module CLI.Poll
     ( dispatchPoll
     , pollClient
     ) where

import           API.APIEntity
import           API.Client

import           AppEnv

import           CLI.Options                        ( PollOptions (..) )

import           Colog                              ( logInfo, logWarning )

import           Control.Monad                      ( forever )

import           Database.BikeShare.EndpointQueried

import           Prelude                            hiding ( log )

import           UnliftIO


-- | Dispatch CLI arguments to the poller.
dispatchPoll :: PollOptions
             -> AppM ()
dispatchPoll _options = pollClient


pollClient :: AppM ()
pollClient = do
    -- Initialize TVars for per-thread last updated time.
    (statusLastUpdated, infoLastUpdated, sysInfoLastUpdated) <- liftIO $
      (,,) <$> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0

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
