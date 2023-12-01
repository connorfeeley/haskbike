-- | Poll the API for status updates, inserting results in database as needed.
module CLI.Poll
     ( dispatchPoll
     , pollClient
     ) where


import           API.Client
import           API.Pollable
import           API.ResponseWrapper
import qualified API.Types                             as AT

import           AppEnv

import           CLI.Options                           ( PollOptions (..) )
import           CLI.Poll.Utils

import           Colog                                 ( logDebug, logInfo )

import           Control.Lens
import           Control.Monad                         ( void )

import qualified Data.Text                             as T

import           Database.BikeShare.Operations
import           Database.BikeShare.StationInformation
import           Database.BikeShare.StationStatus

import           Fmt                                   ( format )

import           Text.Pretty.Simple.Extras                            ( pShowCompact )

import           Prelude                               hiding ( log )

import           UnliftIO


-- | Dispatch CLI arguments to the poller.
dispatchPoll :: PollOptions
             -> AppM ()
dispatchPoll _options = pollClient


pollClient :: AppM ()
pollClient = do
    -- Initialize TVars and TBQueues for station information and station status.
    (statusTtl, statusLastUpdated, statusQueueResp    :: TBQueue (ResponseWrapper [AT.StationStatus]),
     infoTtl, infoLastUpdated, infoQueueResp          :: TBQueue (ResponseWrapper [AT.StationInformation]),
     sysInfoTtl, sysInfoLastUpdated, sysInfoQueueResp :: TBQueue (ResponseWrapper AT.SystemInformation)) <- liftIO $
        (,,,,,,,,) <$> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4
                   <*> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4
                   <*> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4

    logInfo "Fetching station information from API once."
    -- Threads to fetch and handle station information from API once.
    infoPollOnce   <- async (requester infoQueueResp infoTtl infoLastUpdated)
    infoHandleOnce <- async (handler   infoQueueResp)
    -- Wait for both threads to finish.
    waitBoth infoPollOnce infoHandleOnce


    logInfo "Initializing polling and handling threads."
    -- The polling threads.
    infoPollingThread    <- createPollingThread infoQueueResp    infoTtl    infoLastUpdated
    statusPollingThread  <- createPollingThread statusQueueResp  statusTtl  statusLastUpdated
    sysInfoPollingThread <- createPollingThread sysInfoQueueResp sysInfoTtl sysInfoLastUpdated

    -- The handling threads for station information and station status.
    infoHandlingThread    <- createHandlingThread infoQueueResp
    statusHandlingThread  <- createHandlingThread statusQueueResp
    sysInfoHandlingThread <- createHandlingThread sysInfoQueueResp

    -- Start concurrent operations and wait until any one of them finishes (which should not happen).
    -- If any thread finishes, we cancel the others as there is an implied dependency between them.
    _ <- waitAnyCancel [ infoPollingThread,  statusPollingThread,  sysInfoPollingThread
                       , infoHandlingThread, statusHandlingThread, sysInfoHandlingThread
                       ]

    logInfo "Polling and handling threads running. Press Ctrl+C to terminate."
    return ()

instance Pollable (ResponseWrapper [AT.StationStatus]) where
  -- | Endpoint (ClientM a)
  request = stationStatus

  -- | Thread action to request station information from API.
  requester = requesterFn "Stn Status" request

  -- | Thread action to handle API response for station status query.
  handler queue = void $ do
    response <- liftIO $ atomically $ readTBQueue queue

    let status = response ^. respData
    logInfo $ format "(Status) Received {} status records from API." (length status)

    -- Insert the updated status.
    insertedResult <- insertStationStatus status

    -- Log each station ID updated.
    mapM_ (logDebug . T.pack . fmtLog . dataToTuple) insertedResult

    -- Log number of inserted rows.
    logInfo $ format "(Status) Inserted: {}" (length insertedResult)

    where
      dataToTuple s = ( s ^. statusStationId . unInformationStationId
                      , s ^. statusLastReported
                      )
      fmtLog (sid, lr) = format "ID: {} {}" sid (pShowCompact lr)


-- * Station information handling.

instance Pollable (ResponseWrapper [AT.StationInformation]) where
  -- | Endpoint (ClientM a)
  request = stationInformation

  -- | Thread action to request station information from API.
  requester = requesterFn "Stn Info" request

  -- | Thread action to handle API response for station information query.
  handler queue = void $ do
    response <- liftIO $ atomically $ readTBQueue queue

    let info = response ^. respData
    logInfo $ format "(Info) Received {} info records from API." (length info)

    -- Insert the station information (updating existing records, if existing).
    insertedResult <- insertStationInformation info

    -- Log each station ID updated.
    mapM_ (logDebug . T.pack . fmtLog) insertedResult

    -- Log number of inserted rows.
    logInfo $ format "(Info) Updated/inserted {} records into database." (length insertedResult)

    where
      fmtLog inserted = format "ID: {}" (pShowCompact inserted)

-- * System information handling.

instance Pollable (ResponseWrapper AT.SystemInformation) where
  -- | Endpoint (ClientM a)
  request = systemInformation

  -- | Thread action to request station information from API.
  requester = requesterFn "Sys Info" request

  -- | Thread action to handle API response for station information query.
  handler queue = void $ do
    response <- liftIO $ atomically $ readTBQueue queue

    let (reported, info) = (response ^. respLastUpdated, response ^. respData)
    logInfo "(Sys Info) Received system info from API."

    -- Insert the station information (updating existing records, if existing).
    (_insertedInfo, _insertedInfoCount) <- insertSystemInformation reported info

    -- Log number of inserted rows.
    logInfo "(Sys Info) Updated/inserted system information into database."
