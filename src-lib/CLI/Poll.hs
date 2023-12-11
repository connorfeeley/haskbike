{-# LANGUAGE DataKinds #-}
-- | Poll the API for status updates, inserting results in database as needed.
module CLI.Poll
     ( dispatchPoll
     , handlerStationInformation
     , handlerStationStatus
     , handlerSystemInformation
     , pollClient
     ) where


import           API.APIEntity
import           API.Client
import           API.ClientLifted
import           API.Pollable
import           API.ResponseWrapper
import qualified API.StationInformation                as AT
import qualified API.StationStatus                     as AT
import qualified API.SystemInformation                 as AT

import           AppEnv

import           CLI.Options                           ( PollOptions (..) )
import           CLI.Poll.Utils

import           Colog                                 ( logDebug, logInfo )

import           Control.Lens
import           Control.Monad                         ( forever, void )

import qualified Data.Text                             as T
import           Data.Time

import           Database.BikeShare.EndpointQueried
import           Database.BikeShare.Operations
import           Database.BikeShare.StationInformation
import qualified Database.BikeShare.StationInformation as DB
import           Database.BikeShare.StationStatus
import qualified Database.BikeShare.StationStatus      as DB
import qualified Database.BikeShare.SystemInformation  as DB

import           Fmt                                   ( format )

import           Prelude                               hiding ( log )

import           Servant.Client

import           Text.Pretty.Simple.Extras             ( pShowCompact )

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


    -- FIXME
    handlerStationInformation
    handlerStationStatus
    handlerSystemInformation

    logInfo "Initializing polling and handling threads."
    -- The polling threads.
    infoPollingThread    <- (async . forever) (requester infoQueueResp    infoTtl    infoLastUpdated)
    statusPollingThread  <- (async . forever) (requester statusQueueResp  statusTtl  statusLastUpdated)
    sysInfoPollingThread <- (async . forever) (requester sysInfoQueueResp sysInfoTtl sysInfoLastUpdated)

    -- The handling threads for station information and station status.
    infoHandlingThread    <- (async . forever) (handler infoQueueResp)
    statusHandlingThread  <- (async . forever) (handler statusQueueResp)
    sysInfoHandlingThread <- (async . forever) (handler sysInfoQueueResp)

    -- Start concurrent operations and wait until any one of them finishes (which should not happen).
    -- If any thread finishes, we cancel the others as there is an implied dependency between them.
    _ <- waitAnyCancel [ infoPollingThread,  statusPollingThread,  sysInfoPollingThread
                       , infoHandlingThread, statusHandlingThread, sysInfoHandlingThread
                       ]

    logInfo "Polling and handling threads running. Press Ctrl+C to terminate."
    return ()


handlerStationInformation :: ( ApiFetcher ClientM [AT.StationInformation]
                             , DbInserter AppM AT.StationInformation DB.StationInformationT Identity
                             ) => AppM ()
handlerStationInformation = void $ do
  -- Convert API records to database entities and handle failures if necessary.
  apiResult <- runQueryM (apiFetch :: ClientM (ResponseWrapper [AT.StationInformation]))
  case apiResult of
    Left err -> handleResponseError StationInformationEP err
    Right resp -> do
      handleResponseSuccess StationInformationEP (_respLastUpdated resp)
      insertedResult <- dbInsert (_respData resp) :: AppM [DB.StationInformationT Identity]
      logInfo $ format "(Info) Updated/inserted {} records into database." (length insertedResult)

handlerStationStatus :: ( ApiFetcher ClientM [AT.StationStatus]
                        , DbInserter AppM AT.StationStatus DB.StationStatusT Identity
                        ) => AppM ()
handlerStationStatus = void $ do
  -- Convert API records to database entities and handle failures if necessary.
  apiResult <- runQueryM (apiFetch :: ClientM (ResponseWrapper [AT.StationStatus]))
  case apiResult of
    Left err -> handleResponseError StationStatusEP err
    Right resp -> do
      handleResponseSuccess StationStatusEP (_respLastUpdated resp)
      insertedResult <- dbInsert (_respData resp) :: AppM [DB.StationStatusT Identity]
      logInfo $ format "(Info) Updated/inserted {} records into database." (length insertedResult)

handlerSystemInformation :: ( ApiFetcher ClientM AT.SystemInformation
                            , DbInserter AppM (UTCTime, AT.SystemInformation) DB.SystemInformationT Identity
                            ) => AppM ()
handlerSystemInformation = void $ do
  -- Convert API records to database entities and handle failures if necessary.
  apiResult <- runQueryM (apiFetch :: ClientM (ResponseWrapper AT.SystemInformation))
  case apiResult of
    Left err -> handleResponseError SystemInformationEP err
    Right resp -> do
      handleResponseSuccess SystemInformationEP (_respLastUpdated resp)
      insertedResult <- dbInsert [(_respLastUpdated resp, _respData resp)] :: AppM [DB.SystemInformationT Identity]
      logInfo $ format "(Info) Updated/inserted {} records into database." (length insertedResult)


instance Pollable (ResponseWrapper [AT.StationStatus]) where
  -- | Endpoint (ClientM a)
  request = stationStatus

  -- | Thread action to request station information from API.
  requester = requesterFn StationStatusEP "Stn Status" request

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
  requester = requesterFn StationInformationEP "Stn Info" request

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
  requester = requesterFn SystemInformationEP "Sys Info" request

  -- | Thread action to handle API response for station information query.
  handler queue = void $ do
    response <- liftIO $ atomically $ readTBQueue queue

    let (reported, info) = (response ^. respLastUpdated, response ^. respData)
    logInfo "(Sys Info) Received system info from API."

    -- Insert the station information (updating existing records, if existing).
    (_insertedInfo, _insertedInfoCount) <- insertSystemInformation reported info

    -- Log number of inserted rows.
    logInfo "(Sys Info) Updated/inserted system information into database."
