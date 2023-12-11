{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
import           API.Pollable
import           API.ResponseWrapper
import qualified API.StationInformation                       as AT
import qualified API.StationStatus                            as AT
import qualified API.SystemInformation                        as AT

import           AppEnv

import           CLI.Options                                  ( PollOptions (..) )
import           CLI.Poll.Utils

import           Colog                                        ( logDebug, logInfo )

import           Control.Lens
import           Control.Monad                                ( forever, void )

import qualified Data.Text                                    as T
import           Data.Time

import           Database.BikeShare.EndpointQueried
import           Database.BikeShare.Operations
import           Database.BikeShare.Tables.StationInformation
import qualified Database.BikeShare.Tables.StationInformation as DB
import           Database.BikeShare.Tables.StationStatus
import qualified Database.BikeShare.Tables.StationStatus      as DB
import qualified Database.BikeShare.Tables.SystemInformation  as DB

import           Fmt                                          ( format )

import           Prelude                                      hiding ( log )

import           Text.Pretty.Simple.Extras                    ( pShowCompact )

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
    handlerStationInformation prepareData
    handlerStationStatus prepareData
    handlerSystemInformation prepareData dbInsert SystemInformationEP

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


handlerStationInformation :: ( ApiFetcher AppM [AT.StationInformation]
                             , ApiConverter    [AT.StationInformation] [AT.StationInformation]
                             , DbInserter AppM  AT.StationInformation   DB.StationInformationT Identity
                             )
                          => (ResponseWrapper [AT.StationInformation] -> [AT.StationInformation]) -> AppM ()
handlerStationInformation prepare = void $ do
  -- Convert API records to database entities and handle failures if necessary.
  apiResult <- fetchFromApi
  case apiResult of
    Left err -> handleResponseError StationInformationEP err
    Right resp -> do
      handleResponseSuccess StationInformationEP (_respLastUpdated resp)
      insertedResult <- dbInsert (prepare resp) :: AppM [DB.StationInformationT Identity]
      logInfo $ format "(Info) Updated/inserted {} records into database." (length insertedResult)

handlerStationStatus :: ( ApiFetcher AppM [AT.StationStatus]
                        , ApiConverter    [AT.StationStatus] [AT.StationStatus]
                        , DbInserter AppM AT.StationStatus DB.StationStatusT Identity
                        ) => (ResponseWrapper [AT.StationStatus] -> [AT.StationStatus]) -> AppM ()
handlerStationStatus prepare = void $ do
  -- Convert API records to database entities and handle failures if necessary.
  apiResult <- fetchFromApi
  case apiResult of
    Left err -> handleResponseError StationStatusEP err
    Right resp -> do
      handleResponseSuccess StationStatusEP (_respLastUpdated resp)
      insertedResult <- dbInsert (prepare resp) :: AppM [DB.StationStatusT Identity]
      logInfo $ format "(Info) Updated/inserted {} records into database." (length insertedResult)

handlerSystemInformation :: ( ApiFetcher AppM AT.SystemInformation
                            , ApiConverter    AT.SystemInformation (UTCTime, AT.SystemInformation)
                            , DbInserter AppM (UTCTime, AT.SystemInformation) DB.SystemInformationT Identity
                            , HasTable DB.SystemInformationT
                            )
                         => (ResponseWrapper AT.SystemInformation -> (UTCTime, AT.SystemInformation))
                         -> ([(UTCTime, AT.SystemInformation)] -> AppM [DB.SystemInformationT Identity])
                         -> EndpointQueried
                         -> AppM ()
handlerSystemInformation prepare insertDb ep = void $ do
  -- Convert API records to database entities and handle failures if necessary.
  apiResult <- fetchFromApi
  case apiResult of
    Left err -> handleResponseError ep err
    Right resp -> do
      handleResponseSuccess ep (_respLastUpdated resp)
      insertedResult <- insertDb [prepare resp]
      logInfo $ format "(Info) Updated/inserted {} records into database." (length insertedResult)

-- genericHandler :: ( ApiFetcher a1
--                   , DbInserter AppM a2 d dm
--                   , HasTable d
--                   ) => ClientM a1 -> (a1 -> ([a1] -> [a2])) -> AppM ()
-- genericHandler clientFn transformFn = void $ do
--   -- Convert API records to database entities and handle failures if necessary.
--   apiResult <- runQueryM clientFn
--   case apiResult of
--     Left err -> handleResponseError SystemInformationEP err
--     Right resp -> do
--       handleResponseSuccess SystemInformationEP (_respLastUpdated resp)
--       insertedResult <- dbInsert [(_respLastUpdated resp, _respData resp)] :: AppM [DB.SystemInformationT Identity]
--       logInfo $ format "(Info) Updated/inserted {} records into database." (length insertedResult)


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
