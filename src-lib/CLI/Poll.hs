-- | Poll the API for status updates, inserting results in database as needed.
module CLI.Poll
     ( dispatchPoll
     , pollClient
     ) where

import           API.Client
import           API.ResponseWrapper
import           API.Types                     ( StationInformationResponse, StationStatusResponse, unInfoStations,
                                                 unStatusStations )

import           AppEnv

import           CLI.Options                   ( PollOptions (..) )

import           Colog                         ( Message, WithLog, log, logException, pattern D, pattern E, pattern I )

import           Control.Concurrent            ( threadDelay )
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad                 ( void )
import           Control.Monad.Cont            ( forever, lift )

import qualified Data.Text                     as Text
import           Data.Time

import           Database.BikeShare            ( d_status_last_reported, d_status_station_id )
import           Database.BikeShare.Operations

import           Prelude                       hiding ( log )

import           ReportTime                    ( localToPosix, localToSystem )

import           TextShow                      ( showt )

import           UnliftIO                      ( MonadIO, MonadUnliftIO, liftIO )
import           UnliftIO.Async                ( concurrently_ )


-- | Dispatch CLI arguments to the poller.
dispatchPoll :: PollOptions
             -> App ()
dispatchPoll _options = pollClient


pollClient :: App ()
pollClient = do
  (statusTtl, statusLastUpdated, statusQueueResp, infoTtl, infoLastUpdated, infoQueueResp) <- liftIO $
    (,,,,,) <$> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4 <*> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4

  log I "Polling API for status updates."
  void $
    concurrently_
      (spawnStatus statusQueueResp statusTtl statusLastUpdated)
      (spawnInfo infoQueueResp infoTtl infoLastUpdated)
  log I "Done."
  where
    spawnStatus queueStatusResp ttl lastUpdated = concurrently_
                  (statusRequester queueStatusResp ttl)
                  (statusHandler queueStatusResp lastUpdated)
    spawnInfo   infoQueueResp ttl lastUpdated = concurrently_
                  (informationRequester infoQueueResp ttl lastUpdated)
                  (informationHandler infoQueueResp)


-- | Thread action to request station information from API.
statusRequester :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                => TBQueue StationStatusResponse -- ^ Queue of responses
                -> TVar Int                      -- ^ Interval between requests
                -> m ()
statusRequester queue interval_var = void . forever $ do
  interval <- liftIO $ readTVarIO interval_var
  log D $ "(Status) Sleeping for " <> showt (interval `div` 1000000) <> " seconds."
  liftIO $ threadDelay interval
  liftIO (runQueryWithEnv stationStatus) >>= \case
    Left err -> logException err
    Right result -> do
      currentTimeZone <- liftIO getCurrentTimeZone
      let time' = localToSystem currentTimeZone (result ^. response_last_updated)
      let ttl = result ^. response_ttl
      log I $ "Status TTL=" <> showt ttl <> " | last updated=" <> Text.pack (show time')
      liftIO $ atomically $ writeTVar interval_var (ttl * 1000000)
      liftIO $ atomically $ writeTBQueue queue result


-- | Thread action to handle API response for station status query.
statusHandler :: TBQueue StationStatusResponse  -- ^ Queue of responses
              -> TVar Int                       -- ^ Last updated time
              -> App ()
statusHandler queue last_updated = void . forever $ do
  response <- liftIO $ atomically $ readTBQueue queue
  let currentTime = localToPosix $ response ^. response_last_updated

  previousTime <- liftIO $ readTVarIO last_updated

  let timeElapsed = currentTime - previousTime

  -- Check if last_updated went backwards
  if timeElapsed >= 0
    then -- Update last_updated variable.
      liftIO $ atomically $ writeTVar last_updated currentTime
    else log E $ Text.unwords ["(Status) last_updated went backwards: ", "[" <> showt previousTime <> "]", " -> ", "[" <> showt currentTime <> "]", "(", showt timeElapsed, ")"]

  let status = response ^. (response_data . unStatusStations)
  log D $ Text.unwords ["Received",  showt (length status),  "status records from API."]

  updated_api <- separateNewerStatusRecords <$> withConn <*> pure status >>= liftIO

  -- Insert the updated status.
  inserted_result <- insertStationStatus <$> withConn <*> pure (updated_api ^. filter_newer) >>= liftIO

  -- Log each station ID updated.
  mapM_ (log D . Text.pack . fmtLog) $
    zipWith dataToTuple
      (inserted_result ^. insert_deactivated)
      (inserted_result ^. insert_inserted)

  -- Log counts of rows inserted and activated.
  log I $ "(Status) Updated " <> showt (length $ inserted_result ^. insert_deactivated)
   <> " | Inserted " <> showt (length $ inserted_result ^. insert_inserted)

  where
    dataToTuple u i = (i ^. d_status_station_id, u ^. d_status_last_reported, i ^. d_status_last_reported)
    fmtLog (sid, lr, lr') = "ID: " ++ show sid ++ " " ++ maybe "-" show lr ++ "->" ++ maybe "-" show lr'

-- | Handle last_updated field in response.
handleTimeElapsed :: (WithLog env Message m, MonadIO m, MonadUnliftIO m) => LocalTime -> TVar Int -> m ()
handleTimeElapsed currentTime last_updated = do
  let currentTime' = localToPosix currentTime
  previousTime <- liftIO $ readTVarIO last_updated
  let timeElapsed = currentTime' - previousTime

  -- Check if last_updated went backwards
  if timeElapsed >= 0
    then -- Update last_updated variable.
      liftIO $ atomically $ writeTVar last_updated currentTime'
    else log E $ Text.unwords [ "(Info)", "last_updated went backwards: "
                              , "[" <> showt previousTime <> "]"
                              , " -> "
                              , "[" <> showt currentTime' <> "]"
                              , "(", showt timeElapsed, ")"
                              ]


-- | Thread action to request station information from API.
informationRequester :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                     => TBQueue StationInformationResponse -- ^ Queue of responses
                     -> TVar Int                           -- ^ Interval between requests
                     -> TVar Int                           -- ^ Last updated time
                     -> m ()
informationRequester queue interval_var lastUpdated = void . forever $ do
  interval <- liftIO $ readTVarIO interval_var
  log D $ "(Info) Sleeping for " <> showt (interval `div` 1000000) <> " seconds."
  liftIO (runQueryWithEnv stationInformation) >>= \case
    Left err -> logException err
    Right result -> do
      currentTimeZone <- liftIO getCurrentTimeZone
      let time' = localToSystem currentTimeZone (result ^. response_last_updated)
      let ttl = result ^. response_ttl
      log I $ "(Info) TTL=" <> showt ttl <> " | last updated=" <> Text.pack (show time')

      handleTimeElapsed (result ^. response_last_updated) lastUpdated

      liftIO $ atomically $ writeTVar interval_var (ttl * 1000000)
      liftIO $ atomically $ writeTBQueue queue result

-- | Thread action to handle API response for station information query.
informationHandler :: TBQueue StationInformationResponse  -- ^ Queue of responses
                   -> App ()
informationHandler queue = void . forever $ do
  response <- liftIO $ atomically $ readTBQueue queue

  let info = response ^. (response_data . unInfoStations)
  log D $ Text.unwords ["(Info)", "Received",  showt (length info),  "info records from API."]

  -- Insert the station information (updating existing records, if existing).
  inserted_result <- insertStationInformation <$> withConn <*> pure info >>= liftIO

  -- Log each station ID updated.
  mapM_ (log D . Text.pack . fmtLog) inserted_result

  -- Log counts of rows inserted and activated.
  log I $ "(Info) Updated/inserted " <> showt (length inserted_result)

  where
    fmtLog inserted = "ID: " ++ show inserted
