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

import           Colog                         ( Message, WithLog, log, logException, pattern D, pattern I, pattern W )

import           Control.Concurrent            ( threadDelay )
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad                 ( void )
import           Control.Monad.Cont            ( forever )

import qualified Data.Text                     as Text

import           Database.BikeShare            ( d_status_last_reported, d_status_station_id )
import           Database.BikeShare.Operations

import           Fmt

import           Prelude                       hiding ( log )

import           ReportTime                    ( localToPosix )

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
                  (statusRequester queueStatusResp ttl  lastUpdated)
                  (statusHandler queueStatusResp)
    spawnInfo   infoQueueResp ttl lastUpdated = concurrently_
                  (informationRequester infoQueueResp ttl lastUpdated)
                  (informationHandler infoQueueResp)


-- | Thread action to request station information from API.
statusRequester :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                => TBQueue StationStatusResponse -- ^ Queue of responses.
                -> TVar Int                      -- ^ Interval between requests in seconds.
                -> TVar Int                      -- ^ Last updated time.
                -> m ()
statusRequester queue intervalSecsVar lastUpdated = void . forever $ do
  intervalSecs <- liftIO $ readTVarIO intervalSecsVar
  liftIO (runQueryWithEnv stationStatus) >>= \case
    Left err -> logException err
    Right result -> do
      extendBy <- handleTimeElapsed "Status" result lastUpdated
      handleTTL "Status" result intervalSecsVar extendBy

      liftIO $ atomically $ writeTBQueue queue result

      log D $ format "(Status) Sleeping for {} seconds." intervalSecs
      liftIO $ threadDelay (intervalSecs * 1000000)


-- | Thread action to handle API response for station status query.
statusHandler :: TBQueue StationStatusResponse  -- ^ Queue of responses
              -> App ()
statusHandler queue = void . forever $ do
  response <- liftIO $ atomically $ readTBQueue queue

  let status = response ^. (response_data . unStatusStations)
  log I $ format "(Status) Received {} status records from API." (length status)

  updated_api <- separateNewerStatusRecords <$> withConn <*> pure status >>= liftIO

  -- Insert the updated status.
  inserted_result <- insertStationStatus <$> withConn <*> pure (updated_api ^. filter_newer) >>= liftIO

  -- Log each station ID updated.
  mapM_ (log D . Text.pack . fmtLog) $
    zipWith dataToTuple
      (inserted_result ^. insert_deactivated)
      (inserted_result ^. insert_inserted)

  -- Log counts of rows inserted and activated.
  log I $ format "(Status) Updated: {} | Inserted: {}"
    (length $ inserted_result ^. insert_deactivated)
    (length $ inserted_result ^. insert_inserted)

  where
    dataToTuple u i = (i ^. d_status_station_id, u ^. d_status_last_reported, i ^. d_status_last_reported)
    fmtLog (sid, lr, lr') = format "ID: {} {} {}" sid (maybe "-" show lr) (maybe "-" show lr')


-- | Handle last_updated field in response.
handleTimeElapsed :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                  => Text.Text         -- ^ Log prefix
                  -> ResponseWrapper a -- ^ TTL from API response.
                  -> TVar Int          -- ^ TVar holding last_updated between threads.
                  -> m (Maybe Int)     -- ^ Optional number of seconds extend TTL by, if last_updated went backwards.
handleTimeElapsed logPrefix apiResult lastUpdatedVar = do
  let currentTime' = localToPosix $ apiResult ^. response_last_updated
  previousTime <- liftIO $ readTVarIO lastUpdatedVar
  let timeElapsed = currentTime' - previousTime

  -- Check if last_updated went backwards
  if timeElapsed >= 0
    then do -- Update last_updated variable.
      liftIO $ atomically $ writeTVar lastUpdatedVar currentTime'
      pure Nothing
    else do
      log W $ format "{} last_updated went backwards: {} [{}] -> [{}]" logPrefix previousTime currentTime' timeElapsed
      pure (Just (-timeElapsed))


-- | Handle last_updated field in response.
handleTTL :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
          => Text.Text         -- ^ Log prefix
          -> ResponseWrapper a -- ^ TTL from API response.
          -> TVar Int          -- ^ TVar holding TTL.
          -> Maybe Int         -- ^ Optional number of seconds to extend TTL by.
          -> m ()
handleTTL logPrefix apiResult ttlVar extendBy = do
  let ttlSecs = apiResult ^. response_ttl
  log I $ format "({}) TTL={}" logPrefix (showt ttlSecs)
  liftIO $ atomically $ writeTVar ttlVar ttlSecs


-- | Thread action to request station information from API.
informationRequester :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                     => TBQueue StationInformationResponse -- ^ Queue of responses.
                     -> TVar Int                           -- ^ Interval between requests, in seconds.
                     -> TVar Int                           -- ^ Last updated time.
                     -> m ()
informationRequester queue intervalSecsVar lastUpdated = void . forever $ do
  intervalSecs <- liftIO $ readTVarIO intervalSecsVar
  liftIO (runQueryWithEnv stationInformation) >>= \case
    Left err -> logException err
    Right result -> do
      extendBy <- handleTimeElapsed "Info" result lastUpdated
      handleTTL "Info" result intervalSecsVar extendBy

      liftIO $ atomically $ writeTBQueue queue result

      log D $ format "(Info) Sleeping for {} seconds." intervalSecs
      liftIO $ threadDelay (intervalSecs * 1000000)

-- | Thread action to handle API response for station information query.
informationHandler :: TBQueue StationInformationResponse  -- ^ Queue of responses
                   -> App ()
informationHandler queue = void . forever $ do
  response <- liftIO $ atomically $ readTBQueue queue

  let info = response ^. (response_data . unInfoStations)
  log I $ format "(Info) Received {} info records from API." (length info)

  -- Insert the station information (updating existing records, if existing).
  inserted_result <- insertStationInformation <$> withConn <*> pure info >>= liftIO

  -- Log each station ID updated.
  mapM_ (log D . Text.pack . fmtLog) inserted_result

  -- Log counts of rows inserted and activated.
  log I $ format "(Info) Updated/inserted {} records into database." (length inserted_result)

  where
    fmtLog inserted = format "ID: {}" (show inserted)
