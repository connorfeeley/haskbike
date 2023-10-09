-- | Poll the API for status updates, inserting results in database as needed.
module CLI.Poll
     ( dispatchPoll
     , pollClient
     ) where

import           API.Client
import           API.ResponseWrapper
import           API.Types                     ( StationStatusResponse, unStatusStations )

import           AppEnv

import           CLI.Options                   ( PollOptions (..) )

import           Colog                         ( Message, WithLog, log, logException, pattern D, pattern E, pattern I )

import           Control.Concurrent            ( threadDelay )
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad                 ( void )
import           Control.Monad.Cont            ( forever )

import qualified Data.Text                     as Text
import           Data.Time

import           Database.BikeShare            ( d_status_last_reported, d_status_station_id )
import           Database.BikeShare.Operations

import           Fmt

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
  (ttl, last_updated, queue_resp) <- liftIO $
    (,,) <$> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4

  log I "Polling API for status updates."
  void $
    concurrently_
      (statusRequester queue_resp ttl)
      (statusHandler queue_resp last_updated)
  log I "Done."

statusRequester :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                => TBQueue StationStatusResponse -- ^ Queue of responses
                -> TVar Int                      -- ^ Interval between requests
                -> m ()
statusRequester queue interval_var = void . forever $ do
  interval <- liftIO $ readTVarIO interval_var
  log D $ "Sleeping for " <> showt (interval `div` 1000000) <> " seconds."
  liftIO $ threadDelay interval
  liftIO (runQueryWithEnv stationStatus) >>= \case
    Left err -> logException err
    Right result -> do
      currentTimeZone <- liftIO getCurrentTimeZone
      let time' = localToSystem currentTimeZone (result ^. response_last_updated)
      let ttl = result ^. response_ttl
      log I $ "TTL=" <> showt ttl <> " | last updated=" <> Text.pack (show time')
      liftIO $ atomically $ writeTVar interval_var (ttl * 1000000)
      liftIO $ atomically $ writeTBQueue queue result

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
    else log E $ Text.unwords ["last_updated went backwards: ", "[" <> showt previousTime <> "]", " -> ", "[" <> showt currentTime <> "]", "(", showt timeElapsed, ")"]

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
  log I $ "Updated " <> showt (length $ inserted_result ^. insert_deactivated)
   <> " | Inserted " <> showt (length $ inserted_result ^. insert_inserted)

  where
    dataToTuple u i = (i ^. d_status_station_id, u ^. d_status_last_reported, i ^. d_status_last_reported)
    fmtLog (sid, lr, lr') = "ID: " ++ show sid ++ " " ++ maybe "-" show lr ++ "->" ++ maybe "-" show lr'
