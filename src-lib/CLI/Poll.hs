-- | Poll the API for status updates, inserting results in database as needed.
module CLI.Poll
     ( dispatchPoll
     , pollClient
     ) where

import           API.Client
import           API.ResponseWrapper
import           API.Types              ( StationStatusResponse, status_stations )

import           AppEnv

import           CLI.Options            ( PollOptions (..) )

import           Colog                  ( Message, WithLog, log, logException, pattern D, pattern E, pattern I )

import           Control.Concurrent     ( threadDelay )
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad          ( void )
import           Control.Monad.Cont     ( forever )

import qualified Data.Text              as Text
import           Data.Time

import           Database.BikeShare     ( d_status_last_reported, d_status_station_id )
import           Database.BikeShare.Operations

import           Fmt

import           Prelude                hiding ( log )

import           ReportTime             ( localToPosix, localToSystem )

import           UnliftIO               ( MonadIO, MonadUnliftIO, liftIO )
import           UnliftIO.Async         ( concurrently_ )


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
  log D $ "Sleeping for " <> Text.pack (show interval) <> " microseconds."
  liftIO $ threadDelay interval
  liftIO (runQueryWithEnv stationStatus) >>= \case
    Left err -> logException err
    Right result -> do
      currentTimeZone <- liftIO getCurrentTimeZone
      let time' = localToSystem currentTimeZone (result ^. response_last_updated)
      let ttl = result ^. response_ttl
      log I $ "TTL=" <> Text.pack (show ttl) <> " | last updated=" <> Text.pack (show time')
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
    else log E $ "last_updated went backwards: "
      <> show previousTime <> " -> " <> show currentTime
      <> "(" |++| show timeElapsed |++| ")"

  let status = response ^. (response_data . status_stations)
  log I $ "TTL=" <> Text.pack (show (length status))

  updated_api <- separateNewerStatusRecords <$> withConn <*> pure status >>= liftIO

  -- Insert the updated status.
  inserted_result <- insertStationStatus <$> withConn <*> pure (updated_api ^. filter_newer) >>= liftIO

  let message_data =
          zipWith
              ( \updated inserted ->
                  ( inserted ^.. d_status_station_id
                  , updated ^.. d_status_last_reported
                  , inserted ^.. d_status_last_reported
                  )
              )
              (inserted_result ^. insert_deactivated)
              (inserted_result ^. insert_inserted)
  let messages =
          map
              ( \(sid, last_reported, last_reported') ->
                  "ID: "
                      ++ show sid
                      ++ " "
                      ++ show last_reported -- ID
                      ++ "->"
                      ++ show last_reported' -- [prev reported] -> [new reported]
              )
              message_data
  mapM_ (log D . Text.pack) messages
  log I $
      "HANDLER: updated="
          <> Text.pack (show (length $ inserted_result ^. insert_deactivated))
          <> " inserted="
          <> Text.pack (show (length $ inserted_result ^. insert_inserted))
