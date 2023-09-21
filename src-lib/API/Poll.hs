{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
-- |

module API.Poll
  ( main )
  where

import           API.Client
import           API.ResponseWrapper
import           API.Types                       (StationStatusResponse,
                                                  StationStatusResponseData (..),
                                                  status_stations,
                                                  status_status)
import           Common
import           Database.Operations
import           Database.Utils

import           Control.Concurrent              (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Lens
import           Control.Monad                   (forever, void)
import           Data.Foldable                   (for_)
import           Database.Beam
import           Database.Beam.Postgres
import           Database.Migrations             (migrateDB)
import           Database.PostgreSQL.Simple
import           Servant.Client
import           UnliftIO.Async


main :: IO ()
main = do
  ttl <- newTVarIO (1 * 1000000)
  queue_resp <- newTBMQueueIO 4

  -- Request status every second.
  -- void $ forkIO $ forever $ fillQueue queue ttl

  concurrently_
    (statusRequester queue_resp ttl)
    (statusHandler queue_resp)

statusRequester :: TBMQueue StationStatusResponse -> TVar Int -> IO ()
statusRequester queue interval_var = loop'
  where
    loop' = do
      interval <- readTVarIO interval_var
      putStrLn $ "REQUEST: interval=" ++ show (interval `div` 1000000) ++ "s"
      threadDelay interval

      response <- runQueryWithEnv stationStatus
      case response of
        Left  err  -> putStrLn $ "REQUEST: error: " ++ show err
        Right result -> do
          time' <- localToSystem $ result ^. response_last_updated
          let ttl = result ^. response_ttl
          putStrLn $ "REQUEST: TTL=" ++ show ttl ++ " | " ++ "last updated=" ++ show time'

          -- Update the interval
          atomically $ writeTVar interval_var (ttl * 1000000)

          -- Enqueue the response.
          putStrLn "REQUEST: enqueing response"
          atomically $ writeTBMQueue queue result

          loop' -- Restart loop

statusHandler :: TBMQueue StationStatusResponse -> IO ()
statusHandler queue = do
  loop'
  where
    loop' = do
      conn <- connectDb
      mnext <- atomically $ readTBMQueue queue
      case mnext of
        Nothing -> atomically retry
        Just response -> do
          let resp_data = response ^. response_data
          let status = resp_data ^. status_stations

          updated <- filterStatusUpdated conn status
          putStrLn $ "HANDLER: updated=" ++ show (length updated)

          -- Insert the updated status.
          _updated' <- insertUpdatedStationStatus conn updated

          loop' -- Restart loop
