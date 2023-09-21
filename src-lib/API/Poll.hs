{- |
Poll the API for status updates, inserting results in database as needed.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module API.Poll
     ( main
     , pollClient
     ) where

import           API.Client
import           API.ResponseWrapper
import           API.Types                       ( StationStatusResponse, status_stations )

import           Common

import           Control.Concurrent              ( threadDelay )
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Lens

import           Database.Beam.Postgres          ( Connection )
import           Database.BikeShare              ( BeamReportTime, d_status_last_reported, d_status_station_id )
import           Database.Operations
import           Database.Utils

import           UnliftIO.Async


main :: IO ()
main = do
  -- Setup the database.
  conn <- setupDatabaseName dbnameTest

  pollClient conn

pollClient :: Connection -> IO ()
pollClient conn = do
  ttl <- newTVarIO (0 * 1000000)
  queue_resp <- newTBMQueueIO 4

  -- Request status every second.
  -- void $ forkIO $ forever $ fillQueue queue ttl

  concurrently_
    (statusRequester queue_resp ttl)
    (statusHandler conn queue_resp)

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

statusHandler :: Connection -> TBMQueue StationStatusResponse -> IO ()
statusHandler conn queue =
  loop'
  where
    loop' = do
      mnext <- atomically $ readTBMQueue queue
      case mnext of
        Nothing -> atomically retry
        Just response -> do
          let resp_data = response ^. response_data
          let status = resp_data ^. status_stations
          putStrLn $ "HANDLER: status_stations=" ++ show (length status)

          updated_api <- filterStatus conn status
          putStrLn $ "HANDLER: updated_api="  ++ show (length $ updated_api ^. filter_updated)
          putStrLn $ "HANDLER: same_api=" ++ show (length $ updated_api ^. filter_same)

          -- Insert the updated status.
          inserted_result <- insertUpdatedStationStatus conn status
          let message_data = zipWith (\ updated inserted ->
                                 ( inserted ^.. d_status_station_id
                                 , updated  ^.. d_status_last_reported
                                 , inserted ^.. d_status_last_reported
                                 ))
                                 (inserted_result ^. insert_deactivated) (inserted_result ^. insert_inserted)
          let messages = map (\(sid, last_reported, last_reported') ->
                   "ID: [" ++ show sid ++ "] " ++ -- ID
                   show last_reported ++ "->" ++ show last_reported' -- [prev reported] -> [new reported]
                ) message_data
          mapM_ putStrLn messages
          putStrLn $ "HANDLER: updated=" ++ show (length $ inserted_result ^. insert_deactivated) ++ " inserted=" ++ show (length $ inserted_result ^. insert_inserted)

          loop' -- Restart loop
