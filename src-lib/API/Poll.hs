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

import           Control.Applicative             ( Applicative (liftA2), liftA )
import           Control.Concurrent              ( threadDelay )
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Lens
import           Control.Monad                   ( when )

import           Data.Maybe                      ( listToMaybe )
import           Data.Time
import           Data.Time.Clock.POSIX

import           Database.Beam.Postgres          ( Connection )
import           Database.BikeShare              ( BeamReportTime (BeamReportTime), d_status_last_reported,
                                                   d_status_station_id )
import           Database.Operations
import           Database.Utils

import           Text.Pretty.Simple              ( pPrint, pPrintString )

import           UnliftIO.Async


main :: IO ()
main = do
  -- Setup the database.
  conn <- setupDatabaseName dbnameTest

  pollClient conn

pollClient :: Connection -> IO ()
pollClient conn = do
  ttl           <- newTVarIO (0 * 1000000)
  last_updated  <- newTVarIO 0
  queue_resp    <- newTBMQueueIO 4

  -- Request status every second.
  -- void $ forkIO $ forever $ fillQueue queue ttl

  concurrently_
    (statusRequester queue_resp ttl)
    (statusHandler conn queue_resp last_updated)

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

statusHandler :: Connection                     -- ^ Database connection
              -> TBMQueue StationStatusResponse -- ^ Queue of responses
              -> TVar Int                       -- ^ Last updated time
              -> IO ()
statusHandler conn queue last_updated =
  loop'
  where
    loop' = do
      mnext <- atomically $ readTBMQueue queue
      case mnext of
        Nothing -> atomically retry
        Just response -> do
          let currentTime = localToPosix $ response ^. response_last_updated

          previousTime <- readTVarIO last_updated

          let timeElapsed = currentTime - previousTime

          -- Check if last_updated went backwards
          case timeElapsed > 0 of
            True  ->
              -- Update last_updated variable.
              atomically $ writeTVar last_updated currentTime
            False -> do
              putStrLn $ "HANDLER: last_updated went backwards: " ++ show previousTime ++ " -> " ++ show currentTime ++ "(" ++ show timeElapsed ++ ")"

          let status = response ^. (response_data . status_stations)
          putStrLn $ "HANDLER: status_stations=" ++ show (length status)

          updated_api <- separateNewerStatusRecords conn status
          -- pPrint $ listToMaybe status
          -- pPrint $ listToMaybe $ updated_api ^. filter_newer
          -- pPrint $ listToMaybe $ updated_api ^. filter_unchanged

          putStrLn $ "HANDLER:     newer="  ++ show (length $ updated_api ^. filter_newer)
          putStrLn $ "HANDLER: unchanged="     ++ show (length $ updated_api ^. filter_unchanged)

          -- Insert the updated status.
          inserted_result <- insertUpdatedStationStatus conn $ updated_api ^. filter_newer

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
