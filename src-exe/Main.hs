module Main
     ( main
     ) where

import           API.Client
import qualified API.Poll            as P
import           API.ResponseWrapper
import           API.Types

import           Control.Lens

import           Database.Operations
import           Database.Utils

import           Servant.Client      ( ClientError )


main :: IO ()
main = do
  -- Connect to and reset database.
  -- conn <- setupDatabaseName dbnameProduction

  -- Connect to database without resetting.
  conn <- connectDbName dbnameProduction

  -- Request station information and status from API.
  info   <- runQueryWithEnv stationInformation  :: IO (Either ClientError StationInformationResponse)
  status <- runQueryWithEnv stationStatus       :: IO (Either ClientError StationStatusResponse)
  
  -- Insert station information into database.
  case info of
    Left  err  -> putStrLn $ "MAIN (ERROR): " ++ show err
    Right response -> do
      inserted <- insertStationInformation conn $ response ^. response_data . info_stations
      putStrLn $ "MAIN:     inserted info " ++ show (length inserted)

  -- Insert station status into database.
  case status of
    Left  err  -> putStrLn $ "REQUEST: error: " ++ show err
    Right response -> do
      inserted <- insertUpdatedStationStatus conn $ response ^. response_data . status_stations
      putStrLn $ "MAIN:    inserted status " ++ show (length $ inserted ^. insert_inserted)
      putStrLn $ "MAIN: deactivated status " ++ show (length $ inserted ^. insert_deactivated)

  -- Run API poller method.
  P.pollClient conn
