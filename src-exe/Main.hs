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

  -- Insert station information if missing from database.
  queryStationInformation conn >>= \info_rows ->
    case length info_rows of
      0 -> do -- Request station information from API.
        info   <- runQueryWithEnv stationInformation  :: IO (Either ClientError StationInformationResponse)
        -- Insert station information into database.
        case info of
          Left  err  -> putStrLn $ "MAIN (ERROR): " ++ show err
          Right response -> do
            inserted <- insertStationInformation conn $ response ^. response_data . info_stations
            putStrLn $ "MAIN:     inserted info " ++ show (length inserted)
      _ -> putStrLn "MAIN:     station information already present"

  -- Insert station status if missing from database.
  queryStationStatus conn >>= \status_rows ->
    case length status_rows of
      0 -> do -- Request station status from API.
        status <- runQueryWithEnv stationStatus       :: IO (Either ClientError StationStatusResponse)
        -- Insert station status into database.
        case status of
          Left  err  -> putStrLn $ "REQUEST: error: " ++ show err
          Right response -> do
            inserted <- insertUpdatedStationStatus conn $ response ^. response_data . status_stations
            putStrLn $ "MAIN:    inserted status " ++ show (length $ inserted ^. insert_inserted)
            putStrLn $ "MAIN: deactivated status " ++ show (length $ inserted ^. insert_deactivated)
      _ -> putStrLn "MAIN:    station status already present"

  -- Run API poller method.
  P.pollClient conn
