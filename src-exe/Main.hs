module Main
     ( main
     ) where

import           API.Client
import qualified API.Poll               as P
import           API.ResponseWrapper
import           API.Types

import           Control.Lens
import           Control.Monad          ( when )

import           Database.Beam.Postgres ( Connection )
import           Database.Operations
import           Database.Utils

import           Servant.Client         ( ClientError )


main :: IO ()
main = do
  -- Connect to and reset database.
  -- conn <- setupDatabaseName dbnameProduction

  -- Connect to database without resetting.
  conn <- connectDbName dbnameProduction

  -- Insert station information if missing from database.
  infoQuery <- queryStationInformation conn
  when (null infoQuery) $ handleStationInformation conn

  -- Insert station status if missing from database.
  -- statusQuery <- queryStationStatus conn
  -- when (null statusQuery) $ handleStationStatus conn

  P.pollClient conn

handleStationInformation :: Connection -- ^ Database connection.
                         -> IO ()
handleStationInformation conn = do
  info <- runQueryWithEnv stationInformation :: IO (Either ClientError StationInformationResponse)
  case info of
    Left err -> putStrLn $ "MAIN (ERROR): " ++ show err
    Right response -> do
      let stations = response ^. response_data . info_stations
      inserted <- insertStationInformation conn stations
      putStrLn $ "MAIN:     inserted info " ++ show (length inserted)

_handleStationStatus :: Connection -- ^ Database connection.
                     -> IO ()
_handleStationStatus conn = do
  status <- runQueryWithEnv stationStatus :: IO (Either ClientError StationStatusResponse)
  case status of
    Left err -> putStrLn $ "MAIN: error: " ++ show err
    Right response -> do
      let stationsStatus = response ^. response_data . status_stations
      inserted <- insertUpdatedStationStatus conn stationsStatus
      putStrLn $ "MAIN:     inserted status " ++ show (length (inserted ^. insert_inserted))
