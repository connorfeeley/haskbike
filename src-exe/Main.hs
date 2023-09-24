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
import           Database.Migrations
import           Database.Operations
import           Database.Utils

import           Servant.Client         ( ClientError )

import           System.Environment
import           System.Exit            ( exitSuccess )


main :: IO ()
main = do
  -- Get command-line arguments
  args <- getArgs

  conn <- if "--migrate" `elem` args || "--migrate-only" `elem` args
    then do
      -- Perform database migrations.
      putStrLn "Migrating database..."
      conn' <- connectDbName dbnameProduction
      _ <- migrateDB conn'

      -- Exit if only migrations were requested.
      when ("--migrate-only" `elem` args) $
        putStrLn "Migration done - exiting" >>
        exitSuccess

      pure conn'
    else if "--reset" `elem` args then do
      -- Reset the database.
      putStrLn "Resetting database..."
      _ <- setupDatabaseName dbnameProduction
      -- Exit.
      exitSuccess
    -- Otherwise, connect to the database.
    else putStrLn "Connecting to database..." >>
         connectDbName dbnameProduction

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
