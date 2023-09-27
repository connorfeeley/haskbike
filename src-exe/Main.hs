{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}

module Main
     ( main
     ) where

import           API.Client
import qualified API.Poll               as P
import           API.ResponseWrapper
import           API.Types

import           AppEnv

import           Colog                  ( Message, WithLog, log, pattern I, pattern W )
import           Colog.Message          ( logException )

import           Control.Lens
import           Control.Monad          ( when )
import           Control.Monad.IO.Class ( MonadIO (liftIO) )

import qualified Data.Text              as Text

import           Database.Beam.Postgres ( Connection )
import           Database.Migrations
import           Database.Operations
import           Database.Utils

import           Prelude                hiding ( log )

import           Servant.Client         ( ClientError )

import           System.Environment
import           System.Exit            ( exitSuccess )

import           UnliftIO               ( MonadUnliftIO )

main :: IO ()
main = runApp simpleEnv appMain

appMain :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
        => m ()
appMain = do
  -- Get command-line arguments
  args <- liftIO getArgs

  conn <- if "--migrate" `elem` args || "--migrate-only" `elem` args
    then do
      -- Perform database migrations.
      log W "Migrating database."

      conn' <- liftIO $ connectDbName dbnameProduction
      _ <- liftIO $ migrateDB conn'

      -- Exit if only migrations were requested.
      when ("--migrate-only" `elem` args) $
        log W "Migrating database." >>
        liftIO exitSuccess

      pure conn'
    else if "--reset" `elem` args then do
      -- Reset the database.
      log W "Resetting database."
      conn <- liftIO $ setupDatabaseName dbnameProduction

      -- Insert station information if missing from database.
      infoQuery <- liftIO $ queryStationInformation conn
      when (null infoQuery) $ handleStationInformation conn
      -- Exit.
      liftIO exitSuccess
    -- Otherwise, connect to the database.
    else log I "Connecting to database." >> liftIO (connectDbName dbnameProduction)

  -- Insert station information if missing from database.
  infoQuery <- liftIO $ queryStationInformation conn
  when (null infoQuery) $ handleStationInformation conn

  P.pollClient conn

handleStationInformation :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                         => Connection -- ^ Database connection.
                         -> m ()
handleStationInformation conn = do
  information <- liftIO (runQueryWithEnv stationInformation :: IO (Either ClientError StationInformationResponse))
  case information of
    Left err -> logException err
    Right response -> do
      let stations = response ^. response_data . info_stations
      inserted <- liftIO $ insertStationInformation conn stations
      log I $ "Line length: " <> Text.pack (show $ length inserted)
