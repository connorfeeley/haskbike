-- | CLI interface for database operations.
module CLI.Database
     ( dispatchDatabase
     , handleInformation
     , handleStatus
     ) where

import           API.Client
import           API.ResponseWrapper
import           API.Types

import           CLI.Options

import           Colog                  ( Message, WithLog, log, pattern D, pattern E, pattern I, pattern W )

import           Control.Lens
import           Control.Monad          ( unless, (<=<) )
import           Control.Monad.IO.Class ( MonadIO (liftIO) )

import           Data.Foldable          ( for_ )
import qualified Data.Text              as Text

import           Database.Beam.Postgres ( Connection )
import           Database.Migrations
import           Database.Operations
import           Database.Utils

import           Options.Applicative

import           Prelude                hiding ( log )

import           Servant.Client         ( ClientError )

import           System.Exit            ( exitSuccess )

import           UnliftIO               ( MonadUnliftIO )


-- | Helper functions.

-- | Reset the database.
reset :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => String -> m Connection
reset name = log W "Resetting database." >> setupDb name >>= \conn -> handleInformation conn >> pure conn

-- | Setup the database.
setupDb :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => String -> m Connection
setupDb name = liftIO $ setupDatabaseName name

-- | Get the database name from the CLI options.
dbname :: Options -> String
dbname = optDatabase


-- | Dispatch CLI arguments to the database interface.
dispatchDatabase :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => Options -> m Connection
dispatchDatabase options = case optCommand options of
  Poll _pollOptions
    | optEnableMigration options -> migrate (dbname options) >>= \conn -> pure conn
    | otherwise -> connectToDatabase (dbname options)
  Reset resetOptions -> handleReset options resetOptions
  _ -> connectToDatabase (dbname options)
  where
    connectToDatabase name = log I "Connecting to database." >> liftIO (connectDbName name)
    migrate     name    = log W "Migrating database." >> connectDbNameAndMigrate name
    connectDbNameAndMigrate = liftIO . (migrateDB' <=< connectDbName)
    migrateDB' :: Connection -> IO Connection
    migrateDB' conn = migrateDB conn >> return conn


-- | Handle the 'Reset' command.
handleReset :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => Options -> ResetOptions -> m Connection
handleReset options resetOptions = do
  log E $ "Reset command unimplemented. Parsed options: " <> (Text.pack . show) resetOptions

  if optResetOnly resetOptions
    then reset (dbname options) >> liftIO exitSuccess
    else reset (dbname options)


-- | Helper for station information request.
handleInformation :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => Connection -> m ()
handleInformation conn = do
  log D "Querying station information from API."
  infoQuery <- liftIO $ queryStationInformation conn
  log D "Queried station information from API."
  unless (null infoQuery) $ handleStationInformation conn

-- | Handle station information request.
handleStationInformation :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => Connection -> m ()
handleStationInformation conn = do
  stationInfo <- liftIO (runQueryWithEnv stationInformation :: IO (Either ClientError StationInformationResponse))
  for_ (rightToMaybe stationInfo) $ \response -> do
        let stations = response ^. response_data . info_stations
        liftIO (insertStationInformation conn stations) >>= report
  where
    report = log I . ("Stations inserted: " <>) . Text.pack . show . length
    rightToMaybe = either (const Nothing) Just

-- | Helper for station status request.
handleStatus :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => Connection -> m ()
handleStatus conn = do
  log D "Querying station status from API."
  statusQuery <- liftIO $ queryStationStatus conn
  log D "Queried station status from API."
  unless (null statusQuery) $ handleStationStatus conn

-- | Handle station status request.
handleStationStatus :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => Connection -> m ()
handleStationStatus conn = do
  stationStatus <- liftIO (runQueryWithEnv stationStatus :: IO (Either ClientError StationStatusResponse))
  for_ (rightToMaybe stationStatus) $ \response -> do
        let stations = response ^. response_data . status_stations
        liftIO (insertStationStatus conn stations) >>= report
  where
    report = log I . ("Status updated: " <>) . Text.pack . show .
      (\inserted -> length (inserted ^. insert_inserted) + length (inserted ^. insert_deactivated))
    rightToMaybe = either (const Nothing) Just
