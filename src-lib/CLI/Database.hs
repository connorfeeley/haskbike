-- | CLI interface for database operations.
module CLI.Database
     ( dispatchDatabase
     , handleInformation
     , handleStatus
     ) where

import           API.Client
import           API.ResponseWrapper
import           API.Types

import           AppEnv

import           CLI.Options

import           Colog                         ( Message, WithLog, log, pattern D, pattern E, pattern I, pattern W )

import           Control.Lens
import           Control.Monad                 ( unless, void, (<=<) )
import           Control.Monad.IO.Class        ( MonadIO (liftIO) )
import           Control.Monad.Reader          ( asks, lift )

import           Data.Foldable                 ( for_ )
import qualified Data.Text                     as Text

import           Database.Beam.Postgres        ( Connection )
import           Database.BikeShare            ( bikeshareStationInformation, bikeshareStationStatus )
import           Database.BikeShare.Migrations
import           Database.BikeShare.Operations
import           Database.BikeShare.Utils

import           Options.Applicative

import           Prelude                       hiding ( log )

import           Servant.Client                ( ClientError )

import           System.Exit                   ( exitFailure, exitSuccess )

import           UnliftIO                      ( MonadUnliftIO )


-- | Helper functions.

-- | Reset the database.
reset :: String -> App ()
reset name =
  log W "Resetting database."
  >> resetDb name
  >> handleInformation

-- | Setup the database.
resetDb :: String -> App Connection
resetDb name =
  (dropTables <$> withConn)
  >> (migrateDatabase <$> withConn)
  >>= liftIO

-- | Get the database name from the CLI options.
dbname :: Options -> String
dbname = optDatabase


-- | Dispatch CLI arguments to the database interface.
dispatchDatabase :: Options -> App Connection
dispatchDatabase options = do
  case optCommand options of
    Poll _pollOptions
      | optEnableMigration options -> do
          log D "Migrating database."
          mig <- withConn >>= liftIO . migrateDB
          log D "Migrated database."
          withConn >>= liftIO . pure
      | otherwise -> withConn >>= liftIO . pure
    Reset resetOptions -> handleReset options resetOptions >>= liftIO . pure
    _ -> withConn >>= liftIO . pure


-- | Handle the 'Reset' command.
handleReset :: (App ~ m, WithAppEnv env Message m)  => Options -> ResetOptions -> m Connection
handleReset options resetOptions = do
  if optResetOnly resetOptions
    then reset (dbname options) >> liftIO exitSuccess
    else reset (dbname options) >> liftIO exitFailure

-- | Helper for station information request.
handleInformation :: App ()
handleInformation = do
  log D "Querying station information from database."
  numInfoRows <- queryRowCount <$> withConn <*> pure bikeshareStationInformation >>= liftIO
  log D "Queried station information from database."
  unless (null numInfoRows) handleStationInformation

-- | Handle station information request.
handleStationInformation :: App ()
handleStationInformation = do
  log D "Requesting station information from API."
  stationInfo <- liftIO (runQueryWithEnv stationInformation :: IO (Either ClientError StationInformationResponse))
  log D "Requested station information from API."

  for_ (rightToMaybe stationInfo) $ \response -> do
        let stations = response ^. response_data . info_stations
        log D "Inserting station information into database."
        numInfoRows <- insertStationInformation <$> withConn <*> pure stations >>= liftIO >>= report
        log D "Inserted station information into database."
  where
    report = log I . ("Stations inserted: " <>) . Text.pack . show . length
    rightToMaybe = either (const Nothing) Just

-- | Helper for station status request.
handleStatus :: App ()
handleStatus = do
  log D "Querying station status from database."
  numStatusRows <- queryRowCount <$> withConn <*> pure bikeshareStationStatus >>= liftIO
  log D "Queried station status from database."
  unless (null numStatusRows) handleStationStatus

-- | Handle station status request.
handleStationStatus :: App ()
handleStationStatus = do
  log D "Requesting station status from API."
  stationStatus <- liftIO (runQueryWithEnv stationStatus :: IO (Either ClientError StationStatusResponse))
  log D "Requested station status from API."

  for_ (rightToMaybe stationStatus) $ \response -> do
        let stations = response ^. response_data . status_stations
        log D "Inserting station status into database."
        numStatusRows <- insertStationStatus <$> withConn <*> pure stations >>= liftIO >>= report
        log D "Inserted station status into database."
  where
    report = log I . ("Status updated: " <>) . Text.pack . show .
      (\inserted -> length (inserted ^. insert_inserted) + length (inserted ^. insert_deactivated))
    rightToMaybe = either (const Nothing) Just
