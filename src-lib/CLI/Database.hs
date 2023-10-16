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

import           Colog                         ( log, pattern D, pattern I, pattern W )

import           Control.Lens
import           Control.Monad                 ( unless )

import           Data.Foldable                 ( for_ )
import qualified Data.Text                     as Text

import           Database.Beam.Postgres        ( Connection )
import           Database.BikeShare            ( bikeshareStationInformation, bikeshareStationStatus )
import           Database.BikeShare.Migrations
import           Database.BikeShare.Operations
import           Database.BikeShare.Utils

import           Formatting

import           Options.Applicative

import           Prelude                       hiding ( log )

import           Servant.Client                ( ClientError )

import           System.Exit                   ( exitSuccess )


-- | Helper functions.

-- | Dispatch CLI arguments to the database interface.
dispatchDatabase :: Options -> AppM Connection
dispatchDatabase options = do
  case optCommand options of
    Poll _pollOptions
      | optEnableMigration options -> do
          log D "Migrating database."
          withConn >>= liftIO . migrateDB
          log D "Migrated database."
          withConn >>= liftIO . pure
      | otherwise -> withConn >>= liftIO . pure
    Reset resetOptions -> handleReset options resetOptions >>= liftIO . pure
    _ -> withConn >>= liftIO . pure


-- | Handle the 'Reset' command.
handleReset :: (AppM ~ m, WithAppMEnv env Message m)  => Options -> ResetOptions -> m Connection
handleReset options resetOptions = do
  pPrintCompact options
  pPrintCompact resetOptions
  if optResetOnly resetOptions
    then log W "Only resetting database..." >> withConn >>= liftIO . dropTables >> log W "Database reset; exiting." >> liftIO exitSuccess
    else log W "Resetting database..."      >> withConn >>= liftIO . dropTables >> log W "Database reset." >>
         log W "Migrating database." >> withConn >>= liftIO . migrateDB >> log W "Migrations performed." >>
         log I "Initializing database." >> handleInformation >> liftIO exitSuccess

-- | Helper for station information request.
handleInformation :: AppM ()
handleInformation = do
  log D "Querying station information from database."
  numInfoRows <- queryRowCount bikeshareStationInformation
  log D "Queried station information from database."
  unless (null numInfoRows) handleStationInformation

-- | Handle station information request.
handleStationInformation :: AppM ()
handleStationInformation = do
  log D "Requesting station information from API."
  stationInfo <- liftIO (runQueryWithEnv stationInformation :: IO (Either ClientError StationInformationResponse))
  log D "Requested station information from API."

  for_ (rightToMaybe stationInfo) $ \response -> do
        let stations = response ^. response_data . unInfoStations
        log D "Inserting station information into database."
        insertStationInformation stations >>= report
        log D "Inserted station information into database."
  where
    report = log I . ("Stations inserted: " <>) . Text.pack . show . length
    rightToMaybe = either (const Nothing) Just

-- | Helper for station status request.
handleStatus :: AppM ()
handleStatus = do
  log D "Querying station status from database."
  numStatusRows <- queryRowCount bikeshareStationStatus
  log D "Queried station status from database."
  unless (null numStatusRows) handleStationStatus

-- | Handle station status request.
handleStationStatus :: AppM ()
handleStationStatus = do
  log D "Requesting station status from API."
  stationStatus' <- liftIO (runQueryWithEnv stationStatus :: IO (Either ClientError StationStatusResponse))
  log D "Requested station status from API."

  for_ (rightToMaybe stationStatus') $ \response -> do
        let stations = response ^. response_data . unStatusStations
        log D "Inserting station status into database."
        insertStationStatus stations >>= report
        log D "Inserted station status into database."
  where
    report = log I . ("Status updated: " <>) . Text.pack . show .
      (\inserted -> length inserted + length inserted)
    rightToMaybe = either (const Nothing) Just
