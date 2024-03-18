-- | CLI interface for database operations.
module CLI.Database
     ( dispatchDatabase
     , handleInformation
     , handleStationInformation
     , handleStationStatus
     , handleStatus
     ) where

import           API.Client
import           API.ClientLifted              ( runQueryM )
import           API.ResponseWrapper
import           API.StationInformation
import           API.StationStatus

import           AppEnv

import           CLI.Options

import           Colog                         ( log, pattern D, pattern I, pattern W )

import           Control.Lens
import           Control.Monad                 ( unless )
import           Control.Monad.Catch           ( MonadCatch, MonadThrow )

import           Data.Foldable                 ( for_ )
import qualified Data.Text                     as Text

import           Database.BikeShare
import           Database.BikeShare.Operations
import           Database.BikeShare.Utils

import           Options.Applicative

import           Prelude                       hiding ( log )

import           Servant.Client                ( ClientError )

import           System.Exit                   ( exitSuccess )

import           Text.Pretty.Simple.Extras

import           UnliftIO                      ( MonadIO, MonadUnliftIO, liftIO )


-- | Helper functions.

-- | Dispatch CLI arguments to the database interface.
dispatchDatabase :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                 => Options -> m ()
dispatchDatabase options = do
  case optCommand options of
    Reset resetOptions -> handleReset options resetOptions
    _ | optEnableMigration options -> do
          log I "Migrating database."
          migrateDB
          log I "Migrated database."
      | otherwise -> pure ()


-- | Handle the 'Reset' command.
handleReset :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
            => Options -> ResetOptions -> m ()
handleReset options resetOptions = do
  pPrintCompact options
  pPrintCompact resetOptions
  if optResetOnly resetOptions
    then log W "Only resetting database..." >> dropTables >> migrateDB >> log W "Database reset; exiting." >> liftIO exitSuccess
    else log W "Resetting database..."      >> dropTables >> log W "Database reset." >>
         log W "Migrating database."        >> migrateDB >> log W "Migrations performed." >>
         log I "Initializing database."     >> handleInformation >> liftIO exitSuccess

-- | Helper for station information request.
handleInformation :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                  => m ()
handleInformation = do
  log D "Querying station information from database."
  numInfoRows <- queryRowCount bikeshareStationInformation
  log D "Queried station information from database."
  unless (0 == numInfoRows) handleStationInformation

-- | Handle station information request.
handleStationInformation :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                         => m ()
handleStationInformation = do
  log D "Requesting station information from API."
  stationInfo <- runQueryM stationInformation :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m) => m (Either ClientError (ResponseWrapper [StationInformation]))
  log D "Requested station information from API."

  for_ (rightToMaybe stationInfo) $ \response -> do
        let stations = response ^. respData
        let reported = response ^. respLastUpdated
        log D "Inserting station information into database."
        insertStationInformation reported stations >>= report
        log D "Inserted station information into database."
  where
    report = log I . ("Stations inserted: " <>) . Text.pack . show . length
    rightToMaybe = either (const Nothing) Just

-- | Helper for station status request.
handleStatus :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m) => m ()
handleStatus = do
  log D "Querying station status from database."
  numStatusRows <- queryRowCount bikeshareStationStatus
  log D "Queried station status from database."
  unless (0 == numStatusRows) handleStationStatus

-- | Handle station status request.
handleStationStatus :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                    => m ()
handleStationStatus = do
  log D "Requesting station status from API."
  stationStatus' <- runQueryM stationStatus :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m) => m (Either ClientError (ResponseWrapper [StationStatus]))
  log D "Requested station status from API."

  for_ (rightToMaybe stationStatus') $ \response -> do
        let stations = response ^. respData
        log D "Inserting station status into database."
        insertStationStatus stations >>= report
        log D "Inserted station status into database."
  where
    report = log I . ("Status updated: " <>) . Text.pack . show .
      (\inserted -> length inserted + length inserted)
    rightToMaybe = either (const Nothing) Just
