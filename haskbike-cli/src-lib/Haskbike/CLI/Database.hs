-- | CLI interface for database operations.
module Haskbike.CLI.Database
     ( dispatchDatabase
     , handleInformation
     , handleStationInformation
     , handleStationStatus
     , handleStatus
     ) where

import           Colog                           ( logDebug, logInfo, logWarning )

import           Control.Lens
import           Control.Monad                   ( unless )
import           Control.Monad.Catch             ( MonadCatch, MonadThrow )

import           Data.Foldable                   ( for_ )
import qualified Data.Text                       as Text

import           Haskbike.API.Client
import           Haskbike.API.ClientLifted       ( runQueryM )
import           Haskbike.API.ResponseWrapper
import           Haskbike.API.StationInformation
import           Haskbike.API.StationStatus
import           Haskbike.AppEnv
import           Haskbike.CLI.Options
import           Haskbike.Database.BikeShare
import           Haskbike.Database.Operations
import           Haskbike.Database.Utils

import           Options.Applicative

import           Prelude                         hiding ( log )

import           Servant.Client                  ( ClientError )

import           System.Exit                     ( exitSuccess )

import           Text.Pretty.Simple.Extras

import           UnliftIO                        ( MonadIO, MonadUnliftIO, liftIO )


-- | Helper functions.

-- | Dispatch CLI arguments to the database interface.
dispatchDatabase :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                 => Options -> m ()
dispatchDatabase options = do
  case optCommand options of
    Reset resetOptions -> handleReset options resetOptions
    _ | optEnableMigration options -> do
          logInfo "Migrating database."
          migrateDB
          logInfo "Migrated database."
      | otherwise -> pure ()


-- | Handle the 'Reset' command.
handleReset :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
            => Options -> ResetOptions -> m ()
handleReset options resetOptions = do
  pPrintCompact options
  pPrintCompact resetOptions
  if optResetOnly resetOptions
    then logWarning "Only resetting database..." >> dropTables >> migrateDB >> logWarning "Database reset; exiting." >> liftIO exitSuccess
    else logWarning "Resetting database..."      >> dropTables >> logWarning "Database reset." >>
         logWarning "Migrating database."        >> migrateDB >> logWarning "Migrations performed." >>
         logInfo "Initializing database."     >> handleInformation >> liftIO exitSuccess

-- | Helper for station information request.
handleInformation :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                  => m ()
handleInformation = do
  logDebug "Querying station information from database."
  numInfoRows <- queryRowCount bikeshareStationInformation
  logDebug "Queried station information from database."
  unless (0 == numInfoRows) handleStationInformation

-- | Handle station information request.
handleStationInformation :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                         => m ()
handleStationInformation = do
  logDebug "Requesting station information from API."
  stationInfo <- runQueryM stationInformation :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m) => m (Either ClientError (ResponseWrapper [StationInformation]))
  logDebug "Requested station information from API."

  for_ (rightToMaybe stationInfo) $ \response -> do
        let stations = response ^. respData
        let reported = response ^. respLastUpdated
        logDebug "Inserting station information into database."
        insertStationInformation reported stations >>= report
        logDebug "Inserted station information into database."
  where
    report = logInfo . ("Stations inserted: " <>) . Text.pack . show . length
    rightToMaybe = either (const Nothing) Just

-- | Helper for station status request.
handleStatus :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m) => m ()
handleStatus = do
  logDebug "Querying station status from database."
  numStatusRows <- queryRowCount bikeshareStationStatus
  logDebug "Queried station status from database."
  unless (0 == numStatusRows) handleStationStatus

-- | Handle station status request.
handleStationStatus :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                    => m ()
handleStationStatus = do
  logDebug "Requesting station status from API."
  stationStatus' <- runQueryM stationStatus :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m) => m (Either ClientError (ResponseWrapper [StationStatus]))
  logDebug "Requested station status from API."

  for_ (rightToMaybe stationStatus') $ \response -> do
        let stations = response ^. respData
        logDebug "Inserting station status into database."
        insertStationStatus stations >>= report
        logDebug "Inserted station status into database."
  where
    report = logInfo . ("Status updated: " <>) . Text.pack . show .
      (\inserted -> length inserted + length inserted)
    rightToMaybe = either (const Nothing) Just
