-- | Database V004 migrations.

module Haskbike.Database.Schema.V004.Migrations
     ( migrateDB
     , migration
     ) where

import           Control.Arrow                                  ( (>>>) )
import           Control.Monad
import           Control.Monad.Catch                            ( MonadCatch, MonadThrow )

import           Data.Pool                                      ( withResource )

import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate                 as Pg
import           Database.PostgreSQL.Simple

import           Haskbike.AppEnv
import           Haskbike.Database.Schema.V001.Migrations       ( allowDestructive )
import qualified Haskbike.Database.Schema.V003.BikeShare        as V003
import qualified Haskbike.Database.Schema.V003.Migrations       as V003
import qualified Haskbike.Database.Schema.V004.BikeShare        as V004
import qualified Haskbike.Database.Schema.V004.StationOccupancy as V004

import           UnliftIO

migrationStationOccupancy :: CheckedDatabaseSettings Postgres V003.BikeshareDb
                          -> Migration Postgres (CheckedDatabaseSettings Postgres V004.BikeshareDb)
migrationStationOccupancy oldDb =
  V004.BikeshareDb
    <$> preserve (V003._bikeshareEndpointQueriedType oldDb)
    <*> preserve (V003._bikeshareStationInformation oldDb)
    <*> preserve (V003._bikeshareStationStatus oldDb)
    -- Add a new table with the same structure as station_status to store *only* changed rows.
    <*> preserve (V003._bikeshareStationStatusChanges oldDb)
    <*> preserve (V003._bikeshareSystemInformation oldDb)
    <*> preserve (V003._bikeshareSystemInformationCount oldDb)
    <*> preserve (V003._bikeshareQueryLog oldDb)
    <*> preserve (V003._bikeshareStationLookup oldDb)
    <*> V004.createStationOccupancy

migration :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres V004.BikeshareDb)
migration =
  V003.migration >>>
  migrationStep description migrationStationOccupancy
  where
    description = "Add station status changes table (station_status_changes) to store only station_status rows that differ from the previous row (by station ID)"

migrateDB :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
          => m (Maybe (CheckedDatabaseSettings Postgres V004.BikeshareDb))
migrateDB = do
  checkedDbSettings <- withPostgres $
    bringUpToDateWithHooks
    allowDestructive
    Pg.migrationBackend
    migration

  pool <- withConnPool
  void . liftIO . withResource pool $ \conn -> do
    forM_ V004.extraOccupancyMigrations $ \mig -> do
      execute_ conn mig
  pure checkedDbSettings
