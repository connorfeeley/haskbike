-- | Database V005 migrations.
--
-- No table-shape changes versus V004; this version exists to scope a new
-- raw-SQL migration (the @(info_station_id, last_reported)@ btree index
-- on @station_status@) under its own 'MigrationSteps' boundary instead
-- of accumulating indefinitely in V004's "extras" bag.

module Haskbike.Database.Schema.V005.Migrations
     ( migrateDB
     , migration
     ) where

import           Control.Arrow                                    ( (>>>) )
import           Control.Monad
import           Control.Monad.Catch                              ( MonadCatch, MonadThrow )

import           Data.Pool                                        ( withResource )

import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate                   as Pg
import           Database.PostgreSQL.Simple

import           Haskbike.AppEnv
import           Haskbike.Database.Schema.V001.Migrations         ( allowDestructive )
import qualified Haskbike.Database.Schema.V001.StationInformation as V001
import qualified Haskbike.Database.Schema.V001.StationStatus      as V001
import qualified Haskbike.Database.Schema.V001.SystemInformation  as V001
import qualified Haskbike.Database.Schema.V004.BikeShare          as V004
import qualified Haskbike.Database.Schema.V004.Migrations         as V004
import qualified Haskbike.Database.Schema.V004.StationOccupancy   as V004

import           UnliftIO


-- | V005 step: same table set as V004, no Beam-tracked schema change.
-- The actual DDL ('extraStatusMigrations') runs as part of 'migrateDB'
-- below, after Beam's own migration apply has recorded that this step
-- ran. The step is a versioning marker so future schema changes chain
-- from V005 rather than appending to V004's extras forever.
migrationStatusIndex :: CheckedDatabaseSettings Postgres V004.BikeshareDb
                     -> Migration Postgres (CheckedDatabaseSettings Postgres V004.BikeshareDb)
migrationStatusIndex oldDb =
  V004.BikeshareDb
    <$> preserve (V004._bikeshareEndpointQueriedType   oldDb)
    <*> preserve (V004._bikeshareStationInformation    oldDb)
    <*> preserve (V004._bikeshareStationStatus         oldDb)
    <*> preserve (V004._bikeshareStationStatusChanges  oldDb)
    <*> preserve (V004._bikeshareSystemInformation     oldDb)
    <*> preserve (V004._bikeshareSystemInformationCount oldDb)
    <*> preserve (V004._bikeshareQueryLog              oldDb)
    <*> preserve (V004._bikeshareStationLookup         oldDb)
    <*> preserve (V004._bikeshareStationOccupancy      oldDb)


migration :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres V004.BikeshareDb)
migration =
  V004.migration >>>
  migrationStep description migrationStatusIndex
  where
    description = "Add (info_station_id, last_reported) btree index on station_status. Critical for per-station time-bounded queries (integrateColumns, queryDockingEventsCount, queryStationStatusBetween, station_occupancy cache fill); without it those queries seq-scan station_status."


migrateDB :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
          => m (Maybe (CheckedDatabaseSettings Postgres V004.BikeshareDb))
migrateDB = do
  checkedDbSettings <- withPostgres $
    bringUpToDateWithHooks
    allowDestructive
    Pg.migrationBackend
    migration

  -- Idempotent raw-SQL migrations applied after the Beam chain. All are
  -- @CREATE INDEX IF NOT EXISTS@ / @ADD COLUMN IF NOT EXISTS@ / etc., so
  -- replays are no-ops.
  withConnPool >>= \pool -> void . liftIO . withResource pool $ \conn -> withTransaction conn $ do
    forM_ V001.extraVehicleTypeColumnMigrations           $ \mig -> execute_ conn mig
    forM_ V001.extraInfoIndexMigrations                   $ \mig -> execute_ conn mig
    forM_ V001.extraSystemInformationCountIndexMigrations $ \mig -> execute_ conn mig
    forM_ V001.extraStatusMigrations                      $ \mig -> execute_ conn mig
    forM_ V004.extraOccupancyMigrations                   $ \mig -> execute_ conn mig

  pure checkedDbSettings
