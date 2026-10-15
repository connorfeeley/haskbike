-- | Database V006 migrations.
--
-- V006 changes the shape of the @queries@ table: it adds a nullable
-- @replayed_at@ column via 'Haskbike.Database.Schema.V006.QueryLogs.migrateQueriesAddReplayedAt'
-- (an @ALTER TABLE ... ADD COLUMN@ step), so it needs its own 'BikeshareDb'
-- bundle (V006.BikeShare) around the new 'V006.QueryLogT', the same way V002
-- bundled around a new station_lookup table and V004 around a new
-- station_occupancy table. V001..V005 are left untouched; a fresh database
-- still replays every version in order starting from V001.

module Haskbike.Database.Schema.V006.Migrations
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
import qualified Haskbike.Database.Schema.V004.StationOccupancy   as V004
import qualified Haskbike.Database.Schema.V005.Migrations         as V005
import qualified Haskbike.Database.Schema.V006.BikeShare          as V006
import qualified Haskbike.Database.Schema.V006.QueryLogs          as V006

import           UnliftIO


-- | V006 step: adds the nullable @replayed_at@ column to @queries@ via
-- 'V006.migrateQueriesAddReplayedAt' (@ALTER TABLE ... ADD COLUMN@); every
-- other table is preserved unchanged from V004's shape. Chains from V005 so
-- a fresh database still replays every version in order from V001.
migrationReplayedAt :: CheckedDatabaseSettings Postgres V004.BikeshareDb
                    -> Migration Postgres (CheckedDatabaseSettings Postgres V006.BikeshareDb)
migrationReplayedAt oldDb =
  V006.BikeshareDb
    <$> preserve (V004._bikeshareEndpointQueriedType   oldDb)
    <*> preserve (V004._bikeshareStationInformation    oldDb)
    <*> preserve (V004._bikeshareStationStatus         oldDb)
    <*> preserve (V004._bikeshareStationStatusChanges  oldDb)
    <*> preserve (V004._bikeshareSystemInformation     oldDb)
    <*> preserve (V004._bikeshareSystemInformationCount oldDb)
    <*> V006.migrateQueriesAddReplayedAt (V004._bikeshareQueryLog oldDb)
    <*> preserve (V004._bikeshareStationLookup         oldDb)
    <*> preserve (V004._bikeshareStationOccupancy      oldDb)


migration :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres V006.BikeshareDb)
migration =
  V005.migration >>>
  migrationStep description migrationReplayedAt
  where
    description = "Add replayed_at column to queries (replay tracking for failed API queries)."


migrateDB :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
          => m (Maybe (CheckedDatabaseSettings Postgres V006.BikeshareDb))
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
    forM_ V004.extraOccupancyKeyMigrations                $ \mig -> execute_ conn mig

  pure checkedDbSettings
