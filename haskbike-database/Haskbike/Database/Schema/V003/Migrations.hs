-- |

module Haskbike.Database.Schema.V003.Migrations
     ( migrateDB
     , migration
     ) where

import           Control.Arrow                               ( (>>>) )
import           Control.Monad.Catch                         ( MonadCatch, MonadThrow )

import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate              as Pg

import           Haskbike.AppEnv
import           Haskbike.Database.Schema.V001.Migrations    ( allowDestructive )
import qualified Haskbike.Database.Schema.V001.StationStatus as V001
import qualified Haskbike.Database.Schema.V002.BikeShare     as V002
import qualified Haskbike.Database.Schema.V002.Migrations    as V002
import qualified Haskbike.Database.Schema.V003.BikeShare     as V003

import           UnliftIO                                    ( MonadIO, MonadUnliftIO )

migrationStationStatusChanges :: CheckedDatabaseSettings Postgres V002.BikeshareDb
                              -> Migration Postgres (CheckedDatabaseSettings Postgres V003.BikeshareDb)
migrationStationStatusChanges oldDb =
  V003.BikeshareDb
    <$> preserve (V002._bikeshareEndpointQueriedType oldDb)
    <*> preserve (V002._bikeshareStationInformation oldDb)
    <*> preserve (V002._bikeshareStationStatus oldDb)
    -- Add a new table with the same structure as station_status to store *only* changed rows.
    <*> V001.createStationStatus "station_status_changes"
    <*> preserve (V002._bikeshareSystemInformation oldDb)
    <*> preserve (V002._bikeshareSystemInformationCount oldDb)
    <*> preserve (V002._bikeshareQueryLog oldDb)
    <*> preserve (V002._bikeshareStationLookup oldDb)

migration :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres V003.BikeshareDb)
migration = V002.migration >>> migrationStep description migrationStationStatusChanges
  where description = "Add station status changes table (station_status_changes) to store only station_status rows that differ from the previous row (by station ID)"

migrateDB :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
          => m (Maybe (CheckedDatabaseSettings Postgres V003.BikeshareDb))
migrateDB = do
  withPostgres $ bringUpToDateWithHooks
    allowDestructive
    Pg.migrationBackend
    migration
