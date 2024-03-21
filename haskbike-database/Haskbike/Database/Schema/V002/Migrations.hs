-- |

module Haskbike.Database.Schema.V002.Migrations
     ( migrateDB
     , migration
     ) where

import           Control.Arrow                               ( (>>>) )

import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate              as Pg

import           Haskbike.AppEnv
import qualified Haskbike.Database.Schema.V001.BikeShare     as V001
import           Haskbike.Database.Schema.V001.Migrations    ( allowDestructive )
import qualified Haskbike.Database.Schema.V001.Migrations    as V001
import qualified Haskbike.Database.Schema.V002.BikeShare     as V002
import qualified Haskbike.Database.Schema.V002.StationLookup as V002

migrationStationLookup :: CheckedDatabaseSettings Postgres V001.BikeshareDb
                       -> Migration Postgres (CheckedDatabaseSettings Postgres V002.BikeshareDb)
migrationStationLookup oldDb =
  V002.BikeshareDb
    <$> preserve (V001._bikeshareEndpointQueriedType oldDb)
    <*> preserve (V001._bikeshareStationInformation oldDb)
    <*> preserve (V001._bikeshareStationStatus oldDb)
    <*> preserve (V001._bikeshareSystemInformation oldDb)
    <*> preserve (V001._bikeshareSystemInformationCount oldDb)
    <*> preserve (V001._bikeshareQueryLog oldDb)
    <*> V002.createStationLookup

migration :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres V002.BikeshareDb)
migration = V001.initialSetupStep >>> migrationStep "Add station lookup table" migrationStationLookup

migrateDB :: AppM (Maybe (CheckedDatabaseSettings Postgres V002.BikeshareDb))
migrateDB = do
  withPostgres $ bringUpToDateWithHooks
    allowDestructive
    Pg.migrationBackend
    migration
