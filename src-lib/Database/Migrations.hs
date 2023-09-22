{-# LANGUAGE FlexibleContexts  #-}

module Database.Migrations where

import           Data.String                    ( fromString )

import           Database.Beam
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG
import           Database.BikeShare

referenceInformationTable :: BeamMigrateSqlBackend be => Constraint be
referenceInformationTable = Constraint $ referencesConstraintSyntax "station_information" ["station_id"]
                            Nothing
                            (Just referentialActionCascadeSyntax)
                            Nothing

initialSetup :: Migration Postgres
  (CheckedDatabaseSettings Postgres BikeshareDb)
initialSetup = BikeshareDb
  <$> (createTable "station_information" $ StationInformation
        { _info_id                     = field "id"                     PG.serial notNull unique
        , _info_station_id             = field "station_id"             int notNull unique
        , _info_name                   = field "name"                   (varchar (Just 100)) notNull
        , _info_physical_configuration = field "physical_configuration" physicalConfiguration
        , _info_lat                    = field "lat"                    double notNull
        , _info_lon                    = field "lon"                    double notNull
        , _info_altitude               = field "altitude"               (maybeType double)
        , _info_address                = field "address"                (varchar (Just 100)) notNull
        , _info_capacity               = field "capacity"               int notNull
        , _info_is_charging_station    = field "is_charging_station"    boolean notNull
        , _info_rental_methods         = field "rental_methods"         (unboundedArray rentalMethod)
        , _info_is_virtual_station     = field "is_virtual_station"     boolean notNull
        , _info_groups                 = field "groups"                 (unboundedArray (varchar (Just 100)))
        , _info_obcn                   = field "obcn"                   (varchar (Just 100)) notNull
        , _info_nearby_distance        = field "nearby_distance"        double notNull
        , _info_bluetooth_id           = field "bluetooth_id"           (varchar (Just 100)) notNull
        , _info_ride_code_support      = field "ride_code_support"      boolean notNull
        })
  <*> (createTable "station_status" $ StationStatus
        { _d_status_id                      = field "id"                      PG.serial notNull unique
        , _d_status_info_id                 = StationInformationId $ field "info_id" int notNull referenceInformationTable
        , _d_status_station_id              = field "station_id" int notNull
        , _d_status_num_bikes_available     = field "num_bikes_available"     int notNull
        , _d_status_num_bikes_disabled      = field "num_bikes_disabled"      int notNull
        , _d_status_num_docks_available     = field "num_docks_available"     int notNull
        , _d_status_num_docks_disabled      = field "num_docks_disabled"      int notNull
        , _d_status_last_reported           = field "last_reported"           (maybeType reportTimeType)
        , _d_status_is_charging_station     = field "is_charging_station"     boolean notNull
        , _d_status_status                  = field "status"                  stationStatusType
        , _d_status_is_installed            = field "is_installed"            boolean notNull
        , _d_status_is_renting              = field "is_renting"              boolean notNull
        , _d_status_is_returning            = field "is_returning"            boolean notNull
        , _d_status_traffic                 = field "traffic"                 (maybeType (varchar (Just 100)))
        , _d_status_vehicle_docks_available = field "vehicle_docks_available" int notNull
        , _d_status_vehicle_types_available = VehicleType (field "vehicle_types_available_boost"   int)
                                                          (field "vehicle_types_available_iconic"  int)
                                                          (field "vehicle_types_available_efit"    int)
                                                          (field "vehicle_types_available_efit_g5" int)
        , _d_status_active                  = field "active"                  boolean notNull
        })

initialSetupStep :: MigrationSteps Postgres
  ()
  (CheckedDatabaseSettings Postgres BikeshareDb)
initialSetupStep = migrationStep
  "initial_setup"
  (const initialSetup)

-- Beam's simple runner doesn't run destructive migrations
-- by default, so we have to override that.
allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks
  { runIrreversibleHook = pure True }

migrateDB :: Database.Beam.Postgres.Connection -> IO (Maybe (CheckedDatabaseSettings Postgres BikeshareDb))
migrateDB conn = runBeamPostgres conn $
  bringUpToDateWithHooks
    allowDestructive
    PG.migrationBackend
    initialSetupStep

bikeshareDB :: DatabaseSettings Postgres BikeshareDb
bikeshareDB = unCheckDatabase $ evaluateDatabase initialSetupStep

exampleQuery :: Database.Beam.Postgres.Connection -> IO [StationInformationT Identity]
exampleQuery conn = runBeamPostgres conn $
  runSelectReturningList $
    select (all_ (_bikeshareStationInformation bikeshareDB))

-- | Establish a connection to the database.
connectDb :: IO Connection
connectDb =
  connectPostgreSQL $ fromString "host=localhost port=5432 dbname=haskbike connect_timeout=10"
