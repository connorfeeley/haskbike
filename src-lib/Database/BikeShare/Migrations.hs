{-# LANGUAGE FlexibleContexts #-}

module Database.BikeShare.Migrations where

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
        { _infoId                    = field "id"                     PG.serial notNull unique
        , _infoStationId             = field "station_id"             int notNull unique
        , _infoName                  = field "name"                   (varchar (Just 100)) notNull
        , _infoPhysicalConfiguration = field "physical_configuration" physicalConfiguration
        , _infoLat                   = field "lat"                    double notNull
        , _infoLon                   = field "lon"                    double notNull
        , _infoAltitude              = field "altitude"               (maybeType double)
        , _infoAddress               = field "address"                (varchar (Just 100)) notNull
        , _infoCapacity              = field "capacity"               int notNull
        , _infoIsChargingStation     = field "is_charging_station"    boolean notNull
        , _infoRentalMethods         = field "rental_methods"         (unboundedArray rentalMethod)
        , _infoIsValetStation        = field "is_valet_station"       boolean notNull
        , _infoIsVirtualStation      = field "is_virtual_station"     boolean notNull
        , _infoGroups                = field "groups"                 (unboundedArray (varchar (Just 100)))
        , _infoObcn                  = field "obcn"                   (varchar (Just 100)) notNull
        , _infoNearbyDistance        = field "nearby_distance"        double notNull
        , _infoBluetoothId           = field "bluetooth_id"           (varchar (Just 100)) notNull
        , _infoRideCodeSupport       = field "ride_code_support"      boolean notNull
        , _infoRentalUris            = field "rental_uris"            (unboundedArray (varchar (Just 100)))
        , _infoActive                = field "active"                 boolean notNull
        })
  <*> (createTable "station_status" $ StationStatus
        { _statusStationId             = StationInformationId $ field "info_id" int notNull referenceInformationTable
        , _statusLastReported          = field "last_reported"           (maybeType reportTimeType)
        , _statusNumBikesAvailable     = field "num_bikes_available"     int notNull
        , _statusNumBikesDisabled      = field "num_bikes_disabled"      int notNull
        , _statusNumDocksAvailable     = field "num_docks_available"     int notNull
        , _statusNumDocksDisabled      = field "num_docks_disabled"      int notNull
        , _statusIsChargingStation     = field "is_charging_station"     boolean notNull
        , _statusStatus                = field "status"                  stationStatusType
        , _statusIsInstalled           = field "is_installed"            boolean notNull
        , _statusIsRenting             = field "is_renting"              boolean notNull
        , _statusIsReturning           = field "is_returning"            boolean notNull
        , _statusTraffic               = field "traffic"                 (maybeType (varchar (Just 100)))
        , _statusVehicleDocksAvailable = field "vehicle_docks_available" int notNull
        , _statusVehicleTypesAvailable = VehicleType (field "vehicle_types_available_boost"   int)
                                                     (field "vehicle_types_available_iconic"  int)
                                                     (field "vehicle_types_available_efit"    int)
                                                     (field "vehicle_types_available_efit_g5" int)
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
