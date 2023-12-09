{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database.BikeShare.Migrations where

import           Database.Beam
import           Database.Beam.Backend              ( IsSql92DataTypeSyntax (..), timestampType )
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import           Database.Beam.Postgres.CustomTypes
import qualified Database.Beam.Postgres.Migrate     as PG
import           Database.Beam.Postgres.Syntax
import           Database.BikeShare


referenceInformationTable :: BeamMigrateSqlBackend be => Constraint be
referenceInformationTable = Constraint $ referencesConstraintSyntax "station_information" ["station_id"]
                            Nothing
                            (Just referentialActionCascadeSyntax)
                            Nothing

createStationInformation :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity StationInformationT))
createStationInformation =
  createTable "station_information" $ StationInformation
  { _infoId                    = field "id"                     PG.serial notNull unique
  , _infoStationId             = field "station_id"             int notNull unique
  , _infoName                  = field "name"                   (varchar (Just 100)) notNull
  , _infoPhysicalConfiguration = field "physical_configuration" physicalConfiguration
  , _infoLat                   = field "lat"                    double notNull
  , _infoLon                   = field "lon"                    double notNull
  , _infoAltitude              = field "altitude"               (maybeType double)
  , _infoAddress               = field "address"                (maybeType (varchar (Just 100)))
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
  }

createStationStatus :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity StationStatusT))
createStationStatus =
  createTable "station_status" $ StationStatus
  { _statusStationId             = StationInformationId $ field "station_id" int notNull referenceInformationTable
  , _statusLastReported          = field "last_reported"           (DataType (timestampType Nothing True))
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
  }

createSystemInformation :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity SystemInformationT))
createSystemInformation =
  createTable "system_information" $ SystemInformation
  { _sysInfKey                  = SystemInformationKey (field "id"         PG.serial notNull unique)
                                                       (field "reported"   (DataType (timestampType Nothing True)))
  , _sysInfBuildHash            = field "build_hash"                       (DataType pgTextType)
  , _sysInfBuildLabel           = field "build_label"                      (DataType pgTextType)
  , _sysInfBuildNumber          = field "build_number"                     (DataType pgTextType)
  , _sysInfBuildVersion         = field "build_version"                    (DataType pgTextType)
  , _sysInfLanguage             = field "language"                         (DataType pgTextType)
  , _sysInfMobileHeadVersion    = field "mobile_head_version"              int notNull
  , _sysInfMobileMinSuppVersion = field "mobile_minimum_supported_version" int notNull
  , _sysInfName                 = field "name"                             (DataType pgTextType)
  , _sysInfSysId                = field "system_id"                        (DataType pgTextType)
  , _sysInfTimeZone             = field "timezone"                         (DataType pgTextType)
  }

createSystemInformationCount :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity SystemInformationCountT))
createSystemInformationCount =
  createTable "system_information_count" $ SystemInformationCount
  { _sysInfCntKey             = SystemInformationKey (field "id"       PG.serial notNull unique)
                                                     (field "reported" (DataType (timestampType Nothing True)))
  , _sysInfCntStationCount    = field "station_count"                  int notNull
  , _sysInfCntMechanicalCount = field "mechanical_count"               int notNull
  , _sysInfCntEbikeCount      = field "ebike_count"                    int notNull
  }

createQueries :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity QueryLogT))
createQueries =
  createTable "queries" $ QueryLog
  { _queryLogId       = field "id"         PG.serial notNull unique
  , _queryLogTime     = field "time"       (DataType (timestampType Nothing True)) notNull
  , _queryLogEndpoint = field "endpoint"   endpointQueriedType notNull -- Using custom enum type.
  , _queryLogSuccess  = field "success"    boolean notNull
  , _queryLogErrMsg   = field "error_msg"  (DataType pgTextType)
  , _queryLogErrJson  = field "error_json" (maybeType jsonb)
  }

initialSetup :: Migration Postgres (CheckedDatabaseSettings Postgres BikeshareDb)
initialSetup = do
  BikeshareDb
  <$> enumSetup
  <*> createStationInformation
  <*> createStationStatus
  <*> createSystemInformation
  <*> createSystemInformationCount
  <*> createQueries


initialSetupStep :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres BikeshareDb)
initialSetupStep = migrationStep
  "initial_setup"
  (const initialSetup)

-- Beam's simple runner doesn't run destructive migrations
-- by default, so we have to override that.
allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks
  { runIrreversibleHook = pure True }

migrateDB :: Database.Beam.Postgres.Connection -> IO (Maybe (CheckedDatabaseSettings Postgres BikeshareDb))
migrateDB conn = runBeamPostgresDebug putStrLn conn $
  bringUpToDateWithHooks
    allowDestructive
    PG.migrationBackend
    initialSetupStep

enumSetupStep :: MigrationSteps Postgres () (CheckedDatabaseEntity Postgres db (PgType EndpointQueried))
enumSetupStep = migrationStep
  "enum_setup"
  (const enumSetup)

enumSetup :: Migration Postgres (CheckedDatabaseEntity Postgres db (PgType EndpointQueried))
enumSetup = createEndpointQueriedEnum
