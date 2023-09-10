{-# LANGUAGE OverloadedStrings #-}

module Database.Migrations where

import           Database.BikeShare
import qualified Database.StationInformation    as DSI
import qualified Database.StationStatus         as DSS

import           Database.Beam
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG

import           Data.String                    (fromString)

-- It's unfortunate that we have to define this ourselves.
-- utctime :: BeamSqlBackend be => DataType be UTCTime
-- utctime = DataType (timestampType Nothing True)

initialSetup :: Migration Postgres
  (CheckedDatabaseSettings Postgres BikeshareDb)
initialSetup = BikeshareDb
  <$> (createTable "station_information" $ DSI.StationInformation
        { DSI._information_id = field "id" PG.serial notNull unique
        , DSI._information_station_id = field "station_id" int notNull unique
        , DSI._information_name = field "name" (varchar (Just 100)) notNull
        , DSI._information_physical_configuration = field "physical_configuration" DSI.physicalConfiguration
        , DSI._information_lat = field "lat" double notNull
        , DSI._information_lon = field "lon" double notNull
        , DSI._information_altitude = field "altitude" double notNull
        , DSI._information_address = field "address" (varchar (Just 100)) notNull
        , DSI._information_capacity = field "capacity" int notNull
        , DSI._information_is_charging_station = field "is_charging_station" boolean notNull
        , DSI._information_rental_methods = field "rental_methods" (unboundedArray DSI.rentalMethod)
        , DSI._information_is_virtual_station = field "is_virtual_station" boolean notNull
        , DSI._information_groups = field "groups" (unboundedArray (varchar (Just 100)))
        , DSI._information_obcn = field "obcn" (varchar (Just 100)) notNull
        , DSI._information_nearby_distance = field "nearby_distance" double notNull
        , DSI._information_bluetooth_id = field "bluetooth_id" (varchar (Just 100)) notNull
        , DSI._information_ride_code_support = field "ride_code_support" boolean notNull
        })
  <*> (createTable "station_status" $ DSS.StationStatus
        { DSS._status_id = field "id" PG.serial notNull unique
        , DSS._status_station_id = field "station_id" int notNull unique
        , DSS._status_num_bikes_available = field "num_bikes_available" int notNull
        , DSS._status_num_bikes_disabled = field "num_bikes_disabled" int notNull
        , DSS._status_num_docks_available = field "num_docks_available" int notNull
        , DSS._status_num_docks_disabled = field "num_docks_disabled" int notNull
        , DSS._status_last_reported = field "last_reported" (maybeType int)
        , DSS._status_is_charging_station = field "is_charging_station" boolean notNull
        , DSS._status_status = field "status" DSS.stationStatus
        , DSS._status_is_installed = field "is_installed" boolean notNull
        , DSS._status_is_renting = field "is_renting" boolean notNull
        , DSS._status_is_returning = field "is_returning" boolean notNull
        , DSS._status_traffic = field "traffic" (maybeType (varchar (Just 100)))
        , DSS._status_vehicle_docks_available = field "vehicle_docks_available" int notNull
        , DSS._status_vehicle_types_available = DSS.VehicleType (field "vehicle_types_available_boost"    int)
                                                                (field "vehicle_types_available_iconic"   int)
                                                                (field "vehicle_types_available_efit"     int)
                                                                (field "vehicle_types_available_efit_g5"  int)
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
migrateDB conn = runBeamPostgresDebug putStrLn conn $
  bringUpToDateWithHooks
    allowDestructive
    PG.migrationBackend
    initialSetupStep

bikeshareDB :: DatabaseSettings Postgres BikeshareDb
bikeshareDB = unCheckDatabase $ evaluateDatabase initialSetupStep

exampleQuery :: Database.Beam.Postgres.Connection -> IO [DSI.StationInformationT Identity]
exampleQuery conn = runBeamPostgres conn $
  runSelectReturningList $
    select (all_ (_bikeshareStationInformation bikeshareDB))

-- | Establish a connection to the database.
connectDb :: IO Connection
connectDb =
  connectPostgreSQL $ fromString "host=localhost port=5432 dbname=haskbike connect_timeout=10"
