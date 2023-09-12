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

initialSetup :: Migration Postgres
  (CheckedDatabaseSettings Postgres BikeshareDb)
initialSetup = BikeshareDb
  <$> (createTable "station_information" $ DSI.StationInformation
        { DSI._id                     = field "id"                     PG.serial notNull unique
        , DSI._station_id             = field "station_id"             int notNull unique
        , DSI._name                   = field "name"                   (varchar (Just 100)) notNull
        , DSI._physical_configuration = field "physical_configuration" DSI.physicalConfiguration
        , DSI._lat                    = field "lat"                    double notNull
        , DSI._lon                    = field "lon"                    double notNull
        , DSI._altitude               = field "altitude"               double notNull
        , DSI._address                = field "address"                (varchar (Just 100)) notNull
        , DSI._capacity               = field "capacity"               int notNull
        , DSI._is_charging_station    = field "is_charging_station"    boolean notNull
        , DSI._rental_methods         = field "rental_methods"         (unboundedArray DSI.rentalMethod)
        , DSI._is_virtual_station     = field "is_virtual_station"     boolean notNull
        , DSI._groups                 = field "groups"                 (unboundedArray (varchar (Just 100)))
        , DSI._obcn                   = field "obcn"                   (varchar (Just 100)) notNull
        , DSI._nearby_distance        = field "nearby_distance"        double notNull
        , DSI._bluetooth_id           = field "bluetooth_id"           (varchar (Just 100)) notNull
        , DSI._ride_code_support      = field "ride_code_support"      boolean notNull
        })
  <*> (createTable "station_status" $ DSS.StationStatus
        { DSS._id                      = field "id"                      PG.serial notNull unique
        , DSS._station_id              = DSI.StationInformationId $ field "station_id" int notNull unique
        , DSS._num_bikes_available     = field "num_bikes_available"     int notNull
        , DSS._num_bikes_disabled      = field "num_bikes_disabled"      int notNull
        , DSS._num_docks_available     = field "num_docks_available"     int notNull
        , DSS._num_docks_disabled      = field "num_docks_disabled"      int notNull
        , DSS._last_reported           = field "last_reported"           (maybeType int)
        , DSS._is_charging_station     = field "is_charging_station"     boolean notNull
        , DSS._status                  = field "status"                  DSS.stationStatus
        , DSS._is_installed            = field "is_installed"            boolean notNull
        , DSS._is_renting              = field "is_renting"              boolean notNull
        , DSS._is_returning            = field "is_returning"            boolean notNull
        , DSS._traffic                 = field "traffic"                 (maybeType (varchar (Just 100)))
        , DSS._vehicle_docks_available = field "vehicle_docks_available" int notNull
        , DSS._vehicle_types_available = DSS.VehicleType (field "vehicle_types_available_boost"   int)
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
