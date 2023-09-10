{-# LANGUAGE OverloadedStrings #-}

module Database.Migrations where

import           Database.BikeShare
import qualified Database.StationInformation    as DSI

import qualified StationInformation             as SI

import           Data.String                    (fromString)

import           Database.Beam
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG


-- It's unfortunate that we have to define this ourselves.
-- utctime :: BeamSqlBackend be => DataType be UTCTime
-- utctime = DataType (timestampType Nothing True)

initialSetup :: Migration Postgres
  (CheckedDatabaseSettings Postgres BikeshareDb)
initialSetup = BikeshareDb
  <$> (createTable "station_information" $ DSI.StationInformation
        { DSI._information_id = field "id"
            int notNull unique
        , DSI._information_station_id = field "station_id"
            int notNull unique
        , DSI._information_name = field "name"
            (varchar (Just 100)) notNull
        , DSI._information_physical_configuration = field "physical_configuration"
            (varchar (Just 100)) notNull
        , DSI._information_lat = field "lat"
            double notNull
        , DSI._information_lon = field "lon"
            double notNull
        , DSI._information_altitude = field "altitude"
            double notNull
        , DSI._information_address = field "address"
            (varchar (Just 100)) notNull
        , DSI._information_capacity = field "capacity"
            int notNull
        , DSI._information_is_charging_station = field "is_charging_station"
            boolean notNull
        , DSI._information_rental_methods = field "rental_methods"
            SI.rentalMethod
        , DSI._information_is_virtual_station = field "is_virtual_station"
            boolean notNull
        , DSI._information_groups = field "groups"
            (unboundedArray (varchar (Just 100)))
        , DSI._information_obcn = field "obcn"
            (varchar (Just 100)) notNull
        ,  DSI._information_nearby_distance = field "nearby_distance"
            double notNull
        , DSI._information_bluetooth_id = field "bluetooth_id"
            (varchar (Just 100)) notNull
        ,  DSI._information_ride_code_support = field "ride_code_support"
            boolean notNull
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
