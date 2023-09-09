{-# LANGUAGE OverloadedStrings #-}

module Database.Migrations where

import           Database.Beam                    (DataType)
import           Database.Beam.Backend
import           Database.Beam.Migrate
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Postgres

import           Database.BikeShare
import qualified Database.StationInformation               as DSI

import Database.Beam.Query.DataTypes
import Data.Int (Int64)
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import Database.SQLite.Simple hiding (field)


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
            (nationalVarchar (Just 100)) notNull
        , DSI._information_lat = field "lat"
            double notNull
        , DSI._information_lon = field "lon"
            double notNull
        , DSI._information_altitude = field "altitude"
            double notNull
        , DSI._information_address = field "address"
            (nationalVarchar (Just 100)) notNull
        , DSI._information_capacity = field "capacity"
            int notNull
        , DSI._information_is_charging_station = field "is_charging_station"
            boolean notNull
        , DSI._information_rental_methods = field "rental_methods"
            (nationalVarchar (Just 100)) notNull
        , DSI._information_is_virtual_station = field "is_virtual_station"
            boolean notNull
        , DSI._information_groups = field "groups"
            (nationalVarchar (Just 100)) notNull
        , DSI._information_obcn = field "obcn"
            (nationalVarchar (Just 100)) notNull
        ,  DSI._information_nearby_distance = field "nearby_distance"
            double notNull
        , DSI._information_bluetooth_id = field "bluetooth_id"
            (nationalVarchar (Just 100)) notNull
        ,  DSI._information_ride_code_support = field "ride_code_support"
            boolean notNull
        })

initialSetupStep :: MigrationSteps Postgres
  ()
  (CheckedDatabaseSettings Postgres BikeshareDb)
initialSetupStep = migrationStep
  "initial_setup"
  (const initialSetup)
