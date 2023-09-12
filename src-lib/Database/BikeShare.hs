-- | Database schema for BikeShare.

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Database.BikeShare
        ( module DSI
        -- , module DSS
        , BikeshareDb(..)
        , bikeshareDb
        , bikeshareStationInformation
        , bikeshareStationStatus
        ) where

import           Database.Beam

import qualified Database.StationInformation as DSI
import qualified Database.StationStatus      as DSS
import Database.Beam.Migrate (field)

-- | Define the database; only containing one table for now.
data BikeshareDb f where
  BikeshareDb :: { _bikeshareStationInformation :: f (TableEntity DSI.StationInformationT)
                 , _bikeshareStationStatus      :: f (TableEntity DSS.StationStatusT)
                 } -> BikeshareDb f
  deriving (Generic, Database be)

-- | Description of the database.
bikeshareDb :: DatabaseSettings be BikeshareDb
bikeshareDb = defaultDbSettings `withDbModification`
  dbModification
  { _bikeshareStationInformation =
    setEntityName "station_information" <> modifyTableFields tableModification
      { DSI._info_id                     = "id"
      , DSI._info_station_id             = "station_id"
      , DSI._info_name                   = "name"
      , DSI._info_physical_configuration = "physical_configuration"
      , DSI._info_lat                    = "lat"
      , DSI._info_lon                    = "lon"
      , DSI._info_altitude               = "altitude"
      , DSI._info_address                = "address"
      , DSI._info_capacity               = "capacity"
      , DSI._info_is_charging_station    = "is_charging_station"
      , DSI._info_rental_methods         = "rental_methods"
      , DSI._info_is_virtual_station     = "is_virtual_station"
      , DSI._info_groups                 = "groups"
      , DSI._info_obcn                   = "obcn"
      , DSI._info_nearby_distance        = "nearby_distance"
      , DSI._info_bluetooth_id           = "bluetooth_id"
      , DSI._info_ride_code_support      = "ride_code_support"
      }
  , _bikeshareStationStatus =
    setEntityName "station_status" <> modifyTableFields tableModification
      { DSS._status_id                      = "id"
      , DSS._status_station_id              = DSI.StationInformationId "station_id"
      , DSS._status_num_bikes_available     = "num_bikes_available"
      , DSS._status_num_bikes_disabled      = "num_bikes_disabled"
      , DSS._status_num_docks_available     = "num_docks_available"
      , DSS._status_num_docks_disabled      = "num_docks_disabled"
      , DSS._status_last_reported           = "last_reported"
      , DSS._status_is_charging_station     = "is_charging_station"
      , DSS._status_status                  = "status"
      , DSS._status_is_installed            = "is_installed"
      , DSS._status_is_renting              = "is_renting"
      , DSS._status_is_returning            = "is_returning"
      , DSS._status_traffic                 = "traffic"
      , DSS._status_vehicle_docks_available = "vehicle_docks_available"
      , DSS._status_vehicle_types_available = DSS.vehicleTypeFields "vehicle_types_available"
      }
  }

-- Lenses
BikeshareDb
  (TableLens bikeshareStationInformation)
  (TableLens bikeshareStationStatus)
  = dbLenses
