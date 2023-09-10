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
      { DSI._information_id                     = "id"
      , DSI._information_station_id             = "station_id"
      , DSI._information_name                   = "name"
      , DSI._information_physical_configuration = "physical_configuration"
      , DSI._information_lat                    = "lat"
      , DSI._information_lon                    = "lon"
      , DSI._information_altitude               = "altitude"
      , DSI._information_address                = "address"
      , DSI._information_capacity               = "capacity"
      , DSI._information_is_charging_station    = "is_charging_station"
      , DSI._information_rental_methods         = "rental_methods"
      , DSI._information_is_virtual_station     = "is_virtual_station"
      , DSI._information_groups                 = "groups"
      , DSI._information_obcn                   = "obcn"
      , DSI._information_nearby_distance        = "nearby_distance"
      , DSI._information_bluetooth_id           = "bluetooth_id"
      , DSI._information_ride_code_support      = "ride_code_support"
      }
  , _bikeshareStationStatus =
    setEntityName "station_status" <> modifyTableFields tableModification
      { DSS._status_id                      = "id"
      , DSS._status_station_id              = "station_id"
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
