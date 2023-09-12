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
      { DSI._id                     = "id"
      , DSI._station_id             = "station_id"
      , DSI._name                   = "name"
      , DSI._physical_configuration = "physical_configuration"
      , DSI._lat                    = "lat"
      , DSI._lon                    = "lon"
      , DSI._altitude               = "altitude"
      , DSI._address                = "address"
      , DSI._capacity               = "capacity"
      , DSI._is_charging_station    = "is_charging_station"
      , DSI._rental_methods         = "rental_methods"
      , DSI._is_virtual_station     = "is_virtual_station"
      , DSI._groups                 = "groups"
      , DSI._obcn                   = "obcn"
      , DSI._nearby_distance        = "nearby_distance"
      , DSI._bluetooth_id           = "bluetooth_id"
      , DSI._ride_code_support      = "ride_code_support"
      }
  , _bikeshareStationStatus =
    setEntityName "station_status" <> modifyTableFields tableModification
      { DSS._id                      = "id"
      , DSS._station_id              = DSI.StationInformationId "station_id"
      , DSS._num_bikes_available     = "num_bikes_available"
      , DSS._num_bikes_disabled      = "num_bikes_disabled"
      , DSS._num_docks_available     = "num_docks_available"
      , DSS._num_docks_disabled      = "num_docks_disabled"
      , DSS._last_reported           = "last_reported"
      , DSS._is_charging_station     = "is_charging_station"
      , DSS._status                  = "status"
      , DSS._is_installed            = "is_installed"
      , DSS._is_renting              = "is_renting"
      , DSS._is_returning            = "is_returning"
      , DSS._traffic                 = "traffic"
      , DSS._vehicle_docks_available = "vehicle_docks_available"
      , DSS._vehicle_types_available = DSS.vehicleTypeFields "vehicle_types_available"
      }
  }

-- Lenses
BikeshareDb
  (TableLens bikeshareStationInformation)
  (TableLens bikeshareStationStatus)
  = dbLenses
