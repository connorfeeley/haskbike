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
        -- , bikeshareStationStatus
        ) where

import           Database.Beam

import qualified Database.StationInformation as DSI

-- | Define the database; only containing one table for now.
data BikeshareDb f where
  BikeshareDb :: { _bikeshareStationInformation :: f (TableEntity DSI.StationInformationT)
                 -- , _bikeshareStationStatus      :: f (TableEntity DSS.StationStatusT)
                 } -> BikeshareDb f
  deriving (Generic, Database be)

-- | Description of the database.
bikeshareDb :: DatabaseSettings be BikeshareDb
bikeshareDb = defaultDbSettings `withDbModification`
  dbModification {
  _bikeshareStationInformation =
    setEntityName "station_information" <> modifyTableFields tableModification
      { DSI._information_id = "id"
      , DSI._information_station_id = "station_id"
      , DSI._information_name = "name"
      , DSI._information_physical_configuration = "physical_configuration"
      , DSI._information_lat = "lat"
      , DSI._information_lon = "lon"
      , DSI._information_altitude = "altitude"
      , DSI._information_address = "address"
      , DSI._information_capacity = "capacity"
      , DSI._information_is_charging_station = "is_charging_station"
      , DSI._information_rental_methods = "rental_methods"
      , DSI._information_is_virtual_station = "is_virtual_station"
      , DSI._information_groups = "groups"
      , DSI._information_obcn = "obcn"
      , DSI._information_nearby_distance = "nearby_distance"
      , DSI._information_bluetooth_id = "bluetooth_id"
      , DSI._information_ride_code_support = "ride_code_support"
      }
  }

-- Lenses
BikeshareDb
  (TableLens bikeshareStationInformation)
  -- (TableLens bikeshareStationStatus)
  = dbLenses
