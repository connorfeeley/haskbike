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
     ( BikeshareDb (..)
     , module Database.Types
     , bikeshareDb
     , bikeshareStationInformation
     , bikeshareStationStatus
     ) where

import           Database.Beam
import           Database.Types


-- | Define the database; only containing one table for now.
data BikeshareDb f where
  BikeshareDb :: { _bikeshareStationInformation :: f (TableEntity StationInformationT)
                 , _bikeshareStationStatus      :: f (TableEntity StationStatusT)
                 } -> BikeshareDb f
  deriving (Generic, Database be)

-- | Description of the database.
bikeshareDb :: DatabaseSettings be BikeshareDb
bikeshareDb = defaultDbSettings `withDbModification`
  dbModification
  { _bikeshareStationInformation =
    setEntityName "station_information" <> modifyTableFields tableModification
      { _info_id                     = "id"
      , _info_station_id             = "station_id"
      , _info_name                   = "name"
      , _info_physical_configuration = "physical_configuration"
      , _info_lat                    = "lat"
      , _info_lon                    = "lon"
      , _info_altitude               = "altitude"
      , _info_address                = "address"
      , _info_capacity               = "capacity"
      , _info_is_charging_station    = "is_charging_station"
      , _info_rental_methods         = "rental_methods"
      , _info_is_virtual_station     = "is_virtual_station"
      , _info_groups                 = "groups"
      , _info_obcn                   = "obcn"
      , _info_nearby_distance        = "nearby_distance"
      , _info_bluetooth_id           = "bluetooth_id"
      , _info_ride_code_support      = "ride_code_support"
      }
  , _bikeshareStationStatus =
    setEntityName "station_status" <> modifyTableFields tableModification
      { _d_status_id                      = "id"
      , _d_status_info_id                 = StationInformationId "info_id"
      , _d_status_station_id              = "station_id"
      , _d_status_num_bikes_available     = "num_bikes_available"
      , _d_status_num_bikes_disabled      = "num_bikes_disabled"
      , _d_status_num_docks_available     = "num_docks_available"
      , _d_status_num_docks_disabled      = "num_docks_disabled"
      , _d_status_last_reported           = "last_reported"
      , _d_status_is_charging_station     = "is_charging_station"
      , _d_status_status                  = "status"
      , _d_status_is_installed            = "is_installed"
      , _d_status_is_renting              = "is_renting"
      , _d_status_is_returning            = "is_returning"
      , _d_status_traffic                 = "traffic"
      , _d_status_vehicle_docks_available = "vehicle_docks_available"
      , _d_status_vehicle_types_available = vehicleTypeFields "vehicle_types_available"
      , _d_status_active                  = "active"
      }
  }

-- Lenses
BikeshareDb
  (TableLens bikeshareStationInformation)
  (TableLens bikeshareStationStatus)
  = dbLenses
