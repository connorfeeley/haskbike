-- | Database schema for BikeShare.

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Database.BikeShare
     ( BikeshareDb (..)
     , module Database.BikeShare.Types
     , bikeshareDb
       -- , bikeshareDiagnostics
     , bikeshareStationInformation
     , bikeshareStationStatus
     ) where



import           Database.Beam
import           Database.BikeShare.Types


-- | Define the database; only containing one table for now.
data BikeshareDb f where
  BikeshareDb :: { _bikeshareStationInformation :: f (TableEntity StationInformationT)
                 , _bikeshareStationStatus      :: f (TableEntity StationStatusT)
                 -- , _bikeshareDiagnostics        :: f (TableEntity DiagnosticsT)
                 } -> BikeshareDb f
  deriving (Generic, Database be)

-- | Description of the database.
bikeshareDb :: DatabaseSettings be BikeshareDb
bikeshareDb = defaultDbSettings `withDbModification`
  dbModification
  { _bikeshareStationInformation =
    setEntityName "station_information" <> modifyTableFields tableModification
      { _infoId                    = "id"
      , _infoStationId             = "station_id"
      , _infoName                  = "name"
      , _infoPhysicalConfiguration = "physical_configuration"
      , _infoLat                   = "lat"
      , _infoLon                   = "lon"
      , _infoAltitude              = "altitude"
      , _infoAddress               = "address"
      , _infoCapacity              = "capacity"
      , _infoIsChargingStation     = "is_charging_station"
      , _infoRentalMethods         = "rental_methods"
      , _infoIsValetStation        = "is_valet_station"
      , _infoIsVirtualStation      = "is_virtual_station"
      , _infoGroups                = "groups"
      , _infoObcn                  = "obcn"
      , _infoNearbyDistance        = "nearby_distance"
      , _infoBluetoothId           = "bluetooth_id"
      , _infoRideCodeSupport       = "ride_code_support"
      , _infoRentalUris            = "rental_uris"
      , _infoActive                = "active"
      }
  , _bikeshareStationStatus =
    setEntityName "station_status" <> modifyTableFields tableModification
      { _statusStationId             = StationInformationId "info_id"
      , _statusLastReported          = "last_reported"
      , _statusNumBikesAvailable     = "num_bikes_available"
      , _statusNumBikesDisabled      = "num_bikes_disabled"
      , _statusNumDocksAvailable     = "num_docks_available"
      , _statusNumDocksDisabled      = "num_docks_disabled"
      , _statusIsChargingStation     = "is_charging_station"
      , _statusStatus                = "status"
      , _statusIsInstalled           = "is_installed"
      , _statusIsRenting             = "is_renting"
      , _statusIsReturning           = "is_returning"
      , _statusTraffic               = "traffic"
      , _statusVehicleDocksAvailable = "vehicle_docks_available"
      , _statusVehicleTypesAvailable = vehicleTypeFields "vehicle_types_available"
      }
  -- , _bikeshareDiagnostics =
  --   setEntityName "diagnostics" <> modifyTableFields tableModification
  --     { _diagnosticId   = "id"
  --     , _diagnosticTime = "time"
  --     }
  }

-- Lenses
BikeshareDb
  (TableLens bikeshareStationInformation)
  (TableLens bikeshareStationStatus)
  -- (TableLens bikeshareDiagnostics)
  = dbLenses
