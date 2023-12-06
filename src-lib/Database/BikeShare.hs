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
     , bikeshareQueryLog
     , bikeshareStationInformation
     , bikeshareStationStatus
     , bikeshareSystemInformation
     , bikeshareSystemInformationCount
     ) where

import           Control.Lens             ( Lens' )

import           Database.Beam
import           Database.BikeShare.Types


data BikeshareDb f where
  BikeshareDb :: { _bikeshareStationInformation     :: f (TableEntity StationInformationT)
                 , _bikeshareStationStatus          :: f (TableEntity StationStatusT)
                 , _bikeshareSystemInformation      :: f (TableEntity SystemInformationT)
                 , _bikeshareSystemInformationCount :: f (TableEntity SystemInformationCountT)
                 , _bikeshareQueryLog               :: f (TableEntity QueryLogT)
                 -- , _bikeshareDiagnostics         :: f (TableEntity DiagnosticsT)
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
      { _statusStationId             = StationInformationId "station_id"
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
  , _bikeshareSystemInformation =
    setEntityName "system_information" <> modifyTableFields tableModification
      { _sysInfKey                   = sysInfKeyFields ""
      , _sysInfBuildHash             = "build_hash"
      , _sysInfBuildLabel            = "build_label"
      , _sysInfBuildNumber           = "build_number"
      , _sysInfBuildVersion          = "build_version"
      , _sysInfLanguage              = "language"
      , _sysInfMobileHeadVersion     = "mobile_head_version"
      , _sysInfMobileMinSuppVersion  = "mobile_minimum_supported_version"
      , _sysInfName                  = "name"
      , _sysInfSysId                 = "system_id"
      , _sysInfTimeZone              = "timezone"
      }
  , _bikeshareSystemInformationCount =
    setEntityName "system_information_count" <> modifyTableFields tableModification
      { _sysInfCntKey                = sysInfKeyFields ""
      , _sysInfCntStationCount       = "station_count"
      , _sysInfCntMechanicalCount    = "mechanical_count"
      , _sysInfCntEbikeCount         = "ebike_count"
      }
  , _bikeshareQueryLog =
    setEntityName "queries" <> modifyTableFields tableModification
      { _queryLogId       = "id"
      , _queryLogTime     = "time"
      , _queryLogEndpoint = "endpoint"
      , _queryLogSuccess  = "endpoint"
      , _queryLogErrMsg   = "endpoint"
      }
  -- , _bikeshareDiagnostics =
  --   setEntityName "diagnostics" <> modifyTableFields tableModification
  --     { _diagnosticId   = "id"
  --     , _diagnosticTime = "time"
  --     }
  }

-- Lenses
bikeshareStationInformation     :: Lens' (BikeshareDb f) (f (TableEntity StationInformationT))
bikeshareStationStatus          :: Lens' (BikeshareDb f) (f (TableEntity StationStatusT))
bikeshareSystemInformation      :: Lens' (BikeshareDb f) (f (TableEntity SystemInformationT))
bikeshareSystemInformationCount :: Lens' (BikeshareDb f) (f (TableEntity SystemInformationCountT))
bikeshareQueryLog               :: Lens' (BikeshareDb f) (f (TableEntity QueryLogT))
BikeshareDb (TableLens bikeshareStationInformation) _ _ _ _     = dbLenses
BikeshareDb _ (TableLens bikeshareStationStatus) _ _ _          = dbLenses
BikeshareDb _ _ (TableLens bikeshareSystemInformation) _ _      = dbLenses
BikeshareDb _ _ _ (TableLens bikeshareSystemInformationCount) _ = dbLenses
BikeshareDb _ _ _ _ (TableLens bikeshareQueryLog)               = dbLenses
