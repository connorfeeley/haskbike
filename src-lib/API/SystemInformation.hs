{-# LANGUAGE TemplateHaskell #-}

-- | GBFS 2.3 System Information

module API.SystemInformation where

import           API.ResponseWrapper

import           Control.Lens        hiding ( (.=) )

import           Data.Aeson          ( FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), object, withObject,
                                       (.:) )

import           GHC.Generics        ( Generic )

-- * Count of bikes by type.
data SystemInformationVehicleCount where
  SystemInformationVehicleCount :: { sysInfEbikeCount      :: Int
                                   , sysInfMechanicalCount :: Int
                                   } -> SystemInformationVehicleCount
  deriving (Eq, Generic, Show)

instance ToJSON SystemInformationVehicleCount where
  toJSON (SystemInformationVehicleCount ebikeCount mechanicalCount) = object
    [ "_ebike_count"      .= ebikeCount
    , "_mechanical_count" .= mechanicalCount
    ]

instance FromJSON SystemInformationVehicleCount where
  parseJSON = withObject "SystemInformationVehicleCount" $ \o -> do
    ebikeCount      <- o .: "_ebike_count"
    mechanicalCount <- o .: "_mechanical_count"
    return $ SystemInformationVehicleCount ebikeCount mechanicalCount

-- * Toronto Bike Share System Information API response.

data SystemInformation where
  SystemInformation :: { _sysInfStationCount         :: Int
                       , _sysInfVehicleCount         :: SystemInformationVehicleCount
                       , _sysInfBuildHash            :: String
                       , _sysInfBuildLabel           :: String
                       , _sysInfBuildNumber          :: String
                       , _sysInfBuildVersion         :: String
                       , _sysInfLanguage             :: String
                       , _sysInfMobileHeadVersion    :: Int
                       , _sysInfMobileMinSuppVersion :: Int
                       , _sysInfName                 :: String
                       , _sysInfSysId                :: String
                       , _sysInfTimeZone             :: String
                       } -> SystemInformation
  deriving (Eq, Generic, Show)

instance ToJSON SystemInformation where
  toJSON (SystemInformation stationCount vehicleCount buildHash buildLabel buildNumber buildVersion language mobileHeadVersion mobileMinSuppVersion name sysId timeZone) = object
    [ "_station_count"                   .= stationCount
    , "_vehicle_count"                   .= vehicleCount
    , "build_hash"                       .= buildHash
    , "build_label"                      .= buildLabel
    , "build_number"                     .= buildNumber
    , "build_version"                    .= buildVersion
    , "language"                         .= language
    , "mobile_head_version"              .= show mobileHeadVersion
    , "mobile_minimum_supported_version" .= show mobileMinSuppVersion
    , "name"                             .= name
    , "system_id"                        .= sysId
    , "timezone"                         .= timeZone
    ]

instance FromJSON SystemInformation where
  parseJSON = withObject "SystemInformation" $ \o -> do
    stationCount         <- o .: "_station_count"
    vehicleCount         <- o .: "_vehicle_count"
    buildHash            <- o .: "build_hash"
    buildLabel           <- o .: "build_label"
    buildNumber          <- o .: "build_number"
    buildVersion         <- o .: "build_version"
    language             <- o .: "language"
    mobileHeadVersion    <- read <$> (o .: "mobile_head_version")
    mobileMinSuppVersion <- read <$> (o .: "mobile_minimum_supported_version")
    name                 <- o .: "name"
    sysId                <- o .: "system_id"
    timeZone             <- o .: "timezone"
    return $ SystemInformation stationCount
                               vehicleCount
                               buildHash
                               buildLabel
                               buildNumber
                               buildVersion
                               language
                               mobileHeadVersion
                               mobileMinSuppVersion
                               name
                               sysId
                               timeZone

-- | Type synonym for the wrapped station information response.
type SystemInformationResponse = ResponseWrapper SystemInformation

-- | Lenses
makeLenses ''SystemInformation
