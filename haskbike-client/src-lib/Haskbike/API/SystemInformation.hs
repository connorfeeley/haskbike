{-# LANGUAGE TemplateHaskell #-}

-- | GBFS 2.3 System Information

module Haskbike.API.SystemInformation where

import           Control.Lens         hiding ( (.=) )

import           Data.Aeson           ( FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), Value (Object), object,
                                        withObject, (.:) )
import qualified Data.Text            as T

import           GHC.Generics         ( Generic )

import           Haskbike.API.Classes

-- * Count of bikes by type.
data SystemInformationVehicleCount where
  SystemInformationVehicleCount :: { sysInfMechanicalCount :: Int
                                   , sysInfEbikeCount      :: Int
                                   } -> SystemInformationVehicleCount
  deriving (Eq, Generic, Show)

instance ToJSON SystemInformationVehicleCount where
  toJSON (SystemInformationVehicleCount mechanicalCount ebikeCount) = object
    [ "_mechanical_count" .= mechanicalCount
    , "_ebike_count"      .= ebikeCount
    ]

instance FromJSON SystemInformationVehicleCount where
  parseJSON = withObject "SystemInformationVehicleCount" $ \o -> do
    mechanicalCount <- o .: "_mechanical_count"
    ebikeCount      <- o .: "_ebike_count"
    return $ SystemInformationVehicleCount mechanicalCount ebikeCount

-- * Toronto Bike Share System Information API response.

data SystemInformation where
  SystemInformation :: { _sysInfStationCount         :: Int
                       , _sysInfVehicleCount         :: SystemInformationVehicleCount
                       , _sysInfBuildHash            :: T.Text
                       , _sysInfBuildLabel           :: T.Text
                       , _sysInfBuildNumber          :: T.Text
                       , _sysInfBuildVersion         :: T.Text
                       , _sysInfLanguage             :: T.Text
                       , _sysInfMobileHeadVersion    :: Int
                       , _sysInfMobileMinSuppVersion :: Int
                       , _sysInfName                 :: T.Text
                       , _sysInfSysId                :: T.Text
                       , _sysInfTimeZone             :: T.Text
                       } -> SystemInformation
  deriving (Eq, Generic, Show)

instance ToJSON SystemInformation where
  toJSON inf = object
    [ "_station_count"                   .= _sysInfStationCount inf
    , "_vehicle_count"                   .= _sysInfVehicleCount inf
    , "build_hash"                       .= _sysInfBuildHash inf
    , "build_label"                      .= _sysInfBuildLabel inf
    , "build_number"                     .= _sysInfBuildNumber inf
    , "build_version"                    .= _sysInfBuildVersion inf
    , "language"                         .= _sysInfLanguage inf
    , "mobile_head_version"              .= (show . _sysInfMobileHeadVersion) inf
    , "mobile_minimum_supported_version" .= (show . _sysInfMobileMinSuppVersion) inf
    , "name"                             .= _sysInfName inf
    , "system_id"                        .= _sysInfSysId inf
    , "timezone"                         .= _sysInfTimeZone inf
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

instance HasDataField SystemInformation where
  -- For SystemInformation, since it's directly under 'data', we pass the parser through
  dataFieldKey = "" -- FIXME: possibly unsafe
  getDataField obj = parseJSON (Object obj)
  putDataField = toJSON

-- | Lenses
makeLenses ''SystemInformation
