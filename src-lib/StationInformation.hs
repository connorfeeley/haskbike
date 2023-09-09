-- | This module contains the data types for the BikeShare station_information API.

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module StationInformation
        ( StationInformation (..)
        , StationInformationResponse (..)
        , PhysicalConfiguration (..)
        , RentalMethod (..)
        ) where

import           Data.Aeson
import qualified Data.Text    as Text
import           GHC.Generics


-- | Enumeration representing a BikeShare station physical configuration.
data PhysicalConfiguration where
  ElectricBikeStation   :: PhysicalConfiguration
  Regular               :: PhysicalConfiguration
  RegularLitMapFrame    :: PhysicalConfiguration
  SmartLitMapFrame      :: PhysicalConfiguration
  SmartMapFrame         :: PhysicalConfiguration
  Vault                 :: PhysicalConfiguration
  deriving (Show, Eq, Generic)

instance ToJSON PhysicalConfiguration where
  toJSON ElectricBikeStation = String (Text.pack "ELECTRICBIKESTATION")
  toJSON Regular             = String (Text.pack "REGULAR")
  toJSON RegularLitMapFrame  = String (Text.pack "REGULARLITMAPFRAME")
  toJSON SmartLitMapFrame    = String (Text.pack "SMARTLITMAPFRAME")
  toJSON SmartMapFrame       = String (Text.pack "SMARTMAPFRAME")
  toJSON Vault               = String (Text.pack "VAULT")

instance FromJSON PhysicalConfiguration where
  parseJSON = withText "PhysicalConfiguration" $ \t -> case t of
     "ELECTRICBIKESTATION" -> return ElectricBikeStation
     "REGULAR"             -> return Regular
     "REGULARLITMAPFRAME"  -> return RegularLitMapFrame
     "SMARTLITMAPFRAME"    -> return SmartLitMapFrame
     "SMARTMAPFRAME"       -> return SmartMapFrame
     "VAULT"               -> return Vault
     _                     -> fail ("Invalid PhysicalConfiguration: " ++ show t)

-- | Enumeration representing a BikeShare rental method.
data RentalMethod where
  Key           :: RentalMethod
  TransitCard   :: RentalMethod
  CreditCard    :: RentalMethod
  Phone         :: RentalMethod
  deriving (Show, Eq, Generic)

instance ToJSON RentalMethod where
  toJSON Key         = String "KEY"
  toJSON TransitCard = String "TRANSITCARD"
  toJSON CreditCard  = String "CREDITCARD"
  toJSON Phone       = String "PHONE"

instance FromJSON RentalMethod where
  parseJSON = withText "RentalMethod" $ \t ->
    case Text.toUpper t of
      "KEY"         -> return Key
      "TRANSITCARD" -> return TransitCard
      "CREDITCARD"  -> return CreditCard
      "PHONE"       -> return Phone
      _             -> fail $ "Invalid RentalMethod: " ++ Text.unpack t

-- | A type representing a BikeShare station.
data StationInformation where
  StationInformation :: { information_station_id                :: Int
                        , information_name                      :: String
                        , information_physical_configuration    :: PhysicalConfiguration
                        , information_lat                       :: Double
                        , information_lon                       :: Double
                        , information_altitude                  :: Double
                        , information_address                   :: String
                        , information_capacity                  :: Int
                        , information_is_charging_station       :: Bool
                        , information_rental_methods            :: [RentalMethod]
                        , information_is_virtual_station        :: Bool
                        , information_groups                    :: [String]
                        , information_obcn                      :: String
                        , information_nearby_distance           :: Double
                        , information_bluetooth_id              :: String
                        , information_ride_code_support         :: Bool
                        , information_rental_uris               :: Object
                        } -> StationInformation
  deriving (Show, Eq, Generic)

instance ToJSON StationInformation where
  toJSON station =
    object [ "station_id"               .= information_station_id               station
           , "name"                     .= information_name                     station
           , "physical_configuration"   .= information_physical_configuration   station
           , "lat"                      .= information_lat                      station
           , "lon"                      .= information_lon                      station
           , "altitude"                 .= information_altitude                 station
           , "address"                  .= information_address                  station
           , "capacity"                 .= information_capacity                 station
           , "is_charging_station"      .= information_is_charging_station      station
           , "rental_methods"           .= information_rental_methods           station
           , "is_virtual_station"       .= information_is_virtual_station       station
           , "groups"                   .= information_groups                   station
           , "obcn"                     .= information_obcn                     station
           , "nearby_distance"          .= information_nearby_distance          station
           , "_bluetooth_id"            .= information_bluetooth_id             station
           , "_ride_code_support"       .= information_ride_code_support        station
           , "rental_uris"              .= information_rental_uris              station
           ]

instance FromJSON StationInformation where
  parseJSON = withObject "StationInformation" $ \v -> StationInformation
    <$> fmap read (v .: "station_id")
    <*> v .: "name"
    <*> v .: "physical_configuration"
    <*> v .: "lat"
    <*> v .: "lon"
    <*> v .: "altitude"
    <*> v .: "address"
    <*> v .: "capacity"
    <*> v .: "is_charging_station"
    <*> v .: "rental_methods"
    <*> v .: "is_virtual_station"
    <*> v .: "groups"
    <*> v .: "obcn"
    <*> v .: "nearby_distance"
    <*> v .: "_bluetooth_id"
    <*> v .: "_ride_code_support"
    <*> v .: "rental_uris"

-- | A wrapper type for the station information response.
newtype StationInformationResponse where
  StationInformationResponse :: {stations :: [StationInformation]} -> StationInformationResponse
  deriving (Show, Generic)

instance FromJSON StationInformationResponse where
  parseJSON = withObject "StationInformationResponse" $ \v -> do
    dataObject <- v .: "data"
    StationInformationResponse <$> dataObject .: "stations"
