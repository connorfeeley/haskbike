-- | This module contains the data types for the BikeShare station_information API.

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module StationInformation
        ( Station (..)
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
data Station where
  Station :: { station_id :: String
             , name :: String
             , physical_configuration :: PhysicalConfiguration
             , lat :: Double
             , lon :: Double
             , altitude :: Double
             , address :: String
             , capacity :: Int
             , is_charging_station :: Bool
             , rental_methods :: [RentalMethod]
             , is_virtual_station :: Bool
             , groups :: [String]
             , obcn :: String
             , nearby_distance :: Double
             , bluetooth_id :: String
             , ride_code_support :: Bool
             , rental_uris :: Object
             } -> Station
  deriving (Show, Eq, Generic)

instance ToJSON Station where
  toJSON station =
    object [ "station_id"               .= station_id station
           , "name"                     .= name station
           , "physical_configuration"   .= physical_configuration station
           , "lat"                      .= lat station
           , "lon"                      .= lon station
           , "altitude"                 .= altitude station
           , "address"                  .= address station
           , "capacity"                 .= capacity station
           , "is_charging_station"      .= is_charging_station station
           , "rental_methods"           .= rental_methods station
           , "is_virtual_station"       .= is_virtual_station station
           , "groups"                   .= groups station
           , "obcn"                     .= obcn station
           , "nearby_distance"          .= nearby_distance station
           , "_bluetooth_id"            .= bluetooth_id station
           , "_ride_code_support"       .= ride_code_support station
           , "rental_uris"              .= rental_uris station
           ]

instance FromJSON Station where
  parseJSON = withObject "Station" $ \v -> Station
    <$> v .: "station_id"
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
  StationInformationResponse :: {stations :: [Station]} -> StationInformationResponse
  deriving (Show, Generic)

instance FromJSON StationInformationResponse where
  parseJSON = withObject "StationInformationResponse" $ \v -> do
    dataObject <- v .: "data"
    StationInformationResponse <$> dataObject .: "stations"
