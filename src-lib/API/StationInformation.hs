-- | This module contains the data types for the BikeShare station_information API.

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module API.StationInformation
        ( StationInformation (..)
        , StationInformationResponse
        , StationInformationResponseData (..)
        , PhysicalConfiguration (..)
        , RentalMethod (..)
        , RentalURIs (..)
        ) where

import           Data.Aeson
import qualified Data.Text            as Text
import           GHC.Generics

import           Data.Attoparsec.Text (Parser, choice, parseOnly, string)
import           Data.Either          (fromRight)
import           Data.Functor         (($>))
import           Data.Text            (pack)
import API.ResponseWrapper


-- | Enumeration representing a BikeShare station physical configuration.
data PhysicalConfiguration where
  ElectricBikeStation   :: PhysicalConfiguration
  Regular               :: PhysicalConfiguration
  RegularLitMapFrame    :: PhysicalConfiguration
  SmartLitMapFrame      :: PhysicalConfiguration
  SmartMapFrame         :: PhysicalConfiguration
  Vault                 :: PhysicalConfiguration
  deriving (Eq, Generic)

instance Show PhysicalConfiguration where
  show ElectricBikeStation = "ELECTRICBIKESTATION"
  show Regular             = "REGULAR"
  show RegularLitMapFrame  = "REGULARLITMAPFRAME"
  show SmartLitMapFrame    = "SMARTLITMAPFRAME"
  show SmartMapFrame       = "SMARTMAPFRAME"
  show Vault               = "VAULT"

instance Read PhysicalConfiguration where
  readsPrec _ = fromRight [] . parseOnly parser . pack
    where
    parser :: Parser [(PhysicalConfiguration, String)]
    parser = choice
      [ string "ELECTRICBIKESTATION" $> [(ElectricBikeStation, "")]
      , string "REGULAR"             $> [(Regular,             "")]
      , string "REGULARLITMAPFRAME"  $> [(RegularLitMapFrame,  "")]
      , string "SMARTLITMAPFRAME"    $> [(SmartLitMapFrame,    "")]
      , string "SMARTMAPFRAME"       $> [(SmartMapFrame,       "")]
      , string "VAULT"               $> [(Vault,               "")]
      ]

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
  deriving (Eq, Generic)

instance Show RentalMethod where
  show Key         = "KEY"
  show TransitCard = "TRANSITCARD"
  show CreditCard  = "CREDITCARD"
  show Phone       = "PHONE"

instance Read RentalMethod where
  readsPrec _ = fromRight [] . parseOnly parser . pack
    where
    parser :: Parser [(RentalMethod, String)]
    parser = choice
      [ string "KEY"                $> [(Key,         "")]
      , string "TRANSITCARD"        $> [(TransitCard, "")]
      , string "CREDITCARD"         $> [(CreditCard,  "")]
      , string "PHONE"              $> [(Phone,       "")]
      ]

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

-- | A type representing a BikeShare rental_uris record.
data RentalURIs where
  RentalURIs :: { rental_uris_android :: String
                , rental_uris_ios     :: String
                , rental_uris_web     :: String
                } -> RentalURIs
  deriving (Show, Eq, Generic)

instance ToJSON RentalURIs where
  toJSON rentalURIs =
    object [ "android" .= rental_uris_android rentalURIs
           , "ios"     .= rental_uris_ios     rentalURIs
           , "web"     .= rental_uris_web     rentalURIs
           ]
instance FromJSON RentalURIs where
  parseJSON = withObject "RentalURIs" $ \v -> RentalURIs
    <$> v .: "android"
    <*> v .: "ios"
    <*> v .: "web"

-- | A type representing a BikeShare station.
data StationInformation where
  StationInformation :: { info_station_id                :: Int
                        , info_name                      :: String
                        , info_physical_configuration    :: PhysicalConfiguration
                        , info_lat                       :: Double
                        , info_lon                       :: Double
                        , info_altitude                  :: Maybe Double
                        , info_address                   :: String
                        , info_capacity                  :: Int
                        , info_is_charging_station       :: Bool
                        , info_rental_methods            :: [RentalMethod]
                        , info_is_virtual_station        :: Bool
                        , info_groups                    :: [String]
                        , info_obcn                      :: String
                        , info_nearby_distance           :: Double
                        , info_bluetooth_id              :: String
                        , info_ride_code_support         :: Bool
                        -- , info_rental_uris               :: RentalURIs
                        } -> StationInformation
  deriving (Show, Eq, Generic)

instance ToJSON StationInformation where
  toJSON station =
    object [ "station_id"               .= info_station_id               station
           , "name"                     .= info_name                     station
           , "physical_configuration"   .= info_physical_configuration   station
           , "lat"                      .= info_lat                      station
           , "lon"                      .= info_lon                      station
           , "altitude"                 .= info_altitude                 station
           , "address"                  .= info_address                  station
           , "capacity"                 .= info_capacity                 station
           , "is_charging_station"      .= info_is_charging_station      station
           , "rental_methods"           .= info_rental_methods           station
           , "is_virtual_station"       .= info_is_virtual_station       station
           , "groups"                   .= info_groups                   station
           , "obcn"                     .= info_obcn                     station
           , "nearby_distance"          .= info_nearby_distance          station
           , "_bluetooth_id"            .= info_bluetooth_id             station
           , "_ride_code_support"       .= info_ride_code_support        station
           -- , "rental_uris"              .= info_rental_uris              station
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
    -- <*> v .: "rental_uris"

-- | A wrapper type for the station information response.
newtype StationInformationResponseData where
  StationInformationResponseData :: {info_stations :: [StationInformation]} -> StationInformationResponseData
  deriving (Show, Generic)

instance FromJSON StationInformationResponseData where
  parseJSON = withObject "StationInformationResponseData" $ \v -> do
    StationInformationResponseData <$> v .: "stations"

-- | Type synonym for the wrapped station information response.
type StationInformationResponse = ResponseWrapper StationInformationResponseData
