-- | This module contains the data types for the BikeShare station_information API.

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module StationInformation
        ( StationInformation (..)
        , StationInformationResponse (..)
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
  StationInformation :: { station_id                :: Int
                        , name                      :: String
                        , physical_configuration    :: PhysicalConfiguration
                        , lat                       :: Double
                        , lon                       :: Double
                        , altitude                  :: Maybe Double
                        , address                   :: String
                        , capacity                  :: Int
                        , is_charging_station       :: Bool
                        , rental_methods            :: [RentalMethod]
                        , is_virtual_station        :: Bool
                        , groups                    :: [String]
                        , obcn                      :: String
                        , nearby_distance           :: Double
                        , bluetooth_id              :: String
                        , ride_code_support         :: Bool
                        -- , rental_uris               :: RentalURIs
                        } -> StationInformation
  deriving (Show, Eq, Generic)

instance ToJSON StationInformation where
  toJSON station =
    object [ "station_id"               .= station_id               station
           , "name"                     .= name                     station
           , "physical_configuration"   .= physical_configuration   station
           , "lat"                      .= lat                      station
           , "lon"                      .= lon                      station
           , "altitude"                 .= altitude                 station
           , "address"                  .= address                  station
           , "capacity"                 .= capacity                 station
           , "is_charging_station"      .= is_charging_station      station
           , "rental_methods"           .= rental_methods           station
           , "is_virtual_station"       .= is_virtual_station       station
           , "groups"                   .= groups                   station
           , "obcn"                     .= obcn                     station
           , "nearby_distance"          .= nearby_distance          station
           , "_bluetooth_id"            .= bluetooth_id             station
           , "_ride_code_support"       .= ride_code_support        station
           -- , "rental_uris"              .= rental_uris              station
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
newtype StationInformationResponse where
  StationInformationResponse :: {stations :: [StationInformation]} -> StationInformationResponse
  deriving (Show, Generic)

instance FromJSON StationInformationResponse where
  parseJSON = withObject "StationInformationResponse" $ \v -> do
    dataObject <- v .: "data"
    StationInformationResponse <$> dataObject .: "stations"
