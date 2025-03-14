-- | This module contains the data types for the BikeShare station_information Haskbike.API.

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Haskbike.API.StationInformation
     ( PhysicalConfiguration (..)
     , RentalMethod (..)
     , RentalURIs (..)
     , StationInformation (..)
     ) where

import           Control.Lens         hiding ( (.=) )

import           Data.Aeson
import           Data.Attoparsec.Text ( Parser, choice, parseOnly, string )
import           Data.Either          ( fromRight )
import           Data.Functor         ( ($>) )
import           Data.Maybe           ( fromMaybe )
import           Data.Text            ( pack )
import qualified Data.Text            as T
import qualified Data.Text            as Text

import           GHC.Generics

import           Haskbike.API.Classes


-- | Enumeration representing a BikeShare station physical configuration.
data PhysicalConfiguration where
  ElectricBikeStation   :: PhysicalConfiguration
  Regular               :: PhysicalConfiguration
  RegularLitMapFrame    :: PhysicalConfiguration
  SmartLitMapFrame      :: PhysicalConfiguration
  SmartMapFrame         :: PhysicalConfiguration
  Vault                 :: PhysicalConfiguration
  deriving (Generic, Eq, Ord)

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
  RentalURIs :: { rentalUrisAndroid :: T.Text
                , rentalUrisIos     :: T.Text
                , rentalUrisWeb     :: T.Text
                } -> RentalURIs
  deriving (Show, Eq, Generic)

instance ToJSON RentalURIs where
  toJSON rentalURIs =
    object [ "android" .= rentalUrisAndroid rentalURIs
           , "ios"     .= rentalUrisIos     rentalURIs
           , "web"     .= rentalUrisWeb     rentalURIs
           ]

instance FromJSON RentalURIs where
  parseJSON = withObject "RentalURIs" $ \v -> RentalURIs
    <$> fmap (fromMaybe "") (v .:? "android")
    <*> fmap (fromMaybe "") (v .:? "ios")
    <*> fmap (fromMaybe "") (v .:? "web")

-- | A type representing a BikeShare station.
data StationInformation where
  StationInformation :: { infoStationId               :: Int
                        , infoName                    :: T.Text
                        , infoPhysicalConfiguration   :: Maybe PhysicalConfiguration
                        , infoLat                     :: Double
                        , infoLon                     :: Double
                        , infoAltitude                :: Maybe Double
                        , infoAddress                 :: Maybe T.Text
                        , infoCapacity                :: Int
                        , infoIsChargingStation       :: Bool
                        , infoRentalMethods           :: Maybe [RentalMethod]
                        , infoIsValetStation          :: Bool
                        , infoIsVirtualStation        :: Bool
                        , infoGroups                  :: [T.Text]
                        , infoObcn                    :: T.Text
                        , infoNearbyDistance          :: Double
                        , infoBluetoothId             :: T.Text
                        , infoRideCodeSupport         :: Bool
                        , infoRentalUris              :: RentalURIs
                        } -> StationInformation
  deriving (Show, Eq, Generic)

instance ToJSON StationInformation where
  toJSON station =
    object [ "station_id"               .= show (infoStationId          station)
           , "name"                     .= infoName                     station
           , "physical_configuration"   .= infoPhysicalConfiguration    station
           , "lat"                      .= infoLat                      station
           , "lon"                      .= infoLon                      station
           , "altitude"                 .= infoAltitude                 station
           , "address"                  .= infoAddress                  station
           , "capacity"                 .= infoCapacity                 station
           , "is_charging_station"      .= infoIsChargingStation        station
           , "rental_methods"           .= infoRentalMethods            station
           , "is_valet_station"         .= infoIsValetStation           station
           , "is_virtual_station"       .= infoIsVirtualStation         station
           , "groups"                   .= infoGroups                   station
           , "obcn"                     .= infoObcn                     station
           , "nearby_distance"          .= infoNearbyDistance           station
           , "_bluetooth_id"            .= infoBluetoothId              station
           , "_ride_code_support"       .= infoRideCodeSupport          station
           , "rental_uris"              .= infoRentalUris               station
           ]

instance FromJSON StationInformation where
  parseJSON = withObject "StationInformation" $ \v -> StationInformation
    <$> fmap read (v .: "station_id")
    <*> v .:  "name"
    <*> v .:  "physical_configuration"
    <*> v .:  "lat"
    <*> v .:  "lon"
    <*> v .:  "altitude"
    <*> v .:? "address"
    <*> v .:  "capacity"
    <*> v .:  "is_charging_station"
    <*> v .:? "rental_methods"
    <*> (fromMaybe False <$> (v .:? "is_valet_station"))
    <*> v .:  "is_virtual_station"
    <*> v .:  "groups"
    <*> v .:  "obcn"
    <*> v .:  "nearby_distance"
    <*> v .:  "_bluetooth_id"
    <*> v .:  "_ride_code_support"
    <*> v .:  "rental_uris"

instance HasDataField [StationInformation] where
  -- For a list of StationInformation, we expect to find them under the 'stations' key
  dataFieldKey = "stations"

-- | Lenses
makeLenses ''StationInformation
