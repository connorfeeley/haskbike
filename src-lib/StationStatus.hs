-- | This module contains the data types for the BikeShare station_status API.

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module StationStatus
        ( StationStatus (..)
        , StationStatusResponse (..)
        , StationStatusString (..)
        , VehicleDock (..)
        , VehicleType (..)
        ) where

import           Data.Aeson
import           Data.Attoparsec.Text (Parser, choice, parseOnly, string)
import           Data.Either          (fromRight)
import           Data.Functor         (($>))
import qualified Data.Text            as Text
import           GHC.Generics

-- | Enumeration representing a BikeShare station status string.
data StationStatusString where
  InService :: StationStatusString
  EndOfLife :: StationStatusString
  deriving (Eq, Generic)

instance Show StationStatusString where
  show InService = "IN_SERVICE"
  show EndOfLife = "END_OF_LIFE"

instance Read StationStatusString where
  readsPrec _ = fromRight [] . parseOnly parser . Text.pack
    where
    parser :: Parser [(StationStatusString, String)]
    parser = choice
      [ string "IN_SERVICE"  $> [(InService, "")]
      , string "END_OF_LIFE" $> [(EndOfLife, "")]
      ]

instance ToJSON StationStatusString where
  toJSON InService = String (Text.pack "IN_SERVICE")
  toJSON EndOfLife = String (Text.pack "END_OF_LIFE")

instance FromJSON StationStatusString where
  parseJSON = withText "StationStatusString" $ \t -> case t of
     "IN_SERVICE"  -> return InService
     "END_OF_LIFE" -> return EndOfLife
     _             -> fail ("Invalid StationStatusString: " ++ show t)

-- | Type representing a BikeShare station's status.
data StationStatus where
  StationStatus :: { station_id                  :: Int
                   , num_bikes_available         :: Int
                   , num_bikes_disabled          :: Int
                   , num_docks_available         :: Int
                   , num_docks_disabled          :: Int
                   , last_reported               :: Maybe Int
                   , is_charging_station         :: Bool
                   , status                      :: StationStatusString
                   , is_installed                :: Bool
                   , is_renting                  :: Bool
                   , is_returning                :: Bool
                   , traffic                     :: Maybe String -- PBSC doesn't seem to set this field
                   , vehicle_docks_available     :: [VehicleDock]
                   , vehicle_types_available     :: [VehicleType]
                   } -> StationStatus
  deriving (Show, Generic)

instance ToJSON StationStatus where
  toJSON station =
    object [ "station_id"               .= show (station_id              station)
           , "num_bikes_available"      .= num_bikes_available           station
           , "num_bikes_disabled"       .= num_bikes_disabled            station
           , "num_docks_available"      .= num_docks_available           station
           , "num_docks_disabled"       .= num_docks_disabled            station
           , "last_reported"            .= last_reported                 station
           , "is_charging_station"      .= is_charging_station           station
           , "status"                   .= status                        station
           , "is_installed"             .= is_installed                  station
           , "is_renting"               .= is_renting                    station
           , "is_returning"             .= is_returning                  station
           , "traffic"                  .= traffic                       station
           , "vehicle_docks_available"  .= vehicle_docks_available       station
           , "vehicle_types_available"  .= vehicle_types_available       station
           ]
instance FromJSON StationStatus where
  parseJSON = withObject "StationStatus" $ \v -> StationStatus
    <$> fmap read (v .: "station_id")
    <*> v .: "num_bikes_available"
    <*> v .: "num_bikes_disabled"
    <*> v .: "num_docks_available"
    <*> v .: "num_docks_disabled"
    <*> v .:? "last_reported"
    <*> v .: "is_charging_station"
    <*> v .: "status"
    <*> v .: "is_installed"
    <*> v .: "is_renting"
    <*> v .: "is_returning"
    <*> v .:? "traffic"
    <*> v .: "vehicle_docks_available"
    <*> v .: "vehicle_types_available"

-- | A type representing a BikeShare station's vehicle dock status.
data VehicleDock where
  VehicleDock :: { vehicle_type_ids :: [String]
                 , dock_count :: Int
                 } -> VehicleDock
  deriving (Show, Generic)

instance ToJSON VehicleDock where
  toJSON docks_available =
    object [ "vehicle_type_ids" .= show (vehicle_type_ids docks_available)
            , "count"           .= dock_count             docks_available
            ]
instance FromJSON VehicleDock where
  parseJSON = withObject "VehicleDock" $ \v -> VehicleDock
    <$> v .: "vehicle_type_ids"
    <*> v .: "count"

-- | A type representing a BikeShare station's vehicle type status.
data VehicleType where
  VehicleType :: { vehicle_type_id :: String
                 , type_count :: Int
                 } -> VehicleType
  deriving (Show, Generic)

instance ToJSON VehicleType where
  toJSON types_available =
    object [ "vehicle_type_id" .= show (vehicle_type_id types_available)
            , "count"          .= type_count            types_available
            ]
instance FromJSON VehicleType where
  parseJSON = withObject "VehicleType" $ \v -> VehicleType
    <$> v .: "vehicle_type_id"
    <*> v .: "count"

-- | A wrapper type for the station information response.
newtype StationStatusResponse where
  StationStatusResponse :: { stations :: [StationStatus] } -> StationStatusResponse
  deriving (Show, Generic)

instance FromJSON StationStatusResponse where
  parseJSON = withObject "StationStatusResponse" $ \v -> do
    dataObject <- v .: "data"
    StationStatusResponse <$> dataObject .: "stations"
