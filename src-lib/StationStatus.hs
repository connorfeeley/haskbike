-- | This module contains the data types for the BikeShare station_status API.

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module StationStatus
        ( StationStatus (..)
        , StationStatusResponse (..)
        ) where

import           Data.Aeson
import qualified Data.Text    as Text
import           GHC.Generics


-- | A simple type to represent a BikeShare station's status.
data StationStatus = StationStatus
    { station_id                      :: String
    , station_num_bikes_available     :: Int
    , station_num_bikes_disabled      :: Int
    , station_num_docks_available     :: Int
    , station_num_docks_disabled      :: Int
    , station_last_reported           :: Int
    , station_is_charging_station     :: Bool
    , station_status                  :: String
    , station_is_installed            :: Bool
    , station_is_renting              :: Bool
    , station_is_returning            :: Bool
    , station_traffic                 :: Maybe String
    , station_vehicle_docks_available :: [VehicleDock]
    , station_vehicle_types_available :: [VehicleType]
    } deriving (Show, Generic)

instance ToJSON StationStatus where
    toJSON = genericToJSON defaultOptions {
              fieldLabelModifier = drop 8 }
instance FromJSON StationStatus where
    parseJSON = genericParseJSON defaultOptions
      { fieldLabelModifier = drop 8 } -- drop the "station_" prefix

-- | A simple type to represent a BikeShare station's vehicle dock status.
data VehicleDock = VehicleDock
    { dock_vehicle_type_ids :: [String]
    , dock_count            :: Int
    } deriving (Show, Generic)

instance ToJSON VehicleDock where
    toJSON = genericToJSON defaultOptions {
              fieldLabelModifier = drop 5 }
instance FromJSON VehicleDock where
    parseJSON = genericParseJSON defaultOptions
      { fieldLabelModifier = drop 5 } -- drop the "dock_" prefix

-- | A simple type to represent a BikeShare station's vehicle type status.
data VehicleType = VehicleType
    { type_vehicle_type_id :: String
    , type_count           :: Int
    } deriving (Show, Generic)

instance ToJSON VehicleType where
    toJSON = genericToJSON defaultOptions
      { fieldLabelModifier = drop 5 } -- drop the "type_" prefix
instance FromJSON VehicleType where
    parseJSON = genericParseJSON defaultOptions
      { fieldLabelModifier = drop 5 } -- drop the "type_" prefix

-- | A wrapper type for the station information response.
newtype StationStatusResponse where
  StationStatusResponse :: {stations :: [StationStatus]} -> StationStatusResponse
  deriving (Show, Generic)

instance FromJSON StationStatusResponse where
  parseJSON = withObject "StationStatusResponse" $ \v -> do
    dataObject <- v .: "data"
    StationStatusResponse <$> dataObject .: "stations"
