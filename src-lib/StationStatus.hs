-- | This module contains the data types for the BikeShare station_status API.

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module StationStatus
        ( StationStatus (..)
        , StationStatusResponse (..)
        , StationStatusString (..)
        ) where

import           Data.Aeson
import qualified Data.Text    as Text
import           GHC.Generics

-- | Enumeration representing a BikeShare station status string.
data StationStatusString where
  InService :: StationStatusString
  EndOfLife :: StationStatusString
  deriving (Show, Eq, Generic)

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
  StationStatus :: { status_station_id                  :: String
                   , status_num_bikes_available         :: Int
                   , status_num_bikes_disabled          :: Int
                   , status_num_docks_available         :: Int
                   , status_num_docks_disabled          :: Int
                   , status_last_reported               :: Maybe Int
                   , status_is_charging_station         :: Bool
                   , status_status                      :: StationStatusString
                   , status_is_installed                :: Bool
                   , status_is_renting                  :: Bool
                   , status_is_returning                :: Bool
                   , status_traffic                     :: Maybe String
                   , status_vehicle_docks_available     :: [VehicleDock]
                   , status_vehicle_types_available     :: [VehicleType]
                   } -> StationStatus
  deriving (Show, Generic)

-- drop the "status_" prefix
instance ToJSON StationStatus where
  toJSON        = genericToJSON defaultOptions          { fieldLabelModifier = drop 7 }
instance FromJSON StationStatus where
  parseJSON     = genericParseJSON defaultOptions       { fieldLabelModifier = drop 7 }

-- | A type representing a BikeShare station's vehicle dock status.
data VehicleDock where
  VehicleDock :: { dock_vehicle_type_ids :: [String]
                 , dock_count :: Int
                 } -> VehicleDock
  deriving (Show, Generic)

-- drop the "dock_" prefix
instance ToJSON VehicleDock where
  toJSON        = genericToJSON defaultOptions          { fieldLabelModifier = drop 5 }
instance FromJSON VehicleDock where
  parseJSON     = genericParseJSON defaultOptions       { fieldLabelModifier = drop 5 }

-- | A type representing a BikeShare station's vehicle type status.
data VehicleType where
  VehicleType :: { type_vehicle_type_id :: String
                 , type_count :: Int
                 } -> VehicleType
  deriving (Show, Generic)

-- drop the "type_" prefix
instance ToJSON VehicleType where
  toJSON        = genericToJSON defaultOptions          { fieldLabelModifier = drop 5 }
instance FromJSON VehicleType where
  parseJSON     = genericParseJSON defaultOptions       { fieldLabelModifier = drop 5 }

-- | A wrapper type for the station information response.
newtype StationStatusResponse where
  StationStatusResponse :: { stations :: [StationStatus] } -> StationStatusResponse
  deriving (Show, Generic)

instance FromJSON StationStatusResponse where
  parseJSON = withObject "StationStatusResponse" $ \v -> do
    dataObject <- v .: "data"
    StationStatusResponse <$> dataObject .: "stations"
