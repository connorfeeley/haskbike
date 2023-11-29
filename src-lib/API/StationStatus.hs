-- | This module contains the data types for the BikeShare station_status API.

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module API.StationStatus
     ( StationStatus (..)
     , StationStatusString (..)
     , TorontoVehicleType (..)
     , VehicleDock (..)
     , VehicleType (..)
     , statusIsChargingStation
     , statusIsInstalled
     , statusIsRenting
     , statusIsReturning
     , statusLastReported
     , statusNumBikesAvailable
     , statusNumBikesDisabled
     , statusNumDocksAvailable
     , statusNumDocksDisabled
     , statusStationId
     , statusStatus
     , statusTraffic
     , statusVehicleDocksAvailable
     , statusVehicleTypesAvailable
     ) where

import           API.Classes

import           Control.Lens         hiding ( (.=) )

import           Data.Aeson           ( FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), Value (String), object,
                                        withObject, withText, (.:), (.:?) )
import           Data.Attoparsec.Text ( Parser, choice, parseOnly, string )
import           Data.Char            ( toUpper )
import           Data.Either          ( fromRight )
import           Data.Functor         ( ($>) )
import qualified Data.Text            as Text
import           Data.Time
import           Data.Time.Extras

import           GHC.Generics         ( Generic )


-- | Enumeration representing a BikeShare station status string.
data StationStatusString where
  InService   :: StationStatusString
  Maintenance :: StationStatusString
  Planned     :: StationStatusString
  EndOfLife   :: StationStatusString
  deriving (Eq, Generic, Ord)

instance Show StationStatusString where
  show InService   = "IN_SERVICE"
  show Maintenance = "MAINTENANCE"
  show Planned     = "PLANNED"
  show EndOfLife   = "END_OF_LIFE"

instance Read StationStatusString where
  readsPrec _ = fromRight [] . parseOnly parser . Text.pack
    where
    parser :: Parser [(StationStatusString, String)]
    parser = choice
      [ string "IN_SERVICE"  $> [(InService, "")]
      , string "PLANNED"     $> [(Planned,   "")]
      , string "END_OF_LIFE" $> [(EndOfLife, "")]
      ]

instance ToJSON StationStatusString where
  toJSON InService   = String (Text.pack "IN_SERVICE")
  toJSON Maintenance = String (Text.pack "MAINTENANCE")
  toJSON Planned     = String (Text.pack "PLANNED")
  toJSON EndOfLife   = String (Text.pack "END_OF_LIFE")

instance FromJSON StationStatusString where
  parseJSON = withText "StationStatusString" $ \t -> case t of
     "IN_SERVICE"  -> return InService
     "MAINTENANCE" -> return Maintenance
     "PLANNED"     -> return Planned
     "END_OF_LIFE" -> return EndOfLife
     _             -> fail ("Invalid StationStatusString: " ++ show t)

-- | Type representing a BikeShare station's status.
data StationStatus where
  StationStatus :: { _statusStationId             :: Int
                   , _statusNumBikesAvailable     :: Int
                   , _statusNumBikesDisabled      :: Int
                   , _statusNumDocksAvailable     :: Int
                   , _statusNumDocksDisabled      :: Int
                   , _statusLastReported          :: Maybe UTCTime
                   , _statusIsChargingStation     :: Bool
                   , _statusStatus                :: StationStatusString
                   , _statusIsInstalled           :: Bool
                   , _statusIsRenting             :: Bool
                   , _statusIsReturning           :: Bool
                   , _statusTraffic               :: Maybe String -- PBSC doesn't seem to set this field
                   , _statusVehicleDocksAvailable :: [VehicleDock]
                   , _statusVehicleTypesAvailable :: [VehicleType]
                   } -> StationStatus
  deriving (Show, Generic)

instance ToJSON StationStatus where
  toJSON station =
    object [ "station_id"               .=  show (_statusStationId                    station)
           , "num_bikes_available"      .= _statusNumBikesAvailable                   station
           , "num_bikes_disabled"       .= _statusNumBikesDisabled                    station
           , "num_docks_available"      .= _statusNumDocksAvailable                   station
           , "num_docks_disabled"       .= _statusNumDocksDisabled                    station
           , "last_reported"            .=  fmap utcToPosix      (_statusLastReported station)
           , "is_charging_station"      .= _statusIsChargingStation                   station
           , "status"                   .= _statusStatus                              station
           , "is_installed"             .= _statusIsInstalled                         station
           , "is_renting"               .= _statusIsRenting                           station
           , "is_returning"             .= _statusIsReturning                         station
           , "traffic"                  .= _statusTraffic                             station
           , "vehicle_docks_available"  .= _statusVehicleDocksAvailable               station
           , "vehicle_types_available"  .= _statusVehicleTypesAvailable               station
           ]
instance FromJSON StationStatus where
  parseJSON = withObject "StationStatus" $ \v -> StationStatus
    <$> fmap read (v .: "station_id")
    <*> v .: "num_bikes_available"
    <*> v .: "num_bikes_disabled"
    <*> v .: "num_docks_available"
    <*> v .: "num_docks_disabled"
    <*> (fmap posixToUtc <$> (v .:? "last_reported"))
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
  deriving (Show, Generic, Eq, Ord)

instance ToJSON VehicleDock where
  toJSON docks_available =
    object [ "vehicle_type_ids" .= vehicle_type_ids docks_available
            , "count"           .= dock_count             docks_available
            ]
instance FromJSON VehicleDock where
  parseJSON = withObject "VehicleDock" $ \v -> VehicleDock
    <$> v .: "vehicle_type_ids"
    <*> v .: "count"

-- | A type representing a BikeShare station's vehicle type status.
data VehicleType where
  VehicleType :: { vehicle_type_id :: TorontoVehicleType
                 , type_count :: Int
                 } -> VehicleType
  deriving (Show, Generic, Eq, Ord)

instance ToJSON VehicleType where
  toJSON types_available =
    object [ "vehicle_type_id" .= show (vehicle_type_id types_available)
            , "count"          .= type_count            types_available
            ]
instance FromJSON VehicleType where
  parseJSON = withObject "VehicleType" $ \v -> VehicleType
    <$> v .: "vehicle_type_id"
    <*> v .: "count"

data TorontoVehicleType where
  Boost  :: TorontoVehicleType
  Iconic :: TorontoVehicleType
  EFit   :: TorontoVehicleType
  EFitG5 :: TorontoVehicleType
  deriving (Generic, Eq, Ord)

instance Show TorontoVehicleType where
  show Boost  = "BOOST"
  show Iconic = "ICONIC"
  show EFit   = "EFIT"
  show EFitG5 = "EFIT G5"

-- | Read instance for 'TorontoVehicleType' (case-insensitive).
instance Read TorontoVehicleType where
  readsPrec _ = fromRight [] . parseOnly parser . Text.pack . map toUpper
    where
    parser :: Parser [(TorontoVehicleType, String)]
    parser = choice
      [ string "BOOST"    $> [(Boost,  "")]
      , string "ICONIC"   $> [(Iconic, "")]
      , string "EFIT"     $> [(EFit,   "")]
      , string "EFIT G5"  $> [(EFitG5, "")]
      ]

instance ToJSON TorontoVehicleType where
  toJSON Boost  = String (Text.pack "BOOST")
  toJSON Iconic = String (Text.pack "ICONIC")
  toJSON EFit   = String (Text.pack "EFIT")
  toJSON EFitG5 = String (Text.pack "EFIT G5")

instance FromJSON TorontoVehicleType where
  parseJSON = withText "TorontoVehicleType" $ \t -> case t of
     "BOOST"   -> return Boost
     "ICONIC"  -> return Iconic
     "EFIT"    -> return EFit
     "EFIT G5" -> return EFitG5
     _         -> fail ("Invalid TorontoVehicleType: " ++ show t)


instance HasDataField [StationStatus] where
  -- For a list of SystemStatus, we expect to find them under the 'stations' key
  getDataField obj = obj .: "stations"


-- | Lenses
makeLenses ''StationStatus
