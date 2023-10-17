-- |

module API.Server.Types.StationStatusVisualization
     ( StationStatusVisualization (..)
     ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString            ( ByteString )
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time

import           GHC.Generics



-- | Type representing a the visualization data for a BikeShare station's status.
data StationStatusVisualization where
  StationStatusVisualization :: { _statusVisStationId       :: Int
                                , _statusVisLastReported    :: LocalTime -- In UTC time
                                , _statusVisChargingStation :: Bool
                                , _statusVisBikesAvailable  :: Int
                                , _statusVisBikesDisabled   :: Int
                                , _statusVisDocksAvailable  :: Int
                                , _statusVisDocksDisabled   :: Int
                                , _statusVisAvailableIconic :: Int
                                , _statusVisAvailableEfit   :: Int
                                , _statusVisAvailableEfitG5 :: Int
                                } -> StationStatusVisualization
  deriving (Show, Generic, Eq, Ord)

instance ToJSON StationStatusVisualization where
  toJSON station =
    object [ "station_id"        .= _statusVisStationId        station
           , "last_reported"     .= _statusVisLastReported     station
           , "charging_station"  .= _statusVisChargingStation  station
           , "bikes_available"   .= _statusVisBikesAvailable   station
           , "bikes_disabled"    .= _statusVisBikesDisabled    station
           , "docks_available"   .= _statusVisDocksAvailable   station
           , "docks_disabled"    .= _statusVisDocksDisabled    station
           , "available_iconic"  .= _statusVisAvailableIconic  station
           , "available_efit"    .= _statusVisAvailableEfit    station
           , "available_efit_g5" .= _statusVisAvailableEfitG5  station
           ]
instance FromJSON StationStatusVisualization where
  parseJSON = withObject "StationStatus" $ \v -> StationStatusVisualization
    <$> v .: "station_id"
    <*> v .: "last_reported"
    <*> v .: "charging_station"
    <*> v .: "bikes_available"
    <*> v .: "bikes_disabled"
    <*> v .: "docks_available"
    <*> v .: "docks_disabled"
    <*> v .: "available_iconic"
    <*> v .: "available_efit"
    <*> v .: "available_efit_g5"
