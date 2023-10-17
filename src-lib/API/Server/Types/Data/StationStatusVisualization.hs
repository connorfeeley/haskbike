-- FIXME: remove once server is fleshed out a bit more.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


-- | This module contains types used to serialize station status data for use in the visualization API.

module API.Server.Types.Data.StationStatusVisualization
     ( StationStatusVisualization (..)
     , fromBeamStationStatusToVisJSON
     , generateJsonDataSource
     ) where

import           AppEnv

import           Control.Lens                     hiding ( (.=) )
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString                  ( ByteString )
import           Data.Coerce                      ( coerce )
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time

import           Database.BikeShare.Operations
import           Database.BikeShare.StationStatus

import           GHC.Generics

import           ReportTime


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

-- | Convert from the Beam StationStatus type to StationStatusVisualization
fromBeamStationStatusToVisJSON :: NominalDiffTime -> StationStatus -> StationStatusVisualization
fromBeamStationStatusToVisJSON timeOffset status =
  StationStatusVisualization { _statusVisStationId       = fromIntegral sid
                             , _statusVisLastReported    = addHours timeOffset (reportToLocal reportTimeZone (status ^. statusLastReported))
                             , _statusVisChargingStation = status ^. statusIsChargingStation
                             , _statusVisBikesAvailable  = fromIntegral $ status ^. statusNumBikesAvailable
                             , _statusVisBikesDisabled   = fromIntegral $ status ^. statusNumBikesDisabled
                             , _statusVisDocksAvailable  = fromIntegral $ status ^. statusNumDocksAvailable
                             , _statusVisDocksDisabled   = fromIntegral $ status ^. statusNumDocksDisabled
                             , _statusVisAvailableIconic = fromIntegral (status ^. vehicleTypesAvailableBoost)
                             , _statusVisAvailableEfit   = fromIntegral (status ^. vehicleTypesAvailableEfit)
                             , _statusVisAvailableEfitG5 = fromIntegral (status ^. vehicleTypesAvailableEfitG5)
                             }
  where
    StationInformationId sid = _statusStationId status

generateJsonDataSource :: Int -> AppM [StationStatusVisualization]
generateJsonDataSource stationId = do
  result <- queryStationStatusBetween stationId startTime endTime
  -- TODO: do UTC -> local time conversion here as well.
  pure $ map (fromBeamStationStatusToVisJSON (-4)) result
  where
    startTime = reportTime (fromGregorian 2023 10 17) (TimeOfDay 00 00 00)
    endTime   = reportTime (fromGregorian 2023 10 18) (TimeOfDay 00 00 00)
