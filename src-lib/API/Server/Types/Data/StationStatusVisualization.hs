-- FIXME: remove once server is fleshed out a bit more.
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


-- | This module contains types used to serialize station status data for use in the visualization API.

module API.Server.Types.Data.StationStatusVisualization
     ( StationStatusVisualization (..)
     , StatusDataParams (..)
     , enforceTimeRangeBounds
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
import           Data.Time.Extras

import           Database.BikeShare.Operations
import           Database.BikeShare.StationStatus

import           GHC.Generics


-- | Type representing a the visualization data for a BikeShare station's status.
data StationStatusVisualization where
  StationStatusVisualization :: { _statusVisStationId       :: Int
                                , _statusVisLastReported    :: UTCTime
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
    object [ "Station ID"           .= _statusVisStationId        station
           , "Last Reported"        .= _statusVisLastReported     station
           , "Charging"             .= _statusVisChargingStation  station
           , "Available Bikes "     .= _statusVisBikesAvailable   station
           , "Disabled Bikes"       .= _statusVisBikesDisabled    station
           , "Available Docks"      .= _statusVisDocksAvailable   station
           , "Disabled Docks"       .= _statusVisDocksDisabled    station
           , "Available Mechanical" .= _statusVisAvailableIconic  station
           , "Available E-Fit"      .= _statusVisAvailableEfit    station
           , "Available E-Fit G5"   .= _statusVisAvailableEfitG5  station
           ]

-- | Convert from the Beam StationStatus type to StationStatusVisualization
fromBeamStationStatusToVisJSON :: StationStatus -> StationStatusVisualization
fromBeamStationStatusToVisJSON status =
  StationStatusVisualization { _statusVisStationId       = fromIntegral sid
                             , _statusVisLastReported    = status ^. statusLastReported
                             , _statusVisChargingStation = status ^. statusIsChargingStation
                             , _statusVisBikesAvailable  = fromIntegral $ status ^. statusNumBikesAvailable
                             , _statusVisBikesDisabled   = fromIntegral $ status ^. statusNumBikesDisabled
                             , _statusVisDocksAvailable  = fromIntegral $ status ^. statusNumDocksAvailable
                             , _statusVisDocksDisabled   = fromIntegral $ status ^. statusNumDocksDisabled
                             , _statusVisAvailableIconic = fromIntegral (status ^. vehicleTypesAvailableIconic)
                             , _statusVisAvailableEfit   = fromIntegral (status ^. vehicleTypesAvailableEfit)
                             , _statusVisAvailableEfitG5 = fromIntegral (status ^. vehicleTypesAvailableEfitG5)
                             }
  where
    StationInformationId sid = _statusStationId status

generateJsonDataSource :: Int -> Maybe LocalTime -> Maybe LocalTime -> AppM [StationStatusVisualization]
generateJsonDataSource stationId startTime endTime = do
  -- Get the current TimeZone and construct our query limits.
  tz <- liftIO getCurrentTimeZone
  currentUtc <- liftIO getCurrentTime
  let params = StatusDataParams tz currentUtc (TimePair startTime endTime)
  let range = enforceTimeRangeBounds params
  -- result <- queryStationStatusBetween stationId (earliestTime (visTimeRange params))
  result <- queryStationStatusBetween stationId (localTimeToUTC tz (earliestTime  range)) (localTimeToUTC tz (latestTime range))
  pure $ map fromBeamStationStatusToVisJSON result

data StatusDataParams a where
  StatusDataParams :: { visTimeZone :: TimeZone
                      , visCurrentUtc :: UTCTime
                      , visTimeRange :: TimePair a
                      } -> StatusDataParams a
  deriving (Show, Generic, Eq, Ord)

enforceTimeRangeBounds :: StatusDataParams (Maybe LocalTime) -> TimePair LocalTime
enforceTimeRangeBounds params = TimePair start end
  where
    tz = visTimeZone params
    currentUtc = visCurrentUtc params
    yesterday = addUTCTime (-24 * 3600) currentUtc
    earliest = earliestTime (visTimeRange params)
    latest   = latestTime   (visTimeRange params)

    -- Default to 24 hours ago -> now.
    start = fromMaybe (utcToLocalTime tz yesterday)  earliest
    end   = fromMaybe (utcToLocalTime tz currentUtc) latest
