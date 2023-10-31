-- | This module contains types used to serialize station status data for use in the visualization API.

module Server.Data.StationStatusChargingsVisualization
     ( StationStatusVisualization (..)
     , StatusDataParams (..)
     , enforceTimeRangeBounds
     , fromBeamStationStatusToVisJSON
     , generateJsonDataSource
     ) where

import           AppEnv                           ( envTimeZone, runAppM )

import           Control.Lens                     hiding ( (.=) )
import           Control.Monad.Except

import           Data.Aeson
import           Data.Maybe
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.Operations
import           Database.BikeShare.StationStatus

import           GHC.Generics

import           ServerEnv


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
                             , _statusVisBikesAvailable  = fromIntegral (status ^. statusNumBikesAvailable)
                             , _statusVisBikesDisabled   = fromIntegral (status ^. statusNumBikesDisabled)
                             , _statusVisDocksAvailable  = fromIntegral (status ^. statusNumDocksAvailable)
                             , _statusVisDocksDisabled   = fromIntegral (status ^. statusNumDocksDisabled)
                             , _statusVisAvailableIconic = fromIntegral (status ^. vehicleTypesAvailableIconic)
                             , _statusVisAvailableEfit   = fromIntegral (status ^. vehicleTypesAvailableEfit)
                             , _statusVisAvailableEfitG5 = fromIntegral (status ^. vehicleTypesAvailableEfitG5)
                             }
  where
    StationInformationId sid = _statusStationId status

generateJsonDataSource :: Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StationStatusVisualization]
generateJsonDataSource stationId startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- getAppEnvFromServer
  let tz = envTimeZone appEnv
  -- AppM actions can be lifter into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  let params = StatusDataParams tz currentUtc (TimePair startTime endTime)
  let range = enforceTimeRangeBounds params
  -- result <- liftIO $ runAppM appEnv $ queryChargingEventsCountExpr (StatusVariationQuery (Just (fromIntegral stationId)) [EarliestTime (localTimeToUTC tz (earliestTime  range)), LatestTime (localTimeToUTC tz (latestTime range))])
  result <- liftIO $ runAppM appEnv $ queryStationStatusBetween stationId (localTimeToUTC tz (earliestTime  range)) (localTimeToUTC tz (latestTime range))
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
