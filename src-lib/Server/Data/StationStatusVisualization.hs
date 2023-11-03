-- | This module contains types used to serialize station status data for use in the visualization API.

module Server.Data.StationStatusVisualization
     ( StationStatusVisualization (..)
     , fromBeamStationStatusToVisJSON
     , generateJsonDataSource
     ) where

import           AppEnv

import           Control.Lens                     hiding ( (.=) )
import           Control.Monad.Except

import           Data.Aeson
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.BikeShare.Expressions
import           Database.BikeShare.StationStatus

import           Server.Page.StatusVisualization

import           ServerEnv


-- | Type representing a the visualization data for a BikeShare station's status.
data StationStatusVisualization where
  StationStatusVisualization :: { _statusVisStationId       :: Maybe Int
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
  StationStatusVisualization { _statusVisStationId       = Just (fromIntegral sid)
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

generateJsonDataSource :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StationStatusVisualization]
generateJsonDataSource (Just stationId) startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- getAppEnvFromServer
  let tz = envTimeZone appEnv
  -- AppM actions can be lifter into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  let params = StatusDataParams tz currentUtc (TimePair startTime endTime)
  let range = enforceTimeRangeBounds params
  result <- liftIO $ runAppM appEnv $ withPostgres $
    runSelectReturningList $ select $ limit_ 10000 $
    statusBetweenExpr (fromIntegral stationId) (localTimeToUTC tz (earliestTime  range)) (localTimeToUTC tz (latestTime range))

  pure $ map fromBeamStationStatusToVisJSON result

generateJsonDataSource Nothing startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- getAppEnvFromServer
  let tz = envTimeZone appEnv
  -- AppM actions can be lifter into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  let params = StatusDataParams tz currentUtc (TimePair startTime endTime)
  let rangeBounded = enforceTimeRangeBounds params
  let rangeIncrement = minsPerHourlyInterval 4 -- 15 minutes

  statusAtRange <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith $
    querySystemStatusAtRangeExpr (localTimeToUTC tz (earliestTime rangeBounded)) (localTimeToUTC tz (latestTime rangeBounded)) rangeIncrement
  (pure . map toVisualization) statusAtRange
  where
    toVisualization st =
      -- Convert fields from 'Int32' to 'Int'.
      StationStatusVisualization { _statusVisStationId       = Nothing
                                 , _statusVisLastReported    = st ^. _1 -- Just use the latest time.
                                 , _statusVisChargingStation = True
                                 , _statusVisBikesAvailable  = st ^. _2 & fromIntegral
                                 , _statusVisBikesDisabled   = st ^. _3 & fromIntegral
                                 , _statusVisDocksAvailable  = st ^. _4 & fromIntegral
                                 , _statusVisDocksDisabled   = st ^. _5 & fromIntegral
                                 , _statusVisAvailableIconic = st ^. _6 & fromIntegral
                                 , _statusVisAvailableEfit   = st ^. _7 & fromIntegral
                                 , _statusVisAvailableEfitG5 = st ^. _8 & fromIntegral
                                 }
