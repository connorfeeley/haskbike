{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Haskbike.Server.API.Data
     ( statusHandler
     ) where

import           Colog

import           Control.Monad.Catch                                 ( MonadCatch )

import qualified Data.ByteString.Lazy                                as BL
import           Data.Csv                                            ( encodeDefaultOrderedByName )
import qualified Data.Text                                           as T
import           Data.Time
import           Data.Time.Extras

import           Haskbike.API.VehicleType
import           Haskbike.Database.EventCounts
import           Haskbike.Database.Expressions                       ( queryStationBefore )
import           Haskbike.Database.Operations.Dockings
import           Haskbike.Database.Operations.Factors
import           Haskbike.Database.Operations.StationOccupancy
import           Haskbike.Database.StatusVariationQuery
import qualified Haskbike.Database.Tables.StationOccupancy           as DB
import           Haskbike.Server.Components.PerformanceData
import           Haskbike.Server.Data.EmptyFullData
import           Haskbike.Server.Data.FactorsCSV
import           Haskbike.Server.Data.StationList
import           Haskbike.Server.Data.StationStatusVisualization
import           Haskbike.Server.Data.SystemInformationVisualization
import           Haskbike.Server.Routes.Data
import           Haskbike.Server.StatusDataParams
import           Haskbike.ServerEnv

import           Servant
import           Servant.Server.Generic

import           UnliftIO


-- * Handlers.

statusHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m) => DataAPI (AsServerT m)
statusHandler =  DataAPI { dataForStation       = stationStatusData
                         , integralsForStation  = stationIntegralData
                         , factorsForStation    = stationFactorData
                         , systemInfoData       = systemInfoDataHandler
                         , performanceCsv       = performanceCsvHandler
                         , dockingEventsData    = handleDockingEventsData
                         , chargingEventsData   = handleChargingEventsData
                         , stationListData      = handleStationListData
                         , emptyFullData        = handleEmptyFullData
                         }


stationStatusData :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m) => Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> m [StationStatusVisualization]
stationStatusData stationId startTime endTime = do
  logInfo $ "Creating JSON payload for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) startTime <> ", end time: " <> (T.pack . show) endTime <> " "
  dataSource <- generateJsonDataSource stationId startTime endTime
  logDebug "Created JSON payload"
  pure dataSource


stationIntegralData :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> m [StatusIntegral]
stationIntegralData stationId startTime endTime = do
  logInfo $ "Creating integral JSON payload for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) startTime <> ", end time: " <> (T.pack . show) endTime <> " "
  dataSource <- generateJsonDataSourceIntegral  stationId startTime endTime
  logDebug "Created integral JSON payload"
  pure dataSource


stationFactorData :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> m [StatusFactor]
stationFactorData stationId startTime endTime = do
  logInfo $ "Creating factor JSON payload for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) startTime <> ", end time: " <> (T.pack . show) endTime <> " "
  dataSource <- generateJsonDataSourceFactor  stationId startTime endTime
  logDebug "Created factor JSON payload"
  pure dataSource

systemInfoDataHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe LocalTime -> Maybe LocalTime -> m [SystemInformationCountVisualization]
systemInfoDataHandler startTime endTime = do
  logInfo $ "Creating system information JSON payload for {start time: " <> (T.pack . show) startTime <> ", end time: " <> (T.pack . show) endTime <> " "
  dataSource <- generateJsonDataSourceSysInfo startTime endTime
  logDebug "Created factor JSON payload"
  pure dataSource

performanceCsvHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> m (Headers '[Header "Content-Disposition" T.Text] BL.ByteString)
performanceCsvHandler stationId startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  logInfo $ "Creating performance data CSV payload for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) startTime <> ", end time: " <> (T.pack . show) endTime <> " "

  let params = StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc)
  let range = enforceTimeRangeBounds params
  let variation = StatusVariationQuery (fromIntegral <$> stationId)
        [ EarliestTime (localTimeToUTC tz (earliestTime range))
        , LatestTime   (localTimeToUTC tz (latestTime range))
        ]

  (integrals, emptyFullTup) <- concurrently
    (queryIntegratedStatus variation)
    (queryStationOccupancy 0 0 stationId (localTimeToUTC tz (earliestTime range)) (localTimeToUTC tz (latestTime range)))

  let emptyFull = head $ map (\(_inf, occ) -> DB.emptyFullFromSecs (DB._stnOccEmptySec occ) (DB._stnOccFullSec occ)) emptyFullTup

  logDebug "Created performance data CSV payload"

  let fileContent = encodeIntegrals emptyFull integrals

  let stationIdString :: String = maybe "system" show stationId
  let filename :: String = stationIdString <> "-performance-" <> show (earliestTime range) <> "-" <> show (latestTime range) <> ".csv"
  let header = "attachment; filename=\"" <> (T.pack . replaceSpaces) filename <> "\""
  logDebug $ "Added file header: " <> header
  pure $ addHeader header (fileContent :: BL.ByteString)
  where
    encodeIntegrals emptyFullTup = encodeDefaultOrderedByName . map (PerformanceDataCSV . integralToPerformanceData emptyFullTup)

replaceSpaces :: String -> String
replaceSpaces [] = []
replaceSpaces (x:xs)
    | x == ' ' = "-" ++ replaceSpaces xs
    | otherwise = x : replaceSpaces xs

handleDockingEventsData :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> m [DockingEventsCount]
handleDockingEventsData stationId startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc))
  let (earliest, latest) = (earliestTime times', latestTime times')

  logInfo . T.pack $ "Rendering data for {station ID: " <> show stationId <> ", start time: " <> show earliest <> ", end time: " <> show latest <> "} "

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  queryDockingEventsCount variation

handleChargingEventsData :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> m [ChargingEvent]
handleChargingEventsData stationId startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc))
  let (earliest, latest) = (earliestTime times', latestTime times')

  logInfo . T.pack $ "Rendering data for {station ID: " <> show stationId <> ", start time: " <> show earliest <> ", end time: " <> show latest <> "} "

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  events <- queryChargingEventsCount variation
  let result = map (\(_info, _totalCount, efitCount, efitG5Count) ->
                      [ ChargingEvent EFit (fromIntegral efitCount)
                      , ChargingEvent EFitG5 (fromIntegral efitG5Count)
                      ])
               events
  pure $ concat result


handleStationListData :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m)
                      => Maybe LocalTime -> m [StationListRecord]
handleStationListData latestTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  logInfo $ "Rendering station list data for time [" <> tshow latestTime <> "]"

  latest <- queryStationBefore (maybe currentUtc (localTimeToUTC tz) latestTime)

  pure $ map (uncurry StationListRecord) latest
  where
    tshow = T.pack . show


handleEmptyFullData :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m)
                    => Maybe LocalTime -> Maybe LocalTime -> m [DB.EmptyFullRecord]
handleEmptyFullData start end = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  logInfo $ "Rendering station empty/full data for time [" <> tshow start <> " - " <> tshow end <> "]"

  (latest, emptyFull) <- concurrently (queryStationBefore (end' currentUtc tz))
                                      (queryStationOccupancy 0 0 (Nothing :: Maybe Int) (start' currentUtc tz) (end' currentUtc tz)
                                      )
  let combined = combineStations latest (resultToEmptyFull emptyFull)

  pure $ map (uncurry3 DB.EmptyFullRecord) combined
  where
    resultToEmptyFull = map (\(i, occ) -> (i, DB.emptyFullFromSecs (DB._stnOccEmptySec occ) (DB._stnOccFullSec occ)))
    tshow = T.pack . show
    start' cUtc tz = maybe (addUTCTime (-60 * 60 * 24) cUtc) (localTimeToUTC tz) start
    end'   cUtc tz = maybe cUtc (localTimeToUTC tz) end

-- | Uncurry a function of three arguments.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
