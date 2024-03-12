{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.DataAPI
     ( DataAPI (..)
     , statusHandler
     ) where


import           API.VehicleType

import           Colog

import           Data.ByteString.Lazy                       ( ByteString )
import qualified Data.ByteString.Lazy                       as BL
import           Data.Csv                                   ( encodeDefaultOrderedByName )
import           Data.Text                                  ( Text )
import qualified Data.Text                                  as T
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.BikeShare.EventCounts
import           Database.BikeShare.Expressions             ( queryLatestStatuses )
import           Database.BikeShare.Operations.Dockings
import           Database.BikeShare.Operations.Factors
import           Database.BikeShare.Operations.FactorsCSV
import           Database.BikeShare.Operations.StationEmpty
import           Database.BikeShare.StatusVariationQuery

import           Servant
import           Servant.Server.Generic

import           Server.Components.PerformanceData
import           Server.Data.EmptyFullData
import           Server.Data.StationStatusVisualization
import           Server.Data.SystemInformationVisualization
import           Server.StatusDataParams

import           ServerEnv

import           UnliftIO


-- | Data API endpoint.
data DataAPI mode where
  DataAPI ::
    { dataForStation :: mode :-
      "data" :>
        "station-status"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [StationStatusVisualization]
    , integralsForStation :: mode :-
      "data" :>
        "station-status" :> "integral"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [StatusIntegral]
    , factorsForStation :: mode :-
      "data" :>
        "station-status" :> "factor"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [StatusFactor]
    , systemInfoData :: mode :-
      "data" :>
        "system-information"
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [SystemInformationCountVisualization]
    , performanceCsv :: mode :-
      "data" :>
        "system-status" :> "performance" :> "csv"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] ByteString)
    , dockingEventsData :: mode :-
      "data" :>
        "events" :> "docking"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [DockingEventsCount]
    , chargingEventsData :: mode :-
      "data" :>
        "events" :> "charging"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [ChargingEvent]
    , emptyFullData :: mode :-
      "data" :>
        "empty-full"
          :> QueryParam "start-time"   LocalTime
          :> QueryParam "end-time"     LocalTime
          :> Get '[JSON] [EmptyFullRecord]
    } -> DataAPI mode
  deriving stock Generic


-- * Handlers.

statusHandler :: DataAPI (AsServerT ServerAppM)
statusHandler =  DataAPI { dataForStation       = stationStatusData
                         , integralsForStation  = stationIntegralData
                         , factorsForStation    = stationFactorData
                         , systemInfoData       = systemInfoDataHandler
                         , performanceCsv       = performanceCsvHandler
                         , dockingEventsData    = handleDockingEventsData
                         , chargingEventsData   = handleChargingEventsData
                         , emptyFullData        = handleEmptyFullData
                         }


stationStatusData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StationStatusVisualization]
stationStatusData stationId startTime endTime = do
  logInfo $ "Creating JSON payload for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) startTime <> ", end time: " <> (T.pack . show) endTime <> " "
  dataSource <- generateJsonDataSource stationId startTime endTime
  logDebug "Created JSON payload"
  pure dataSource


stationIntegralData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StatusIntegral]
stationIntegralData stationId startTime endTime = do
  logInfo $ "Creating integral JSON payload for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) startTime <> ", end time: " <> (T.pack . show) endTime <> " "
  dataSource <- generateJsonDataSourceIntegral  stationId startTime endTime
  logDebug "Created integral JSON payload"
  pure dataSource


stationFactorData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StatusFactor]
stationFactorData stationId startTime endTime = do
  logInfo $ "Creating factor JSON payload for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) startTime <> ", end time: " <> (T.pack . show) endTime <> " "
  dataSource <- generateJsonDataSourceFactor  stationId startTime endTime
  logDebug "Created factor JSON payload"
  pure dataSource

systemInfoDataHandler :: Maybe LocalTime -> Maybe LocalTime -> ServerAppM [SystemInformationCountVisualization]
systemInfoDataHandler startTime endTime = do
  logInfo $ "Creating system information JSON payload for {start time: " <> (T.pack . show) startTime <> ", end time: " <> (T.pack . show) endTime <> " "
  dataSource <- generateJsonDataSourceSysInfo startTime endTime
  logDebug "Created factor JSON payload"
  pure dataSource

performanceCsvHandler :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM (Headers '[Header "Content-Disposition" T.Text] BL.ByteString)
performanceCsvHandler stationId startTime endTime = do
  logInfo $ "Creating performance data CSV payload for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) startTime <> ", end time: " <> (T.pack . show) endTime <> " "

  appEnv <- getAppEnvFromServer
  let tz = envTimeZone appEnv
  currentUtc <- liftIO getCurrentTime

  let params = StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc)
  let range = enforceTimeRangeBounds params
  let variation = StatusVariationQuery (fromIntegral <$> stationId)
        [ EarliestTime (localTimeToUTC tz (earliestTime range))
        , LatestTime   (localTimeToUTC tz (latestTime range))
        ]

  (integrals, emptyFullTup) <- liftIO $ concurrently
    (runAppM appEnv $ queryIntegratedStatus variation)
    (runAppM appEnv $
     withPostgres $ runSelectReturningList $ selectWith $
     queryStationEmptyFullTime stationId (localTimeToUTC tz (earliestTime range)) (localTimeToUTC tz (latestTime range))
    )
  let emptyFull = head $ map (\(_i, (e, f))
                              -> EmptyFull ((secondsToNominalDiffTime . fromIntegral) e) ((secondsToNominalDiffTime . fromIntegral) f)
                             ) emptyFullTup

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

handleDockingEventsData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [DockingEventsCount]
handleDockingEventsData stationId startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifted into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc))
  let (earliest, latest) = (earliestTime times', latestTime times')

  logInfo . T.pack $ "Rendering data for {station ID: " <> show stationId <> ", start time: " <> show earliest <> ", end time: " <> show latest <> "} "

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  liftIO $ runAppM appEnv $ queryDockingEventsCount variation

handleChargingEventsData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [ChargingEvent]
handleChargingEventsData stationId startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifted into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc))
  let (earliest, latest) = (earliestTime times', latestTime times')

  logInfo . T.pack $ "Rendering data for {station ID: " <> show stationId <> ", start time: " <> show earliest <> ", end time: " <> show latest <> "} "

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  events <- liftIO $ runAppM appEnv $ queryChargingEventsCount variation
  let result = map (\(_info, _totalCount, efitCount, efitG5Count) ->
                      [ ChargingEvent EFit (fromIntegral efitCount)
                      , ChargingEvent EFitG5 (fromIntegral efitG5Count)
                      ])
               events
  pure $ concat result


handleEmptyFullData :: Maybe LocalTime -> Maybe LocalTime -> ServerAppM [EmptyFullRecord]
handleEmptyFullData start end = do
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  currentUtc <- liftIO getCurrentTime
  logInfo $ "Rendering station empty/full data for time [" <> tshow start <> " - " <> tshow end <> "]"

  (latest, emptyFull) <- liftIO $ concurrently (runAppM appEnv $ withPostgres $ runSelectReturningList $ select queryLatestStatuses)
                                               (runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith $
                                                queryStationEmptyFullTime (Nothing :: Maybe Integer) (start' currentUtc tz) (end' currentUtc tz))

  let combined = combineStations latest (resultToEmptyFull emptyFull)

  pure $ map (\(i, ss, ef) -> EmptyFullRecord i ss ef) combined
  where
    resultToEmptyFull = map (\(i, (e, f)) -> (i, EmptyFull ((secondsToNominalDiffTime . fromIntegral) e) ((secondsToNominalDiffTime . fromIntegral) f)))
    tshow = T.pack . show
    start' cUtc tz = maybe (addUTCTime (-60 * 60 * 24) cUtc) (localTimeToUTC tz) start
    end'   cUtc tz = maybe cUtc (localTimeToUTC tz) end
