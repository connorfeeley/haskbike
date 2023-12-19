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

import           Database.BikeShare.EventCounts
import           Database.BikeShare.Operations.Dockings
import           Database.BikeShare.Operations.Factors
import           Database.BikeShare.Operations.FactorsCSV
import           Database.BikeShare.StatusVariationQuery

import           Fmt                                        ( format )

import           GHC.Generics                               ( Generic )

import           Servant
import           Servant.Server.Generic

import           Server.Components.PerformanceData
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
                         }


stationStatusData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StationStatusVisualization]
stationStatusData stationId startTime endTime = do
  logInfo $ format "Creating JSON payload for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime
  dataSource <- generateJsonDataSource stationId startTime endTime
  logDebug "Created JSON payload"
  pure dataSource


stationIntegralData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StatusIntegral]
stationIntegralData stationId startTime endTime = do
  logInfo $ format "Creating integral JSON payload for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime
  dataSource <- generateJsonDataSourceIntegral  stationId startTime endTime
  logDebug "Created integral JSON payload"
  pure dataSource


stationFactorData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StatusFactor]
stationFactorData stationId startTime endTime = do
  logInfo $ format "Creating factor JSON payload for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime
  dataSource <- generateJsonDataSourceFactor  stationId startTime endTime
  logDebug "Created factor JSON payload"
  pure dataSource

systemInfoDataHandler :: Maybe LocalTime -> Maybe LocalTime -> ServerAppM [SystemInformationCountVisualization]
systemInfoDataHandler startTime endTime = do
  logInfo $ format "Creating system information JSON payload for {start time: {}, end time: {}} " startTime endTime
  dataSource <- generateJsonDataSourceSysInfo startTime endTime
  logDebug "Created factor JSON payload"
  pure dataSource

performanceCsvHandler :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM (Headers '[Header "Content-Disposition" T.Text] BL.ByteString)
performanceCsvHandler stationId startTime endTime = do
  logInfo $ format "Creating performance data CSV payload for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime

  appEnv <- getAppEnvFromServer
  let tz = envTimeZone appEnv
  currentUtc <- liftIO getCurrentTime

  let params = StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc)
  let range = enforceTimeRangeBounds params
  let variation = StatusVariationQuery (fromIntegral <$> stationId)
        [ EarliestTime (localTimeToUTC tz (earliestTime range))
        , LatestTime   (localTimeToUTC tz (latestTime range))
        ]

  integrals <- liftIO $ runAppM appEnv $ queryIntegratedStatus variation

  logDebug "Created performance data CSV payload"

  let fileContent = encodeIntegrals integrals

  let stationIdString :: String = maybe "system" (format "station-{}") stationId
  let filename :: String = format "{}-performance-{}-{}.csv" stationIdString (earliestTime range) (latestTime range)
  pure $ addHeader (format "attachment; filename=\"{}\"" (replaceSpaces filename)) (fileContent :: BL.ByteString)
  where
    encodeIntegrals = encodeDefaultOrderedByName . map (PerformanceDataCSV . integralToPerformanceData)

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

  logInfo $ format "Rendering page for {station ID: {}, start time: {}, end time: {}} " stationId earliest latest

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

  logInfo $ format "Rendering page for {station ID: {}, start time: {}, end time: {}} " stationId earliest latest

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
