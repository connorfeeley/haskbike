{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.ComponentsAPI
     ( ComponentsAPI (..)
     , EventsComponentAPI (..)
     , componentsHandler
     ) where

import           Colog

import qualified Data.Text                               as T
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.Operations.Dockings
import           Database.BikeShare.Operations.Factors
import           Database.BikeShare.StatusVariationQuery ( StatusThreshold (..), StatusVariationQuery (..) )

import           GHC.Generics                            ( Generic )

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic                  ( AsServerT )

import           Server.Components.ChargingHeader
import           Server.Components.DockingHeader
import           Server.Components.PerformanceData
import           Server.StatusDataParams

import           ServerEnv

import           UnliftIO

-- HTMX API
data ComponentsAPI mode where
  ComponentsAPI ::
    { eventsComponents :: mode :- "components" :> NamedRoutes EventsComponentAPI
    } -> ComponentsAPI mode
  deriving stock Generic

componentsHandler :: ComponentsAPI (AsServerT ServerAppM)
componentsHandler =
  ComponentsAPI { eventsComponents = eventsComponentHandler
                }

data EventsComponentAPI mode where
  EventsComponentAPI ::
    { dockingEventsHeader :: mode :-
      "events"
        :> "docking"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[HTML] DockingHeader
    , chargingEventsHeader :: mode :-
      "events"
        :> "charging"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[HTML] ChargingHeader
    , performanceHeader :: mode :-
      "station-status"
        :> "performance"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[HTML] PerformanceData
    } -> EventsComponentAPI mode
  deriving stock Generic

eventsComponentHandler :: EventsComponentAPI (AsServerT ServerAppM)
eventsComponentHandler = EventsComponentAPI
  { dockingEventsHeader  = dockingsHeader
  , chargingEventsHeader = chargingsHeader
  , performanceHeader    = performanceHeaderHandler
  }

dockingsHeader :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM DockingHeader
dockingsHeader stationId startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifted into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc))
  let (earliest, latest) = (earliestTime times', latestTime times')

  logInfo $ "Rendering page for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) earliest <> ", end time: " <> (T.pack . show) latest

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  events <- liftIO $ runAppM appEnv $ queryDockingEventsCount variation
  pure $ DockingHeader events

chargingsHeader :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM ChargingHeader
chargingsHeader stationId startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifted into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc))
  let (earliest, latest) = (earliestTime times', latestTime times')

  logInfo $ "Rendering page for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) earliest <> ", end time: " <> (T.pack . show) latest

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  chargings <- liftIO $ runAppM appEnv $ queryChargingEventsCount variation

  pure $ ChargingHeader chargings

performanceHeaderHandler :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM PerformanceData
performanceHeaderHandler stationId startTime endTime = do
  appEnv <- getAppEnvFromServer
  let tz = envTimeZone appEnv
  currentUtc <- liftIO getCurrentTime

  let params = StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc)
  let range = enforceTimeRangeBounds params

  perf <- liftIO $ runAppM appEnv $
    queryIntegratedStatus
    (StatusVariationQuery (fromIntegral <$> stationId)
      [ EarliestTime (localTimeToUTC tz (earliestTime  range))
      , LatestTime (localTimeToUTC tz (latestTime range))
      ]
    )

  pure $
    (head . map integralToPerformanceData) perf
