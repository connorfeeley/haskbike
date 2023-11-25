{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.ComponentsAPI
     ( ComponentsAPI (..)
     , EventsComponentAPI (..)
     , componentsHandler
     ) where

import           Colog

import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.Operations.Dockings
import           Database.BikeShare.StatusVariationQuery ( StatusThreshold (..), StatusVariationQuery (..) )

import           Fmt

import           GHC.Generics                            ( Generic )

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic                  ( AsServerT )

import           Server.Page.StatusVisualization
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
    } -> EventsComponentAPI mode
  deriving stock Generic

eventsComponentHandler :: EventsComponentAPI (AsServerT ServerAppM)
eventsComponentHandler = EventsComponentAPI
  { dockingEventsHeader  = dockingsHeader
  , chargingEventsHeader = chargingsHeader
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

  logInfo $ format "Rendering page for {station ID: {}, start time: {}, end time: {}} " stationId earliest latest

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

  logInfo $ format "Rendering page for {station ID: {}, start time: {}, end time: {}} " stationId earliest latest

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  chargings <- liftIO $ runAppM appEnv $ queryChargingEventsCount variation

  pure $ ChargingHeader chargings
