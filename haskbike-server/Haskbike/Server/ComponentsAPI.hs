{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Haskbike.Server.ComponentsAPI
     ( ComponentsAPI (..)
     , EventsComponentAPI (..)
     , componentsHandler
     ) where

import           Colog

import           Control.Monad.Catch                                     ( MonadCatch )
import           Control.Monad.Except                                    ( MonadError )

import qualified Data.Text                                               as T
import           Data.Time
import           Data.Time.Extras

import           Database.Beam

import           Haskbike.Database.Expressions                           ( queryChargingInfrastructure )
import           Haskbike.Database.Operations.Dockings
import           Haskbike.Database.Operations.Factors
import           Haskbike.Database.Operations.StationOccupancy
import           Haskbike.Database.StatusVariationQuery                  ( StatusThreshold (..),
                                                                           StatusVariationQuery (..) )
import qualified Haskbike.Database.Tables.StationOccupancy               as DB
import           Haskbike.Server.Components.ChargingHeader
import           Haskbike.Server.Components.ChargingInfrastructureHeader
import           Haskbike.Server.Components.DockingHeader
import           Haskbike.Server.Components.PerformanceData
import           Haskbike.Server.StatusDataParams
import           Haskbike.ServerEnv

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic                                  ( AsServerT )

import           UnliftIO

-- HTMX API
data ComponentsAPI mode where
  ComponentsAPI ::
    { eventsComponents :: mode :- "components" :> NamedRoutes EventsComponentAPI
    } -> ComponentsAPI mode
  deriving stock Generic

componentsHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, MonadError ServerError m)
                  => ComponentsAPI (AsServerT m)
componentsHandler =
  ComponentsAPI { eventsComponents = eventsComponentHandler }

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
    , chargingInfrastructureHeader :: mode :-
      "system-status"
        :> "charging-infrastructure"
          :> QueryParam "time" LocalTime
          :> Get '[HTML] ChargingInfrastructureHeader
    , performanceHeader :: mode :-
      "station-status"
        :> "performance"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[HTML] PerformanceData
    } -> EventsComponentAPI mode
  deriving stock Generic

eventsComponentHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, MonadError ServerError m)
                       => EventsComponentAPI (AsServerT m)
eventsComponentHandler = EventsComponentAPI
  { dockingEventsHeader          = dockingsHeader
  , chargingEventsHeader         = chargingsHeader
  , chargingInfrastructureHeader = chargingInfrastructureHeaderHandler
  , performanceHeader            = performanceHeaderHandler
  }

dockingsHeader :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m)
               => Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> m DockingHeader
dockingsHeader stationId startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc))
  let (earliest, latest) = (earliestTime times', latestTime times')

  logDebug $ "Rendering docking component for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) earliest <> ", end time: " <> (T.pack . show) latest

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  events <- queryDockingEventsCount variation
  pure $ DockingHeader events

chargingsHeader :: (HasEnv env m, MonadIO m, MonadCatch m)
                => Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> m ChargingHeader
chargingsHeader stationId startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc))
  let (earliest, latest) = (earliestTime times', latestTime times')

  logDebug $ "Rendering charging component for {station ID: " <> (T.pack . show) stationId <> ", start time: " <> (T.pack . show) earliest <> ", end time: " <> (T.pack . show) latest

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  chargings <- queryChargingEventsCount variation

  pure $ ChargingHeader chargings

chargingInfrastructureHeaderHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadError ServerError m)
                                    => Maybe LocalTime -> m ChargingInfrastructureHeader
chargingInfrastructureHeaderHandler t = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  -- Query charging infrastructure at given time.
  events <- withPostgres $ runSelectReturningOne $ selectWith $
    queryChargingInfrastructure (maybe currentUtc (localTimeToUTC tz) t)

  case events of
    Just (eStationCnt, eDockCnt) ->
      pure $ ChargingInfrastructureHeader  { chargingStationCount = fromIntegral eStationCnt
                                           , chargingDockCount    = fromIntegral eDockCnt
                                           }
    _ ->  throwError err500 { errBody = "Unable to calculate charging infrastructure counts." }

performanceHeaderHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadError ServerError m, MonadUnliftIO m)
                         => Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> m PerformanceData
performanceHeaderHandler stationId startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  let params = StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc)
  let range = enforceTimeRangeBounds params

  (perf, emptyFullTup) <- concurrently
    (queryIntegratedStatus
    (StatusVariationQuery (fromIntegral <$> stationId)
      [ EarliestTime (localTimeToUTC tz (earliestTime range))
      , LatestTime   (localTimeToUTC tz (latestTime   range))
      ]
    ))
    (withPostgresTransaction $ queryStationOccupancy 0 0 stationId
      (localTimeToUTC tz (earliestTime range)) (localTimeToUTC tz (latestTime range))
    )
  let emptyFull = head $ map (\(_inf, occ) -> DB.emptyFullFromSecs (DB._stnOccEmptySec occ) (DB._stnOccFullSec occ)) emptyFullTup

  pure $ (head . map (integralToPerformanceData emptyFull)) perf
