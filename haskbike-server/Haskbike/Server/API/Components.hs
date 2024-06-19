{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Haskbike.Server.API.Components
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

import           Haskbike.Database.Expressions
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
import           Haskbike.Server.LatestQueries
import           Haskbike.Server.Page.QueryHistory
import           Haskbike.Server.Routes.Components
import           Haskbike.Server.StatusDataParams
import           Haskbike.ServerEnv

import           Servant
import           Servant.Server.Generic                                  ( AsServerT )

import           UnliftIO

componentsHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, MonadError ServerError m, HasServerEnv env m)
                  => ComponentsAPI (AsServerT m)
componentsHandler =
  ComponentsAPI { eventsComponents = eventsComponentHandler }

eventsComponentHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, MonadError ServerError m, HasServerEnv env m)
                       => EventsComponentAPI (AsServerT m)
eventsComponentHandler = EventsComponentAPI
  { dockingEventsHeader          = dockingsHeader
  , chargingEventsHeader         = chargingsHeader
  , chargingInfrastructureHeader = chargingInfrastructureHeaderHandler
  , performanceHeader            = performanceHeaderHandler
  , latestQueries                = latestQueriesHandler
  , queryApiPage                 = queryApiPageHandler
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

latestQueriesHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadError ServerError m, MonadUnliftIO m)
                     => Maybe LocalTime -> m LatestQueries
latestQueriesHandler _t = do
  tz <- getTz
  latest <- withPostgres $ runSelectReturningList $ selectWith queryLatestQueryLogs
  pure $ latestQueryLogsToMap tz latest

queryApiPageHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m)
                    => m QueryHistoryComponent
queryApiPageHandler = do
  logInfo "Rendering performance CSV page"

  pure QueryHistoryComponent
