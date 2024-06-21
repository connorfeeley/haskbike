{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains the routes for the HTMX components API.

module Haskbike.Server.Routes.Components
     ( ComponentsAPI (..)
     , EventsComponentAPI (..)
     , componentsRoutesLinks
     ) where

import           Data.Time

import           GHC.Generics                                            ( Generic )

import           Haskbike.Database.EndpointQueried                       ( EndpointQueried )
import           Haskbike.Server.Components.ChargingHeader
import           Haskbike.Server.Components.ChargingInfrastructureHeader
import           Haskbike.Server.Components.DockingHeader
import           Haskbike.Server.Components.PerformanceData
import           Haskbike.Server.Components.QueryHistory
import           Haskbike.Server.LatestQueries

import           Servant
import           Servant.HTML.Lucid


-- HTMX API
data ComponentsAPI mode where
  ComponentsAPI ::
    { eventsComponents :: mode :- "components" :> NamedRoutes EventsComponentAPI
    } -> ComponentsAPI mode
  deriving stock Generic

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
          :> Capture "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[HTML] PerformanceData
    , latestQueries :: mode :-
      "latest-queries"
          :> QueryParam "time" LocalTime
          :> Get '[HTML] LatestQueries
    , queryHistory :: mode :-
      "query-history"
          :> QueryParam "endpoint"   EndpointQueried
          :> QueryParam "start-time" UTCTime
          :> QueryParam "end-time"   UTCTime
      :> Get '[HTML] QueryHistoryComponent
    } -> EventsComponentAPI mode
  deriving stock Generic

componentsRoutesLinks :: ComponentsAPI (AsLink Link)
componentsRoutesLinks = allFieldLinks
