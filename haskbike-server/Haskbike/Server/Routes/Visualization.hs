{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains the routes for the visualization API.

module Haskbike.Server.Routes.Visualization
     ( VisualizationAPI (..)
     , VisualizationRoutesAPI (..)
     , visualizationRoutesLinks
     ) where

import           Data.Time

import           GHC.Generics                                    ( Generic )

import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Tables.StationInformation
import qualified Haskbike.Database.Tables.StationOccupancy       as DB
import           Haskbike.Database.Tables.StationStatus
import           Haskbike.Server.Page.List.StationList
import           Haskbike.Server.Page.PerformanceCSV
import           Haskbike.Server.Page.QueryHistory
import           Haskbike.Server.Page.SideMenu
import           Haskbike.Server.Page.StationStatusVisualization
import           Haskbike.Server.Page.SystemInfoVisualization
import           Haskbike.Server.Page.SystemStatusVisualization

import           Servant
import           Servant.HTML.Lucid

-- | Visualization API handler.
data VisualizationAPI mode where
  VisualizationAPI ::
    { visualization :: mode :- "visualization" :> NamedRoutes VisualizationRoutesAPI
    } -> VisualizationAPI mode
  deriving stock Generic

-- | Visualization API handler.
data VisualizationRoutesAPI mode where
  VisualizationRoutesAPI ::
    { pageForStation :: mode :-
        "station-status"
          :> QueryParam "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu StationStatusVisualizationPage)
    , systemStatus :: mode :-
        "system-status"
          :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu SystemStatusVisualizationPage)
    , stationList :: mode :-
        "station-list"
          :> QueryParam "end-time"     LocalTime
          :> Get '[HTML] (PureSideMenu (StationList [(StationInformation, StationStatus)]))
    , stationEmptyFullList :: mode :-
        "station-occupancy"
          :> QueryParam "start-time"   LocalTime
          :> QueryParam "end-time"     LocalTime
          :> Get '[HTML] (PureSideMenu (StationList [(StationInformation, StationStatus, DB.EmptyFull)]))
    , systemInfo :: mode :-
        "system-information"
          :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu SystemInfoVisualizationPage)
    , performanceCsvPage :: mode :-
        "system-status" :>
          "performance" :>
          "csv"
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu PerformanceCSV)
    , queryHistoryPage :: mode :-
        "query-history"
          :> QueryParam "endpoint"   EndpointQueried
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time"   LocalTime
          :> Get '[HTML] (PureSideMenu QueryHistoryPage)
    } -> VisualizationRoutesAPI mode
  deriving stock Generic

visualizationRoutesLinks :: VisualizationAPI (AsLink Link)
visualizationRoutesLinks = allFieldLinks
