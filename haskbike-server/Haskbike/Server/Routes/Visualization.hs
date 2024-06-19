{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains the routes for the visualization API.

module Haskbike.Server.Routes.Visualization
     ( VisualizationAPI (..)
     ) where

import           Data.Time

import           Database.Beam

import           Haskbike.Database.Tables.StationInformation
import qualified Haskbike.Database.Tables.StationOccupancy       as DB
import           Haskbike.Database.Tables.StationStatus
import           Haskbike.Server.Page.List.StationList
import           Haskbike.Server.Page.PerformanceCSV
import           Haskbike.Server.Page.SideMenu
import           Haskbike.Server.Page.StationStatusVisualization
import           Haskbike.Server.Page.SystemInfoVisualization
import           Haskbike.Server.Page.SystemStatusVisualization

import           Servant
import           Servant.HTML.Lucid


-- | Visualization API handler.
data VisualizationAPI mode where
  VisualizationAPI ::
    { pageForStation :: mode :-
        "visualization" :>
          "station-status"
          :> QueryParam "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu StationStatusVisualizationPage)
    , systemStatus :: mode :-
        "visualization" :>
          "system-status"
          :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu SystemStatusVisualizationPage)
    , stationList :: mode :-
        "visualization" :>
          "station-list"
          :> QueryParam "end-time"     LocalTime
          :> Get '[HTML] (PureSideMenu (StationList [(StationInformation, StationStatus)]))
    , stationEmptyFullList :: mode :-
        "visualization" :>
          "station-occupancy"
          :> QueryParam "start-time"   LocalTime
          :> QueryParam "end-time"     LocalTime
          :> Get '[HTML] (PureSideMenu (StationList [(StationInformation, StationStatus, DB.EmptyFull)]))
    , systemInfo :: mode :-
        "visualization" :>
          "system-information"
          :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu SystemInfoVisualizationPage)
    , performanceCsvPage :: mode :-
        "visualization" :>
          "system-status" :>
          "performance" :>
          "csv"
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu PerformanceCSV)
    } -> VisualizationAPI mode
  deriving stock Generic
