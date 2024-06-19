{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains the routes for the visualization API.

module Haskbike.Server.Routes.Visualization
     ( VisualizationAPI (..)
     ) where

import           Colog

import           Control.Lens
import           Control.Monad.Catch                             ( MonadCatch, MonadThrow )
import           Control.Monad.Except                            ( MonadError )

import           Data.Default.Class                              ( def )
import           Data.Maybe                                      ( fromMaybe, listToMaybe )
import qualified Data.Text                                       as T
import           Data.Time
import           Data.Time.Extras

import           Database.Beam

import           Haskbike.Database.Expressions
import           Haskbike.Database.Operations
import           Haskbike.Database.Tables.StationInformation
import qualified Haskbike.Database.Tables.StationOccupancy       as DB
import           Haskbike.Database.Tables.StationStatus
import           Haskbike.Server.API.Static
import           Haskbike.Server.Page.List.StationList
import           Haskbike.Server.Page.PerformanceCSV
import           Haskbike.Server.Page.SideMenu
import           Haskbike.Server.Page.StationStatusVisualization
import           Haskbike.Server.Page.SystemInfoVisualization
import           Haskbike.Server.Page.SystemStatusVisualization
import           Haskbike.Server.Routes.Data
import           Haskbike.ServerEnv
import           Haskbike.TimeInterval

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic

import           UnliftIO                                        ( MonadUnliftIO )


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
