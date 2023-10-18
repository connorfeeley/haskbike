{-# LANGUAGE DataKinds #-}

-- |

module Server.VisualizationAPI
     ( VisualizationAPI
     ) where

import           Data.Time

import           Servant

import           Server.HTML
import           Server.Page.StationStatusVisualization


type VisualizationAPI =
  "visualization"
    :> "station-status"
      :> Capture "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
      :> Get '[HTMLLucid] StationStatusVisualizationPage
