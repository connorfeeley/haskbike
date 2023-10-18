{-# LANGUAGE DataKinds #-}

-- |

module Server.VisualizationAPI
     ( VisualizationAPI
     , VisualizationRoutes (..)
     , visualizationAPI
     ) where

import           Data.Time

import           GHC.Generics                           ( Generic )

import           Servant
import           Servant.HTML.Lucid

import           Server.Page.StationStatusVisualization


type VisualizationAPI =
  "visualization"
    :> "station-status"
      :> Capture "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
      :> Get '[HTML] StationStatusVisualizationPage

visualizationAPI :: Proxy VisualizationAPI
visualizationAPI = Proxy @VisualizationAPI

data VisualizationRoutes mode = VisualizationRoutes
  { pageForStation ::
      mode :- "station-status"
      :> Capture "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
      :> Get '[HTML] StationStatusVisualizationPage
  } deriving Generic
