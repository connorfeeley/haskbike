{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.VisualizationAPI
     ( VisualizationRoutes (..)
     ) where

import           Data.Time

import           GHC.Generics                           ( Generic )

import           Servant
import           Servant.HTML.Lucid

import           Server.Page.StationStatusVisualization


data VisualizationRoutes mode where
  VisualizationRoutes ::
    { pageForStation :: mode :-
        "station-status"
        :> Capture "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
        :> Get '[HTML] StationStatusVisualizationPage
    } -> VisualizationRoutes mode
  deriving stock Generic
