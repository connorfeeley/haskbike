{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.VisualizationAPI
     ( VisualizationAPI (..)
     ) where

import           Data.Time

import           GHC.Generics                           ( Generic )

import           Servant
import           Servant.HTML.Lucid

import           Server.Page.SideMenu
import           Server.Page.StationStatusVisualization


data VisualizationAPI mode where
  VisualizationAPI ::
    { pageForStation :: mode :-
        "visualization" :>
          "station-status"
          :> QueryParam "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu StationStatusVisualizationPage)
    } -> VisualizationAPI mode
  deriving stock Generic
