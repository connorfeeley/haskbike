{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.VisualizationAPI
     ( VisualizationAPI (..)
     ) where

import           Data.Text
import           Data.Time

import           GHC.Generics                           ( Generic )

import           Servant
import           Servant.HTML.Lucid

import           Server.Page.SideMenu
import           Server.Page.StationList
import           Server.Page.StationStatusVisualization


data VisualizationAPI mode where
  VisualizationAPI ::
    { pageForStation :: mode :-
        "visualization" :>
          "station-status"
          :> QueryParam "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu StationStatusVisualizationPage)
    , stationList :: mode :-
        "visualization" :>
          "station-list"
          :> QueryParam "station-type" Text :> Get '[HTML] (PureSideMenu StationList)
    } -> VisualizationAPI mode
  deriving stock Generic
