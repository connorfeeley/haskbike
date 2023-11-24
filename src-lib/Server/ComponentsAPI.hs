{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.ComponentsAPI
     ( ComponentsAPI (..)
     ) where

import           Data.Time

import           GHC.Generics                    ( Generic )

import           Servant
import           Servant.HTML.Lucid

import           Server.Page.StatusVisualization

-- HTMX API
data ComponentsAPI mode where
  ComponentsAPI ::
    { dockingEventsHeader :: mode :-
      "components" :>
        "events"
          :> "docking"
            :> QueryParam "station-id" Int
            :> QueryParam "start-time" LocalTime
            :> QueryParam "end-time" LocalTime
            :> Get '[HTML] DockingHeader
    , chargingEventsHeader :: mode :-
      "components" :>
        "events"
          :> "charging"
            :> QueryParam "station-id" Int
            :> QueryParam "start-time" LocalTime
            :> QueryParam "end-time" LocalTime
            :> Get '[HTML] ChargingHeader
    } -> ComponentsAPI mode
  deriving stock Generic
