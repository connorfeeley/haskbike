{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.ComponentsAPI
     ( ComponentsAPI (..)
     ) where

import           Data.Time

import           Database.BikeShare.Operations   ( AvailabilityCountVariation (..) )

import           GHC.Generics                    ( Generic )

import           Servant
import           Servant.HTML.Lucid

import           Server.Page.StatusVisualization

-- HTMX API
data ComponentsAPI mode where
  ComponentsAPI ::
    { dockingEventsHeader :: mode :-
      "components" :>
        "station-status"
          :> "docking-events"
            :> QueryParam "station-id" Int
            :> QueryParam "start-time" LocalTime
            :> QueryParam "end-time" LocalTime
            :> Get '[HTML] DockingHeader
    , dockingsForStation :: mode :-
      "components" :>
        "station-status"
          :> "dockings"
            :> QueryParam "station-id" Int
            :> QueryParam "start-time" LocalTime
            :> QueryParam "end-time" LocalTime
            :> Get '[HTML] (DockingEventsHeader 'Docking)
    , undockingsForStation :: mode :-
      "components" :>
        "station-status"
          :> "undockings"
            :> QueryParam "station-id" Int
            :> QueryParam "start-time" LocalTime
            :> QueryParam "end-time" LocalTime
            :> Get '[HTML] (DockingEventsHeader 'Undocking)
    } -> ComponentsAPI mode
  deriving stock Generic
