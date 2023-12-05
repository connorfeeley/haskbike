{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.DataAPI
     ( DataAPI (..)
     ) where

import           Data.ByteString.Lazy                       ( ByteString )
import           Data.Text                                  ( Text )
import           Data.Time

import           Database.BikeShare.Operations.Dockings
import           Database.BikeShare.Operations.Factors

import           GHC.Generics                               ( Generic )

import           Servant

import           Server.Data.StationStatusVisualization
import           Server.Data.SystemInformationVisualization


data DataAPI mode where
  DataAPI ::
    { dataForStation :: mode :-
      "data" :>
        "station-status"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [StationStatusVisualization]
    , integralsForStation :: mode :-
      "data" :>
        "station-status" :> "integral"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [StatusIntegral]
    , factorsForStation :: mode :-
      "data" :>
        "station-status" :> "factor"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [StatusFactor]
    , systemInfoData :: mode :-
      "data" :>
        "system-information"
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [SystemInformationCountVisualization]
    , performanceCsv :: mode :-
      "data" :>
        "system-status" :> "performance" :> "csv"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] ByteString)
    , dockingEventsData :: mode :-
      "data" :>
        "events" :> "docking"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [DockingEventsCount]
    , chargingEventsData :: mode :-
      "data" :>
        "events" :> "charging"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [ChargingEvent]
    } -> DataAPI mode
  deriving stock Generic
