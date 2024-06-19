{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Route definitions for the data API.

module Haskbike.Server.Routes.Data
     ( DataAPI (..)
     ) where

import           Data.ByteString.Lazy                                ( ByteString )
import           Data.Text                                           ( Text )
import           Data.Time

import           GHC.Generics                                        ( Generic )

import           Haskbike.Database.EventCounts
import           Haskbike.Database.Operations.Factors
import qualified Haskbike.Database.Tables.StationOccupancy           as DB
import           Haskbike.Server.Data.StationList
import           Haskbike.Server.Data.StationStatusVisualization
import           Haskbike.Server.Data.SystemInformationVisualization

import           Servant


-- | Data API endpoint.
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
    , stationListData :: mode :-
      "data" :>
        "station-list"
          :> QueryParam "time"     LocalTime
          :> Get '[JSON] [StationListRecord]
    , emptyFullData :: mode :-
      "data" :>
        "station-occupancy"
          :> QueryParam "start-time"   LocalTime
          :> QueryParam "end-time"     LocalTime
          :> Get '[JSON] [DB.EmptyFullRecord]
    } -> DataAPI mode
  deriving stock Generic
