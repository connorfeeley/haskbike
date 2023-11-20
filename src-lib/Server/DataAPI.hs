{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.DataAPI
     ( DataAPI (..)
     ) where

import           Data.Time

import           Database.BikeShare.Operations.Factors

import           GHC.Generics                           ( Generic )

import           Servant

import           Server.Data.StationStatusVisualization


data DataAPI mode where
  DataAPI ::
    { dataForStation :: mode :-
      "data" :>
        "station-status"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [StationStatusVisualization]
    , integralForStation :: mode :-
      "data" :>
        "station-status" :> "integral"
          :> QueryParam "station-id" Int
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[JSON] [StatusIntegral]
    } -> DataAPI mode
  deriving stock Generic
