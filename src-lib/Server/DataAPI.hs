{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.DataAPI
     ( DataAPI (..)
     ) where

import           Data.Time

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
    } -> DataAPI mode
  deriving stock Generic
