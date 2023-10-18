{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.DataAPI
     ( DataRoutes (..)
     ) where

import           Data.Time

import           GHC.Generics                           ( Generic )

import           Servant

import           Server.Data.StationStatusVisualization


data DataRoutes mode where
  DataRoutes ::
    { dataForStation :: mode :-
      "station-status"
        :> Capture "station-id" Int
        :> QueryParam "start-time" LocalTime
        :> QueryParam "end-time" LocalTime
        :> Get '[JSON] [StationStatusVisualization]
    } -> DataRoutes mode
  deriving stock Generic
