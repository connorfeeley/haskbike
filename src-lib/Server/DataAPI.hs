{-# LANGUAGE DataKinds #-}
-- |

module Server.DataAPI where

import           Data.Time

import           Servant

import           Server.Data.StationStatusVisualization


type DataAPI =
  "data"
    :> "station-status"
      :> Capture "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
      :> Get '[JSON] [StationStatusVisualization]
