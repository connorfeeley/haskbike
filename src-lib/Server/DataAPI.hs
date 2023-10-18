{-# LANGUAGE DataKinds #-}
-- |

module Server.DataAPI
     ( DataAPI
     , dataAPI
     ) where

import           Data.Time

import           Servant

import           Server.Data.StationStatusVisualization


type DataAPI =
  "data"
    :> "station-status"
      :> Capture "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
      :> Get '[JSON] [StationStatusVisualization]

dataAPI :: Proxy DataAPI
dataAPI = Proxy @DataAPI
