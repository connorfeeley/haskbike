{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module contains the route definitions for the visualization server.

module Server.Routes
     ( Routes (..)
     , record
     ) where

import           AppEnv

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Proxy
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.BikeShare.Expressions

import           Prelude                                ()
import           Prelude.Compat

import           Servant                                as S
import           Servant.Server.Generic

import           Server.Data.StationStatusVisualization
import           Server.DataAPI
import           Server.Page.StationStatusVisualization
import           Server.VisualizationAPI

import           ServerEnv


-- | Route definitions.
data Routes route where
  Routes :: { _visualizationStationStatus :: route :- VisualizationAPI
            , _dataStationStatus          :: route :- DataAPI
            } -> Routes route
  deriving Generic

record :: Routes (AsServerT ServerAppM)
record =
  Routes { _visualizationStationStatus = stationStatusVisualizationPage
         , _dataStationStatus          = stationStatusData
         }

stationStatusData :: Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StationStatusVisualization]
stationStatusData = generateJsonDataSource

stationStatusVisualizationPage :: Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM StationStatusVisualizationPage
stationStatusVisualizationPage stationId startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifter into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  info <- liftIO $ runAppM appEnv (withPostgres $ runSelectReturningOne $ select $ infoByIdExpr [fromIntegral stationId])
  case info of
    Just info' ->
        pure StationStatusVisualizationPage { _statusVisPageStationInfo = info'
                                            , _statusVisPageStationId   = stationId
                                            , _statusVisPageTimeRange   = TimePair startTime endTime
                                            , _statusVisPageTimeZone    = tz
                                            , _statusVisPageCurrentUtc  = currentUtc
                                            }
    _ ->  throwError err404 { errBody = "Unknown station ID." }

routesLinks :: Routes (AsLink Link)
routesLinks = allFieldLinks

apiProxy :: Proxy (ToServantApi Routes)
apiProxy = genericApi (Proxy :: Proxy Routes)
