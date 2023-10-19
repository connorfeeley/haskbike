{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module contains the route definitions for the visualization server.

module Server.Routes
     ( API (..)
     , BikeShareExplorerAPI
     , server
     ) where

import           AppEnv

import           Colog

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Proxy
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.BikeShare
import           Database.BikeShare.Expressions

import           Fmt

import           Servant
import           Servant.Server.Generic

import           Server.Data.StationStatusVisualization
import           Server.DataAPI
import           Server.Page.StationStatusVisualization
import           Server.VisualizationAPI

import           ServerEnv

data API mode where
  API :: { version           :: mode :- "version" :> Get '[JSON] Version
         , stationData       :: mode :- NamedRoutes DataAPI
         , visualizationPage :: mode :- NamedRoutes VisualizationAPI
         , static            :: mode :- NamedRoutes StaticAPI
         } -> API mode
  deriving stock Generic

type Version = ((String, String), (String, String)) -- This will do for the sake of example.

type BikeShareExplorerAPI = NamedRoutes API

versionHandler :: ServerAppM Version
versionHandler = pure (("version", "0.0.1"), ("git-version", "0.0.1"))

server ::  API (AsServerT ServerAppM)
server = API { version = versionHandler
             , stationData = statusHandler
             , visualizationPage = visualizationHandler
             , static = staticHandler
             }

-- * Serve static files.

data StaticAPI mode where
  StaticAPI ::
    { staticApi :: mode :- "static" :> Raw
    } -> StaticAPI mode
  deriving stock Generic

-- * Handlers.

statusHandler :: DataAPI (AsServerT ServerAppM)
statusHandler =  DataAPI { dataForStation = stationStatusData }

staticHandler :: StaticAPI (AsServerT ServerAppM)
staticHandler =  StaticAPI $ serveDirectoryWebApp "static-files"

visualizationHandler :: VisualizationAPI (AsServerT ServerAppM)
visualizationHandler = VisualizationAPI { pageForStation = stationStatusVisualizationPage }

stationStatusData :: Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StationStatusVisualization]
stationStatusData stationId startTime endTime = do
  logInfo $ format "Creating JSON payload for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime
  generateJsonDataSource stationId startTime endTime


stationStatusVisualizationPage :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM StationStatusVisualizationPage
stationStatusVisualizationPage (Just stationId) startTime endTime = do
  logInfo $ format "Rendering page for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifter into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  info <- liftIO $ runAppM appEnv (withPostgres $ runSelectReturningOne $ select $ infoByIdExpr [fromIntegral stationId])
  case info of
    Just info' -> do
      logInfo $ "Matched station information: " <> (info' ^. infoName)
      logInfo $ "Static path: " <> toUrlPiece (fieldLink staticApi)
      pure StationStatusVisualizationPage { _statusVisPageStationInfo = info'
                                          , _statusVisPageStationId   = stationId
                                          , _statusVisPageTimeRange   = TimePair startTime endTime
                                          , _statusVisPageTimeZone    = tz
                                          , _statusVisPageCurrentUtc  = currentUtc
                                          , _statusVisPageDataLink    = fieldLink dataForStation stationId startTime endTime
                                          , _statusVisPageStaticLink  = fieldLink staticApi
                                          }
    _ ->  throwError err404 { errBody = "Unknown station ID." }
stationStatusVisualizationPage Nothing _ _ =
  throwError err404 { errBody = "Station ID parameter is required." }


-- routesLinks :: API (AsLink Link)
-- routesLinks = allFieldLinks

-- apiProxy :: Proxy (ToServantApi API)
-- apiProxy = genericApi (Proxy :: Proxy API)
