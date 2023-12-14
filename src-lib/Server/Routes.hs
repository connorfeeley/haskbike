{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module contains the route definitions for the visualization server.

module Server.Routes
     ( API (..)
     , BikeShareExplorerAPI
     , latestQueries
     , server
     ) where

import           AppEnv

import           Database.Beam

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic

import           Server.ComponentsAPI
import           Server.DataAPI
import           Server.DebugAPI
import           Server.Page.IndexPage
import           Server.Page.SideMenu
import           Server.RobotsTXT
import           Server.StaticAPI
import           Server.Utils
import           Server.VisualizationAPI

import           ServerEnv


data API mode where
  API :: { debugApi          :: mode :- NamedRoutes DebugAPI
         , home              :: mode :- Get '[HTML] (PureSideMenu IndexPage)
         , dataApi           :: mode :- NamedRoutes DataAPI
         , visualizationApi  :: mode :- NamedRoutes VisualizationAPI
         , componentsApi     :: mode :- NamedRoutes ComponentsAPI
         , static            :: mode :- NamedRoutes StaticAPI
         , robots            :: mode :- NamedRoutes RobotsAPI
         } -> API mode
  deriving stock Generic

type BikeShareExplorerAPI = NamedRoutes API

server :: API (AsServerT ServerAppM)
server = API { debugApi          = debugApiHandler
             , home              = homePageHandler
             , dataApi           = statusHandler
             , visualizationApi  = visualizationHandler
             , componentsApi     = componentsHandler
             , static            = staticHandler
             , robots            = robotsHandler
             }

-- * Handlers.

homePageHandler :: ServerAppM (PureSideMenu IndexPage)
homePageHandler = do
  _appEnv <- asks serverAppEnv
  sideMenu $
    IndexPage { _stationStatusLink = fieldLink pageForStation }

-- routesLinks :: API (AsLink Link)
-- routesLinks = allFieldLinks

-- apiProxy :: Proxy (ToServantApi API)
-- apiProxy = genericApi (Proxy :: Proxy API)

