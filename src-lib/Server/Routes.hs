{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

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


-- | The API type.
data API mode where
  API :: { debugApi         :: mode :- NamedRoutes DebugAPI
         , homePage         :: mode :- Get '[HTML] (PureSideMenu IndexPage)
         , dataApi          :: mode :- NamedRoutes DataAPI
         , visualizationApi :: mode :- NamedRoutes VisualizationAPI
         , componentsApi    :: mode :- NamedRoutes ComponentsAPI
         , static           :: mode :- NamedRoutes StaticAPI
         , robotsTxtFile    :: mode :- NamedRoutes RobotsAPI
         } -> API mode
  deriving stock Generic

type BikeShareExplorerAPI = NamedRoutes API

-- routesLinks :: API (AsLink Link)
-- routesLinks = allFieldLinks

-- apiProxy :: Proxy (ToServantApi API)
-- apiProxy = genericApi (Proxy :: Proxy API)

-- | The API handlers.
server :: API (AsServerT ServerAppM)
server = API { debugApi         = debugApiHandler
             , homePage         = homePageHandler
             , dataApi          = statusHandler
             , visualizationApi = visualizationHandler
             , componentsApi    = componentsHandler
             , static           = staticHandler
             , robotsTxtFile    = robotsHandler
             }

-- * Handlers.

-- | Handler render the home page.
homePageHandler :: ServerAppM (PureSideMenu IndexPage)
homePageHandler = do
  _appEnv <- asks serverAppEnv
  sideMenu $
    IndexPage { _stationStatusLink = fieldLink pageForStation }

