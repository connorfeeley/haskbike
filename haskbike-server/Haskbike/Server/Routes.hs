{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains the route definitions for the visualization server.

module Haskbike.Server.Routes
     ( API (..)
     , BikeShareExplorerAPI
     , server
     ) where

import           Control.Monad.Catch                  ( MonadCatch )

import           Database.Beam

import           Haskbike.AppEnv
import           Haskbike.Server.API.Components
import           Haskbike.Server.API.Data
import           Haskbike.Server.API.Debug
import           Haskbike.Server.API.Static
import           Haskbike.Server.API.Visualization
import           Haskbike.Server.Page.IndexPage
import           Haskbike.Server.Page.SideMenu
import           Haskbike.Server.RobotsTXT
import           Haskbike.Server.Routes.Data
import           Haskbike.Server.Routes.Debug
import           Haskbike.Server.Routes.Static
import           Haskbike.Server.Routes.Visualization
import           Haskbike.ServerEnv

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic

import           UnliftIO


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

-- | The API handlers.
server :: ( WithServerEnv m, WithEnv (ServerEnv ServerAppM) m )
       => API (AsServerT m)
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
homePageHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m)
                => m (PureSideMenu IndexPage)
homePageHandler = do
  contactEmail <- getServerContactEmail
  sideMenu $
    IndexPage { _stationStatusLink = fieldLink pageForStation
              , _contactEmail      = contactEmail
              }
