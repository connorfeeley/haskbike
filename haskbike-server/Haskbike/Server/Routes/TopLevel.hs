{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains the top-level route definitions.

module Haskbike.Server.Routes.TopLevel
     ( API (..)
     , BikeShareExplorerAPI
     , routesLinks
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

routesLinks :: API (AsLink Link)
routesLinks = allFieldLinks
