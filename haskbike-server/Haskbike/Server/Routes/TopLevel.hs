{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains the top-level route definitions.

module Haskbike.Server.Routes.TopLevel
     ( API (..)
     , BikeShareExplorerAPI
     , routesLinks
     ) where

import           Database.Beam

import           Haskbike.Server.API.Components
import           Haskbike.Server.Page.IndexPage
import           Haskbike.Server.Page.SideMenu
import           Haskbike.Server.RobotsTXT
import           Haskbike.Server.Routes.Data
import           Haskbike.Server.Routes.Debug
import           Haskbike.Server.Routes.Static
import           Haskbike.Server.Routes.Visualization

import           Servant
import           Servant.HTML.Lucid


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
