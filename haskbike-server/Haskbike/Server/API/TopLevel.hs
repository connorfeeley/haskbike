{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module contains the top-level route handlers.

module Haskbike.Server.API.TopLevel
     ( server
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
import           Haskbike.Server.Routes.TopLevel
import           Haskbike.Server.Routes.Visualization
import           Haskbike.ServerEnv

import           Servant
import           Servant.Server.Generic

import           UnliftIO


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
