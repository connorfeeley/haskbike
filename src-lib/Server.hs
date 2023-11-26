{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- | This module contains the server API to visualize BikeShare data.

module Server
     ( serveVisualization
     ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Function            ( (&) )

import           Network.Wai.Handler.Warp as Warp

import           Prelude                  ()
import           Prelude.Compat

import           Servant                  as S
import           Servant.Server.Generic

import           Server.Routes

import           ServerEnv


-- The 'app' function builds an application from a ServerEnv.
-- It uses the 'ntServerAppM' function to transform actions in the ServerAppM monad into actions in the Handler monad (which is what Servant's functions operate on).
-- This allows us to use our own environment throughout our application while still using Servant's functionality.
app :: ServerEnv ServerAppM -> Application
app s =
  genericServeT (ntServerAppM s) server
  -- 'genericServeT' is a function from the Servant library that serves an API using a generic server.
  -- In this case, ntServerAppM is used as a natural transformation on a generic server 'record'.

-- The 'serveVisualization' function is used to start the server at a specific port.
-- It first extracts the server environment using 'ask'.
-- Once we have the environment, we can start the server by using the 'run' function.
serveVisualization :: ServerAppM ()
serveVisualization = do
  env <- ask

  let _appEnv = serverAppEnv env

  -- Run Warp/Wai server using specific settings.
  liftIO $ runSettings (serverSettings env) (app env)

serverSettings :: ServerEnv ServerAppM -> Settings
serverSettings env = defaultSettings
               & setPort (serverPort env)
               & setTimeout (serverTimeoutSeconds env)
