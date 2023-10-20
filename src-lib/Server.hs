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
  -- 'ask' is from Control.Monad.Reader class. It fetches ServerEnv.

  let _appEnv = serverAppEnv env
  -- The 'serverAppEnv' function gives access to the underlying 'AppM' environment

  liftIO $ run (serverPort env) (app env)
  -- 'run' is a function from Network.Wai.Handler.Warp that runs the application we have built on a specific port.
  -- 'liftIO' elevates the IO action to run inside the ServerAppM monad.
  -- Note that 'app env' is passed as an argument to 'run'. It tells 'run' to use the application built from ServerEnv using 'app'.
