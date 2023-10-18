{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module contains the server API to visualize BikeShare data.

module Server
     ( serveVisualization
     ) where

import           AppEnv

import           Control.Monad.Except
import           Control.Monad.Reader

import           Network.Wai.Handler.Warp as Warp

import           Prelude                  ()
import           Prelude.Compat

import           Servant                  as S
import           Servant.Server.Generic

import           Server.Routes

import           ServerEnv

-- app :: Env AppM -> Application
-- app s = -- serve api $ hoistServer api (nt s) server
--   genericServeT (nt s) record

-- serveVisualization :: Int -> AppM ()
-- serveVisualization port = do
--   env <- ask
--   liftIO $ run port (app env)


app :: ServerEnv AppM -> Application
app s = -- serve api $ hoistServer api (nt s) server
  genericServeT (ntServerAppM s) record


serveVisualization :: Int -> ServerAppM ()
serveVisualization port = do
  senv <- ask
  let env = serverEnv senv
  liftIO $ run port (app senv)

