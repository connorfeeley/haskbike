{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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

-- | Natural transformation function for AppM monad
nt :: Env AppM -> AppM a -> Handler a
nt s a =
  let r = runReaderT (unAppM a) s
  in liftIO r


app :: Env AppM -> Application
app s = -- serve api $ hoistServer api (nt s) server
  genericServeT (nt s) record


serveVisualization :: Int -> AppM ()
serveVisualization port = do
  env <- ask
  liftIO $ run port (app env)

