{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- | This module contains the server API to visualize BikeShare data.

module Haskbike.Server
     ( serveVisualization
     ) where

import           Control.Conditional         ( condM )
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Function               ( (&) )

import           Haskbike.Server.Routes
import           Haskbike.ServerEnv

import           Network.Wai.Handler.Warp    as Warp
import           Network.Wai.Middleware.Gzip ( GzipFiles (..), GzipSettings (..), defaultGzipSettings, gzip )

import           Prelude                     ()
import           Prelude.Compat

import           Servant                     as S

import           UnliftIO


serveVisualization :: (Monad m, MonadReader (ServerEnv ServerAppM) m, MonadUnliftIO m, HasServerEnv (ServerEnv ServerAppM) m, HasEnv (ServerEnv ServerAppM) m)
                   => m ()
serveVisualization = do
  env <- ask

  -- Run with gzip compression if enabled.
  gzipM <- condM
    [ (return (serverGzipCompression env), return (gzip gzipSettings))
    , (return True,                        return id)
    ]

  serverHoisted <- withRunInIO $ \toIo ->
    pure $ hoistServer apiProxy (S.Handler . ExceptT . try . toIo) server

  -- Run Warp/Wai server using specific settings.
  liftIO $ runSettings (serverSettings env) $ gzipM (serve apiProxy serverHoisted)
  where
    apiProxy :: Proxy BikeShareExplorerAPI
    apiProxy = Proxy


gzipSettings :: GzipSettings
gzipSettings =
  defaultGzipSettings
  { gzipFiles = GzipCompress
  , gzipSizeThreshold = 860
  }


serverSettings :: ServerEnv m -> Settings
serverSettings env = defaultSettings
               & setPort (serverPort env)
               & setTimeout (serverTimeoutSeconds env)
