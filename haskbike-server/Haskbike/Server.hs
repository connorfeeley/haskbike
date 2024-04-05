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


import           Control.Conditional                       ( condM )
import           Control.Monad.Catch                       ( throwM )
import           Control.Monad.Reader

import           Data.Default                              ( def )
import           Data.Function                             ( (&) )

import           Haskbike.Server.Routes
import           Haskbike.ServerEnv

import           Network.Wai                               ( Middleware )
import           Network.Wai.Handler.Warp                  as Warp
import           Network.Wai.Middleware.Gzip               ( GzipFiles (..), GzipSettings (..), defaultGzipSettings,
                                                             gzip )
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON

import qualified Servant                                   as S
import qualified Servant.Server.Generic                    as S

import           UnliftIO


serveVisualization :: ( Monad m, MonadReader (ServerEnv ServerAppM) m, MonadUnliftIO m
                      , HasServerEnv (ServerEnv ServerAppM) m, HasEnv (ServerEnv ServerAppM) m
                      ) => m ()
serveVisualization = do
  env <- ask

  -- Run with gzip compression if enabled.
  gzipM <- condM
    [ (return (serverGzipCompression env), return (gzip gzipSettings))
    , (return True,                        return id)
    ]

  logger <- liftIO mkServerLogger

  -- Run Warp/Wai server using specific settings.
  liftIO $ runSettings (serverSettings env) (logger (gzipM (serverApp env)))


-- | Log requests as JSON with response headers.
mkServerLogger :: IO Middleware
mkServerLogger = mkRequestLogger def { outputFormat = CustomOutputFormatWithDetailsAndHeaders formatAsJSONWithHeaders }


gzipSettings :: GzipSettings
gzipSettings =
  defaultGzipSettings
  { gzipFiles = GzipCompress
  , gzipSizeThreshold = 860
  }



serverSettings :: ServerEnv ServerAppM -> Settings
serverSettings env = defaultSettings
               & setPort (serverPort env)
               & setTimeout (serverTimeoutSeconds env)


-- | Natural transformation between server monad and Servant 'Handler' monad.
serverNt :: ServerEnv ServerAppM -> ServerAppM a -> S.Handler a
serverNt env action =
  liftIO $
    runReaderT (unServerAppM action) env `catch` exceptionHandler
  where
    exceptionHandler :: SomeException -> IO a
    exceptionHandler ex = throwM (servantErrFromEx ex)

servantErrFromEx :: SomeException -> S.ServerError
servantErrFromEx _ex = S.err500 { S.errBody = "Internal server error" }

serverApp :: ServerEnv ServerAppM -> S.Application
serverApp state = S.genericServeT (serverNt state) server
