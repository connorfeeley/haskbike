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
import           Control.Monad.Catch         ( throwM )
import           Control.Monad.Reader

import           Data.Function               ( (&) )

import           Haskbike.Server.Routes
import           Haskbike.ServerEnv

import           Network.HTTP.Types          ( Status )
import           Network.Wai
import           Network.Wai.Handler.Warp    as Warp
import           Network.Wai.Middleware.Gzip ( GzipFiles (..), GzipSettings (..), defaultGzipSettings, gzip )

import qualified Servant                     as S
import qualified Servant.Server.Generic      as S

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

  -- Run Warp/Wai server using specific settings.
  liftIO $ runSettings (serverSettings env) $ gzipM (serverApp env)


gzipSettings :: GzipSettings
gzipSettings =
  defaultGzipSettings
  { gzipFiles = GzipCompress
  , gzipSizeThreshold = 860
  }



serverSettings :: ServerEnv ServerAppM -> Settings
serverSettings env = defaultSettings
               & setPort (serverPort env)
               & setLogger serverLogger
               & setTimeout (serverTimeoutSeconds env)

serverLogger :: Request -> Status -> Maybe Integer -> IO ()
serverLogger req status code = putStrLn $ show req ++ " " ++ show status ++ " " ++ show code


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
