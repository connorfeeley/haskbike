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


import           Colog                                     ( HasLog, logError )

import           Control.Conditional                       ( condM )
import           Control.Exception                         ( fromException )
import           Control.Monad.Catch                       ( MonadThrow )
import           Control.Monad.Reader

import           Data.Aeson
import qualified Data.ByteString.Builder                   as Builder
import           Data.Data                                 ( Proxy (..) )
import           Data.Default                              ( def )
import           Data.Function                             ( (&) )
import           Data.String.Conversions                   ( cs )
import qualified Data.String.Conversions.Monomorphic       as SCM
import qualified Data.Text                                 as T

import           Haskbike.Server.API.TopLevel
import           Haskbike.Server.Middleware.Timing         ( mkRequestTimingMiddleware )
import           Haskbike.Server.Page.ErrorPage
import           Haskbike.ServerEnv

import           Lucid

import qualified Network.HTTP.Types                        as H
import           Network.Wai
import           Network.Wai.Handler.Warp                  as Warp
import           Network.Wai.Middleware.Gzip               ( GzipFiles (..), GzipSettings (..), defaultGzipSettings,
                                                             gzip )
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON

import qualified Servant                                   as S
import           Servant                                   ( throwError )
import qualified Servant.API.ContentTypes                  as S
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
  (_reqIdKey, timingM) <- liftIO mkRequestTimingMiddleware

  -- Run Warp/Wai server using specific settings.
  -- Middleware order (outermost first): timing -> logger -> gzip -> app.
  -- 'timingM' is outermost so its measurement covers the full handler time.
  liftIO $ runSettings (serverSettings env) (timingM (logger (gzipM (serverApp env))))


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
serverSettings env = do
  defaultSettings
    & setPort (serverPort env)
    & setTimeout (serverTimeoutSeconds env)
    & setOnExceptionResponse (onExceptionResponse (serverContactEmail env))

onExceptionResponse :: Exception e => T.Text -> e -> Response
onExceptionResponse email ex = do
  responseBuilder H.internalServerError500
                  [(H.hContentType, "text/html; charset=utf-8")]
                  renderErrorPage
  where
    renderErrorPage = Builder.stringUtf8 . SCM.fromLazyByteString . renderBS . toHtml $ ErrorPage email ex

-- | Natural transformation between server monad and Servant 'Handler' monad.
serverNt :: ServerEnv ServerAppM -> ServerAppM a -> S.Handler a
serverNt env action =
  liftIO $
    runReaderT (unServerAppM action) env `catch` onError
  where
    onError x = runReaderT (unServerAppM (exceptionHandler x)) env

-- | Catch handler for the natural transformation. Distinguishes intentional
-- 'ServerError's (thrown via 'throwError', e.g. @err404 "Unknown station ID."@)
-- from genuine runtime exceptions:
--
--   * 'ServerError' is re-thrown unchanged so its status / body / headers
--     reach the client as the handler intended.
--   * Anything else is logged and downgraded to a 500 with a generic body.
--
-- Without this distinction the catch-all rewrites every 4xx into a 500,
-- which is what was masking the @viz-station-status@ "Unknown station ID."
-- 404s as opaque server errors.
exceptionHandler :: (MonadReader env m, HasLog env Message m, MonadThrow m, HasServerEnv env m) => SomeException -> m b
exceptionHandler ex
  | Just (servantErr :: S.ServerError) <- fromException ex = throwError servantErr
  | otherwise = do
      email <- getServerContactEmail
      logError $ "Caught unexpected exception: " <> T.pack (show ex)
      throwError (servantErrFromEx email ex)

servantErrFromEx :: Exception e => T.Text -> e -> S.ServerError
servantErrFromEx email _ex =
  S.err500 { S.errBody    = "Internal server error. Contact " <> cs email <> " if the issue persists."
           , S.errHeaders = [("Content-Type", "application/text")]
           }

customFormatters :: S.ErrorFormatters
customFormatters = S.defaultErrorFormatters
  { S.bodyParserErrorFormatter = customFormatter
  , S.notFoundErrorFormatter = notFoundFormatter
  }

notFoundFormatter :: S.NotFoundErrorFormatter
notFoundFormatter req =
  S.err404 { S.errBody = cs $ "Path not found: " <> rawPathInfo req }

customFormatter :: S.ErrorFormatter
customFormatter tr req err =
  let
    -- aeson Value which will be sent to the client
    value = object ["combinator" .= show tr, "error" .= err]
    -- Accept header of the request
    accH = S.getAcceptHeader req
  in
  -- handleAcceptH is Servant's function that checks whether the client can accept a
  -- certain message type.
  -- In this case we call it with "Proxy '[JSON]" argument, meaning that we want to return a JSON.
  case S.handleAcceptH (Proxy :: Proxy '[S.JSON]) accH value of
    -- If client can't handle JSON, we just return the body the old way
    Nothing -> S.err400 { S.errBody = cs err }
    -- Otherwise, we return the JSON formatted body and set the "Content-Type" header.
    Just (ctypeH, body) -> S.err400
      { S.errBody = body
      , S.errHeaders = [("Content-Type", cs ctypeH)]
      }

serverApp :: ServerEnv ServerAppM -> S.Application
serverApp state = S.genericServeTWithContext (serverNt state) server (customFormatters S.:. S.EmptyContext)
