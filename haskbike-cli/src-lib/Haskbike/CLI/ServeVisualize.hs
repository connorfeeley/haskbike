-- | CLI handler for visualization HTTP server.
module Haskbike.CLI.ServeVisualize
     ( dispatchVisualize
     ) where

import           Colog                hiding ( getLogAction )

import qualified Data.Text            as T

import           Haskbike.AppEnv
import           Haskbike.CLI.Options
import           Haskbike.Server
import           Haskbike.ServerEnv

import           Prelude              hiding ( log )

import           UnliftIO             ( liftIO )


-- | Dispatch CLI arguments to the visualization server.
dispatchVisualize :: (HasEnv (Env AppM) m)
                  => ServeVisualizeOptions -> m ()
dispatchVisualize options = do
  env <- ask

  log I $ "Launching visualization web server on port " <> (T.pack . show) (optServeVisualizePort options)

  let serverEnv = ServerEnv { serverEnvBase         = env
                            , serverPort            = optServeVisualizePort options
                            , serverTimeoutSeconds  = 5 * 60
                            , serverGzipCompression = True
                            , serverMaxIntervals    = 20
                            , serverContactEmail    = "bikes@cfeeley.org"
                            }

  log I $ "Gzip compression enabled: " <> (T.pack . show) (serverGzipCompression serverEnv)


  _ <- liftIO $ runServerAppM serverEnv serveVisualization
  pure ()
