-- | CLI handler for visualization HTTP server.
module Haskbike.CLI.Server
     ( dispatchServer
     ) where

import           Colog                hiding ( getLogAction )

import qualified Data.Text            as T

import           Haskbike.AppEnv
import           Haskbike.CLI.Options
import           Haskbike.Server
import           Haskbike.ServerEnv

import           UnliftIO             ( liftIO )


-- | Dispatch CLI arguments to the visualization server.
dispatchServer :: ( HasEnv (Env AppM) m )
               => ServerOptions -> m ()
dispatchServer options = do
  env <- ask

  logInfo $ "Launching visualization web server on port " <> (T.pack . show) (optServerPort options)

  let serverEnv = ServerEnv { serverEnvBase         = env
                            , serverPort            = optServerPort options
                            , serverTimeoutSeconds  = 5 * 60
                            , serverGzipCompression = True
                            , serverMaxIntervals    = 20
                            , serverContactEmail    = "bikes@cfeeley.org"
                            , serverAssets          = optServerAssetLocation options
                            }

  logInfo $ "Gzip compression enabled: " <> (T.pack . show) (serverGzipCompression serverEnv)


  _ <- liftIO $ runServerAppM serverEnv serveVisualization
  pure ()
