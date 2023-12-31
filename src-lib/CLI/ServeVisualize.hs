-- | CLI handler for visualization HTTP server.
module CLI.ServeVisualize
     ( dispatchVisualize
     ) where

import           AppEnv

import           CLI.Options

import           Colog

import qualified Data.Text   as T

import           Prelude     hiding ( log )

import           Server

import           ServerEnv

import           UnliftIO    ( liftIO )


-- | Dispatch CLI arguments to the visualization server.
dispatchVisualize :: ServeVisualizeOptions -> AppM ()
dispatchVisualize options = do
  log I $ "Launching visualization web server on port " <> (T.pack . show) (optServeVisualizePort options)
  env <- ask

  let serverEnv = ServerEnv { serverAppEnv         = env
                            , serverPort           = optServeVisualizePort options
                            , serverTimeoutSeconds = 5 * 60
                            , serverLogAction      = adaptLogAction (envLogAction env)
                            , serverMaxIntervals   = 20
                            , serverContactEmail   = "bikes@cfeeley.org"
                            }
  liftIO $ runServerAppM serverEnv serveVisualization
