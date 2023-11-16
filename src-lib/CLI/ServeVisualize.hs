-- | CLI handler for visualization HTTP server.
module CLI.ServeVisualize
     ( dispatchVisualize
     ) where

import           AppEnv

import           CLI.Options

import           Colog

import           Control.Monad.Reader ( ask )

import           Fmt

import           Prelude              hiding ( log )

import           Server

import           ServerEnv

import           UnliftIO             ( liftIO )


-- | Dispatch CLI arguments to the visualization server.
dispatchVisualize :: ServeVisualizeOptions -> AppM ()
dispatchVisualize options = do
  log I $ format "Launching visualization web server on port {}." (optServeVisualizePort options)
  env <- ask

  let serverEnv = ServerEnv { serverAppEnv       = env
                            , serverPort         = optServeVisualizePort options
                            , serverLogAction    = simpleMessageAction
                            , serverMaxIntervals = 20
                            }
  liftIO $ runServerAppM serverEnv serveVisualization
