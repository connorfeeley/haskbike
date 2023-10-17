-- | CLI handler for visualization HTTP server.
module CLI.ServeVisualize
     ( dispatchVisualize
     ) where

import           API.Server

import           AppEnv

import           CLI.Options

import           Colog

import           Fmt

import           Prelude     hiding ( log )


-- | Dispatch CLI arguments to the visualization server.
dispatchVisualize :: ServeVisualizeOptions -> AppM ()
dispatchVisualize options = do
  log I $ format "Launching visualization web server on port {}." (optServeVisualizePort options)

  serveVisualization (optServeVisualizePort options)
