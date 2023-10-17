-- | CLI handler for visualization HTTP server.
module CLI.ServeVisualize
     ( dispatchVisualize
     ) where

import           API.Client
import           API.ClientLifted
import           API.Server.VisualizationData
import           API.Types

import           AppEnv

import           CLI.Options
import           CLI.QueryFormat

import           Colog

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader          ( asks, when )

import qualified Data.List                     as List
import           Data.Maybe                    ( fromMaybe )
import           Data.Text.Lazy                ( Text, pack, toStrict, unlines, unpack )

import           Database.BikeShare.Operations

import           Fmt

import           Formatting

import           Prelude                       hiding ( log, unlines )

import           Servant.Client

import           UnliftIO                      ( concurrently )


-- | Dispatch CLI arguments to the visualization server.
dispatchVisualize :: ServeVisualizeOptions -> AppM ()
dispatchVisualize options = do
  log I $ format "Launching visualization web server on port {}." (optServeVisualizePort options)

  liftIO $ serveVisualization (optServeVisualizePort options)
