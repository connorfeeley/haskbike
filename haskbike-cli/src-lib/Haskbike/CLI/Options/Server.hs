-- | Options for the server commands.

module Haskbike.CLI.Options.Server
     ( ServeVisualizeOptions (..)
     , serveVisualizationParser
     ) where

import           Options.Applicative


data ServeVisualizeOptions where
  ServeVisualizeOptions :: { optServeVisualizePort :: Int
                           } -> ServeVisualizeOptions
  deriving (Show, Read)

serveVisualizationParser :: Parser ServeVisualizeOptions
serveVisualizationParser = ServeVisualizeOptions
  <$> argument auto
  ( metavar "HTTP_PORT"
 <> showDefault
 <> value 8081
 <> help "Port to serve visualization interface on." )
