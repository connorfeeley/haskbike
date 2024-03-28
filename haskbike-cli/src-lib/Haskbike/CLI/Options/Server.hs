-- | Options for the server commands.

module Haskbike.CLI.Options.Server
     ( ServerOptions (..)
     , serverOptionsParser
     ) where

import           Haskbike.CLI.Options.Command

import           Options.Applicative


data ServerOptions where
  ServerOptions :: { optServerPort :: Int
                   } -> ServerOptions
  deriving (Show, Read)

instance HasCommandDesc ServerOptions where
  commandDesc = "Visualization HTTP server."

serverOptionsParser :: Parser ServerOptions
serverOptionsParser = ServerOptions
  <$> argument auto
  ( metavar "HTTP_PORT"
 <> showDefault
 <> value 8081
 <> help "Port to serve visualization interface on." )
