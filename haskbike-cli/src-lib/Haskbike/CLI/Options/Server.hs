-- | Options for the server commands.

module Haskbike.CLI.Options.Server
     ( ServerOptions (..)
     , serverOptionsParser
     ) where

import           Haskbike.CLI.Options.Command
import           Haskbike.Server.ExternalAssets

import           Options.Applicative


data ServerOptions where
  ServerOptions :: { optServerPort          :: Int
                   , optServerAssetLocation :: ExternalAssetLocation
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
  <*> option auto
  ( long "assets-location"
 <> showDefault
 <> value ExternalAssetCDN
 <> help ("Serve either vendored assets or from CDN. Allowed values: " <> show allowedValues))
  where
    allowedValues = [externalAssetVendored, externalAssetCDN]
