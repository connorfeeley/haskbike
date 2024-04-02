-- | Options for the debug commands.

module Haskbike.CLI.Options.Debug
     ( DebugMockServerOptions (..)
     , DebugCommand (..)
     , debugCommandParser
     ) where

import           Haskbike.CLI.Options.Command

import           Options.Applicative


-- | 'Debug' subcommands.
data DebugCommand where
  MockServer :: DebugMockServerOptions -> DebugCommand
  MiscStats  :: DebugCommand
  deriving (Show)

instance HasCommandDesc DebugCommand where
  commandDesc = "Miscellaneous debugging faciilities."

-- | Parser for 'DatabaseOptions'.
debugCommandParser :: Parser DebugCommand
debugCommandParser = hsubparser
  (  command "mock-server" (info (MockServer <$> debugMockServerOptionsParser) (progDesc "Run mock API server."))
  <> command "misc-stats"  (info (pure MiscStats)                              (progDesc "Print misc stats."))
  )

-- | Options for the 'MockServer' -> subcommand.
data DebugMockServerOptions where
  DebugMockServerOptions :: { debugMockSrvPort :: Int
                            } -> DebugMockServerOptions
  deriving (Show)

instance HasCommandDesc DebugMockServerOptions where
  commandDesc = "Miscellaneous debugging faciilities."

-- | Parser for 'DebugOptions'.
debugMockServerOptionsParser :: Parser DebugMockServerOptions
debugMockServerOptionsParser = DebugMockServerOptions
  <$> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 8082 -- default value
     <> help "Port to serve mock API on." )
