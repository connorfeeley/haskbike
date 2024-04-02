-- | Options for the debug commands.

module Haskbike.CLI.Options.Debug
     ( DebugMiscOptions (..)
     , debugMiscOptionsParser
     ) where

import           Haskbike.CLI.Options.Command

import           Options.Applicative


-- | Options for the 'Debug' command.
data DebugMiscOptions where
  DebugMiscOptions :: { } -> DebugMiscOptions
  deriving (Show)

instance HasCommandDesc DebugMiscOptions where
  commandDesc = "Miscellaneous debugging faciilities."

-- | Parser for 'DebugOptions'.
debugMiscOptionsParser :: Parser DebugMiscOptions
debugMiscOptionsParser = DebugMiscOptions
