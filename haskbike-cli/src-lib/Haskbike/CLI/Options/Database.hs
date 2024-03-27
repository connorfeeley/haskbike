-- | Options for the database commands.

module Haskbike.CLI.Options.Database
     ( DatabaseCommand (..)
     , ResetOptions (..)
     , databaseCommandParser
     , resetOptionsParser
     ) where

import           Options.Applicative


-- | Options for the 'Database' command.
data DatabaseCommand where
  Reset  :: !ResetOptions -> DatabaseCommand
  deriving (Show)

-- | Options for the 'Reset' command.
data ResetOptions where
  ResetOptions :: { optResetOnly :: Bool
                  , optTest      :: Bool
                  } -> ResetOptions
  deriving (Show)

-- | Parser for 'ResetOptions'.
resetOptionsParser :: Parser ResetOptions
resetOptionsParser = ResetOptions
  <$> switch
      ( long "reset-only"
     <> help "Only reset, don't insert new data." )
  <*> switch
      ( long "test"
     <> help "Run the command in test mode." )

-- | Parser for 'DatabaseOptions'.
databaseCommandParser :: Parser DatabaseCommand
databaseCommandParser = hsubparser
  (  command "reset"
    (info (Reset <$> resetOptionsParser) (progDesc "Reset the database. [DANGER]"))
  <> command "reset"
    (info (Reset <$> resetOptionsParser) (progDesc "Reset the database. [DANGER]"))
  )
