{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Options for the database commands.

module Haskbike.CLI.Options.Database
     ( DatabaseCommand (..)
     , ResetOptions (..)
     , databaseCommandParser
     ) where

import           Data.Time

import           Haskbike.CLI.Options.Command

import           Options.Applicative


-- | Options for the 'Database' command.
data DatabaseCommand where
  Export :: !ExportOptions -> DatabaseCommand
  Reset  :: !ResetOptions  -> DatabaseCommand
  deriving (Show)

instance HasCommandDesc DatabaseCommand where
  commandDesc = "Database operations. [DANGER]"

-- | Parser for 'DatabaseOptions'.
databaseCommandParser :: Parser DatabaseCommand
databaseCommandParser = hsubparser
  (  command "export"
    (info (Export <$> exportOptionsParser) (progDesc "Export data from the database."))
  <> command "reset"
    (info (Reset  <$> resetOptionsParser)  (progDesc "Reset the database. [DANGER]"))
  )


-- * Options and parser for the 'Reset' command.

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


-- * Options and parser for the 'Export' command.

-- | Options for the 'Export' command.
data ExportOptions where
  ExportOptions :: { optExportDir :: FilePath
                   , optExportDay :: Day
                   } -> ExportOptions
  deriving (Show)

exportOptionsParser :: Parser ExportOptions
exportOptionsParser = ExportOptions
  <$> strOption
      ( long "export-dir"
     <> short 'd'
     <> metavar "DIR"
     <> help "Directory to save exported JSON to." )
  <*> option auto
      ( long "day"
     <> short 't'
     <> metavar "DAY"
     <> help "Day to export data for." )
