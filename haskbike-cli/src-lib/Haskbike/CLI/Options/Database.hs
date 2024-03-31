{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Options for the database commands.

module Haskbike.CLI.Options.Database
     ( DatabaseCommand (..)
     , ExportOptions (..)
     , ResetOptions (..)
     , databaseCommandParser
     ) where

import           Data.Kind                    ( Type )
import           Data.String                  ( IsString )
import           Data.Time

import           Haskbike.CLI.Options.Command

import           Options.Applicative


-- | Options for the 'Database' command.
data DatabaseCommand where
  Migrate :: DatabaseCommand
  Export  :: !ExportOptions -> DatabaseCommand
  Reset   :: !ResetOptions  -> DatabaseCommand
  deriving (Show)

instance HasCommandDesc DatabaseCommand where
  commandDesc = "Database operations."

-- | Parser for 'DatabaseOptions'.
databaseCommandParser :: Parser DatabaseCommand
databaseCommandParser = hsubparser
  (  command "migrate" (info (pure Migrate)                   (progDesc "Run database migrations."))
  <> command "export"  (info (Export <$> exportOptionsParser) (progDesc "Export data from the database."))
  <> command "reset"   (info (Reset  <$> resetOptionsParser)  (progDesc "Reset the database. [DANGER]"))
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
  ExportOptions :: { optExportDir       :: FilePath
                   , optExportStationId :: Maybe Int
                   , optExportStartDay  :: Day
                   , optExportEndDay    :: Day
                   } -> ExportOptions
  deriving (Show)

exportOptionsParser :: Parser ExportOptions
exportOptionsParser = ExportOptions
  <$> option str
      ( long "export-dir"
     <> short 'd'
     <> metavar "DIR"
     <> value "test/dumps/" -- default value
     <> help "Directory to save exported JSON to." )
  <*> optional (option auto
      ( long "station-id"
     <> metavar "STATION_ID"
     <> help "Limit exported data to a single station." ))
  <*> argument auto ( metavar "START_DAY" <> helpWithFormat "Starting day to export data for." )
  <*> argument auto ( metavar "END_DAY"   <> helpWithFormat "End day to export data for." )
  where
    formatHelp :: IsString a => a
    formatHelp = "(format: YYYY-MM-DD)"

    helpWithFormat :: forall (f :: Type -> Type) a. String -> Mod f a
    helpWithFormat text = help (text <> " " <> formatHelp)
