{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Options for the database commands.

module Haskbike.CLI.Options.Database
     ( DatabaseCommand (..)
     , ExportOptions (..)
     , ResetOptions (..)
     , WarmOccupancyOptions (..)
     , databaseCommandParser
     ) where

import           Data.Kind                    ( Type )
import           Data.String                  ( IsString )
import           Data.Time

import           Haskbike.CLI.Options.Command

import           Options.Applicative


-- | Options for the 'Database' command.
data DatabaseCommand where
  Migrate       :: DatabaseCommand
  Export        :: !ExportOptions        -> DatabaseCommand
  Reset         :: !ResetOptions         -> DatabaseCommand
  WarmOccupancy :: !WarmOccupancyOptions -> DatabaseCommand
  deriving (Show)

instance HasCommandDesc DatabaseCommand where
  commandDesc = "Database operations."

-- | Parser for 'DatabaseOptions'.
databaseCommandParser :: Parser DatabaseCommand
databaseCommandParser = hsubparser
  (  command "migrate"        (info (pure Migrate)                          (progDesc "Run database migrations."))
  <> command "export"         (info (Export        <$> exportOptionsParser) (progDesc "Export data from the database."))
  <> command "reset"          (info (Reset         <$> resetOptionsParser)  (progDesc "Reset the database. [DANGER]"))
  <> command "warm-occupancy" (info (WarmOccupancy <$> warmOccupancyOptionsParser)
                                     (progDesc "Pre-fill station_occupancy cache for canonical ranges."))
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


-- * Options and parser for the 'WarmOccupancy' command.

-- | Options for the 'WarmOccupancy' command.
--
-- Pre-fills the @station_occupancy@ cache for one or more rolling windows
-- ending at the same anchor (defaults to the current UTC time, floored to
-- the bucket boundary used by 'queryStationOccupancyBucketed').
data WarmOccupancyOptions where
  WarmOccupancyOptions :: { optWarmSpansDays   :: ![Int]
                          , optWarmAnchorEnd   :: !(Maybe UTCTime)
                          , optWarmEmptyThresh :: !Int
                          , optWarmFullThresh  :: !Int
                          } -> WarmOccupancyOptions
  deriving (Show)

warmOccupancyOptionsParser :: Parser WarmOccupancyOptions
warmOccupancyOptionsParser = WarmOccupancyOptions
  <$> option (eitherReader parseSpans)
      ( long "spans"
     <> metavar "DAYS,DAYS,..."
     <> value [1, 7, 30]
     <> showDefault
     <> help "Comma-separated list of window sizes in days; one cache entry per span ending at the anchor time." )
  <*> optional (option auto
      ( long "end"
     <> metavar "UTCTIME"
     <> help "Anchor end time (UTC). Defaults to now. (format: YYYY-MM-DD HH:MM:SS UTC)" ))
  <*> option auto
      ( long "empty-thresh"
     <> metavar "N"
     <> value 0
     <> showDefault
     <> help "empty_thresh value to cache." )
  <*> option auto
      ( long "full-thresh"
     <> metavar "N"
     <> value 0
     <> showDefault
     <> help "full_thresh value to cache." )
  where
    parseSpans :: String -> Either String [Int]
    parseSpans s =
      let parts = filter (not . null) (splitOn ',' s)
      in traverse readInt parts
    splitOn c = foldr (\x acc -> if x == c then [] : acc else (x : head acc) : tail acc) [[]]
    readInt p = case reads p of
      [(n, "")] | n > 0 -> Right n
      _                 -> Left ("invalid span '" <> p <> "': expected positive integer")
