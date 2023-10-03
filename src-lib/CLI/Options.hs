{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}

-- | Command-line options.
module CLI.Options where

import           Database.Utils

import           Options.Applicative

import           Prelude             hiding ( log )


-- | Top-level options.
data Options where
  Options :: { optCommand         :: !Command
             , optVerbose         :: [Bool] -- ^ Verbosity flags.
             , optDatabase        :: String
             , optEnableMigration :: Bool
             } -> Options
  deriving (Show)

-- | Parser for 'Options'.
parseOptions :: Parser Options
parseOptions = Options
  <$> commandParser
  -- Support multiple verbosity flags.
  <*> many verboseFlag
  <*> strOption
      ( long "database"
     <> metavar "DATABASE"
     <> showDefault
     <> value dbnameProduction
     <> help "Target database name." )
  <*> switch
      ( long "enable-migrations"
     <> showDefault
     <> help "Perform database migrations." )
  where
    verboseFlag = flag' True -- Active when flag is present.
                  ( long "verbose"
                 <> short 'v'
                 <> help "Output verbosity (pass multiple times for more verbosity)."
                 <> showDefault)

-- | Top-level commands.
data Command where
  Poll  :: !PollOptions  -> Command
  Query :: !QueryOptions -> Command
  Reset :: !ResetOptions -> Command
  deriving (Show)

-- | Parser for 'Command'.
commandParser :: Parser Command
commandParser = subparser
  (  command "poll"   (info (Poll <$> pollOptionsParser)
                       (progDesc "Poll the API and insert new station status into database."))
  <> command "query" (info (Query <$> queryOptionsParser)
                      (progDesc "Query the database."))
  <> command "reset" (info (Reset <$> resetOptionsParser)
                      (progDesc "Reset the database. [DANGER]"))
  )

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

-- | Options for the 'Query' command.
data QueryOptions where
  QueryByStationId      :: { optStationId       :: Int }    -> QueryOptions
  QueryByStationName    :: { optStationName     :: String } -> QueryOptions
  deriving (Show)

-- | Parser for 'ResetOptions'.
queryOptionsParser :: Parser QueryOptions
queryOptionsParser =
  (QueryByStationId <$> parseStationId) <|> (QueryByStationName <$> parseStationName)

parseStationId :: Parser Int
parseStationId = option auto
  ( long "station-id"
 <> metavar "STATION_ID"
 <> showDefault
 <> value 7001
 <> help "Station ID to query." )

parseStationName :: Parser String
parseStationName = strOption
  ( long "station-name"
 <> metavar "STATION_NAME"
 <> help "Station name to query." )


-- | Options for the 'Poll' command.
data PollOptions where
  PollOptions :: { } -> PollOptions
  deriving (Show)

-- | Parser for 'ResetOptions'.
pollOptionsParser :: Parser PollOptions
pollOptionsParser = pure PollOptions
