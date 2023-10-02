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
             , optVerbose         :: Bool
             , optDatabase        :: String
             , optEnableMigration :: Bool
             } -> Options
  deriving (Show)

-- | Parser for 'Options'.
parseOptions :: Parser Options
parseOptions = Options
  <$> commandParser
  <*> switch
  -- TODO: implement verbose output.
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose output." )
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

-- | Top-level commands.
data Command where
  Poll  :: Command
  Query :: !QueryOptions -> Command
  Reset :: !ResetOptions -> Command
  deriving (Show)

-- | Parser for 'Command'.
commandParser :: Parser Command
commandParser = subparser
  (  command "poll"   (info (pure Poll)
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
  QueryOptions :: { optStationId :: String -- TODO: convert to Int
                  } -> QueryOptions
  deriving (Show)

-- | Parser for 'ResetOptions'.
queryOptionsParser :: Parser QueryOptions
queryOptionsParser = QueryOptions
  <$> strOption
      ( long "station-id"
     <> metavar "STATION_ID"
     <> showDefault
     <> value "7001"
     <> help "Station ID to query." )
