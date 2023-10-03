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
  QueryByStationName    :: { optStationName     :: MatchMethod String
                           } -> QueryOptions
  deriving (Show)

data MatchMethod a =
    ExactMatch a
  | PrefixMatch a
  | SuffixMatch a
  | WildcardMatch a
  deriving (Show, Functor)

-- | Unwrap a 'MatchMethod'.
unMatchMethod :: MatchMethod a -> a
unMatchMethod (ExactMatch a)    = a
unMatchMethod (PrefixMatch a)   = a
unMatchMethod (SuffixMatch a)   = a
unMatchMethod (WildcardMatch a) = a

-- | Parser for 'ResetOptions'.
queryOptionsParser :: Parser QueryOptions
queryOptionsParser = subparser $
  command "id"   (info (QueryByStationId   <$> parseStationId)   (progDesc "Query by station ID.")) <>
  command "name" (info (QueryByStationName <$> parseStationName) (progDesc "Query by station name."))

parseStationId :: Parser Int
parseStationId = argument auto
  ( metavar "STATION_ID"
 <> showDefault
 <> value 7001
 <> help "Station ID to query." )

-- | Parser for variants of 'MatchMethod' parameterized over 'String'.
parseStationName :: Parser (MatchMethod String)
parseStationName = exact <|> prefix <|> suffix <|> wildcard where
  exact = ExactMatch <$> strOption
    ( long "exact"
   <> metavar "STATION_NAME"
   <> help "Query for an exact match of the station name." )
  prefix = PrefixMatch <$> strOption
    ( long "prefix"
   <> metavar "PREFIX"
   <> help "Query stations where STATION_NAME appears at the start of the name." )
  suffix = SuffixMatch <$> strOption
    ( long "suffix"
   <> metavar "SUFFIX"
   <> help "Query stations where STATION_NAME appears at the end of the name." )
  wildcard = WildcardMatch <$> argument str
    ( metavar "STATION_NAME"
   <> help "Query stations where STATION_NAME appears anywhere in the name." )

-- | Options for the 'Poll' command.
data PollOptions where
  PollOptions :: { } -> PollOptions
  deriving (Show)

-- | Parser for 'ResetOptions'.
pollOptionsParser :: Parser PollOptions
pollOptionsParser = pure PollOptions
