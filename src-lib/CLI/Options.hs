{-# LANGUAGE RecordWildCards #-}

-- | Command-line options.
module CLI.Options where

import           Data.Time

import           Database.BikeShare.Utils

import           Options.Applicative

import           Prelude                  hiding ( log )


-- | Top-level options.
data Options where
  Options :: { optVersion         :: !Bool   -- ^ Flag for version (derived from cabal file and git revision).
             , optCommand         :: !Command
             , optVerbose         :: ![Bool] -- ^ Verbosity flags.
             , optLogDatabase     :: !Bool   -- ^ If database queries should be logged.
             , optLogRichOutput   :: !Bool   -- ^ If log output should use rich messages.
             , optLogBuffering    :: !Bool   -- ^ If log output should be buffered.
             , optDatabase        :: !String
             , optEnableMigration :: !Bool
             } -> Options
  deriving (Show)

-- | Parser for 'Options'.
parseOptions :: Parser Options
parseOptions = Options
  <$> versionFlag
  <*> commandParser
  -- Support multiple verbosity flags.
  <*> many verboseFlag
  <*> logDatabase
  <*> logPlain
  <*> logBuffering
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
    versionFlag = switch
                 (long "version"
               <> short 'V'
               <> help "Print version (from cabal file and git revision).")
    verboseFlag = flag' True -- Active when flag is present.
                 (long "verbose"
               <> short 'v'
               <> help "Output verbosity (pass multiple times for more verbosity)."
               <> showDefault)
    logDatabase = switch
                 (long "log-database"
               <> showDefault
               <> help "Log database queries." )
    logPlain = flag True False
              (long "plain"
            <> short 'p'
            <> help "Plain logging output, instead of rich messages."
            <> showDefault)
    logBuffering = flag True False
             (long "unbuffered"
           <> help "Disable stdout and stderr bufferring."
           <> showDefault)

-- | Top-level commands.
data Command where
  Poll           :: !PollOptions           -> Command
  Query          :: !QueryOptions          -> Command
  QueryApi       :: Command
  Events         :: !EventsOptions         -> Command
  ServeVisualize :: !ServeVisualizeOptions -> Command
  DebugMisc      :: !DebugMiscOptions      -> Command
  Reset          :: !ResetOptions          -> Command
  deriving (Show)

-- | Parser for 'Command'.
commandParser :: Parser Command
commandParser = hsubparser
  (  command "poll"   (info (Poll <$> pollOptionsParser)
                       (progDesc "Poll the API and insert new station status into database."))
  <> command "query" (info (Query <$> queryOptionsParser)
                      (progDesc "Query the database."))
  <> command "events" (info (Events <$> eventsOptionsParser)
                      (progDesc "Docking and undocking events."))
  <> command "visualize" (info (ServeVisualize <$> serveVisualizationParser)
                      (progDesc "Visualization HTTP server."))
  <> command "debug" (info (DebugMisc <$> debugMiscOptionsParser)
                      (progDesc "Miscellaneous debugging faciilities."))
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
data QueryOptions =
  QueryOptions { optRefresh :: Bool
               , optQueryBy :: QueryMethod
               } deriving (Show)

data QueryMethod =
    QueryByStationId Int
  | QueryByStationName (MatchMethod String)
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


-- | Parser for 'QueryOptions'.
queryOptionsParser :: Parser QueryOptions
queryOptionsParser = QueryOptions
  <$> flag True False (long "no-refresh" <> help "Don't refresh the data from the API.")
  <*> hsubparser (queryByIdParser <> queryByNameParser)
  where
    queryByIdParser   = command "id"   (info (QueryByStationId   <$> parseStationId)   (progDesc "Query by station ID."))
    queryByNameParser = command "name" (info (QueryByStationName <$> parseStationName) (progDesc "Query by station name."))

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

data ServeVisualizeOptions where
  ServeVisualizeOptions :: { optServeVisualizePort :: Int
                           } -> ServeVisualizeOptions
  deriving (Show, Read)

serveVisualizationParser :: Parser ServeVisualizeOptions
serveVisualizationParser = ServeVisualizeOptions
  <$> argument auto
  ( metavar "HTTP_PORT"
 <> showDefault
 <> value 8081
 <> help "Port to serve visualization interface on." )

-- | Options for the 'Debug' command.
data DebugMiscOptions where
  DebugMiscOptions :: { optFoo :: Bool -- TODO: this is just a placeholder.
                      } -> DebugMiscOptions
  deriving (Show)

-- | Parser for 'DebugOptions'.
debugMiscOptionsParser :: Parser DebugMiscOptions
debugMiscOptionsParser = DebugMiscOptions
  <$> switch
      ( long "foo"
     <> help "Foo. Foo foo foo bar." )

-- | Options for the 'Events' command.
-- data EventsOptions where
--   EventsCounts  :: !(Maybe Int)         -> EventsOptions
--   EventsRange   :: !EventRangeOptions   -> EventsOptions
--   deriving (Show)

data EventsOptions =
  EventsOptions { optEventsSubcommand :: EventSubcommand
                , optEventsLimit      :: Maybe Int
                } deriving (Show)

data EventSubcommand =
    EventCounts EventCountOptions
  | EventRange  EventRangeOptions
  deriving (Show)

data EventCountOptions =
  EventCountOptions
    { optEventsCountLimit     :: Maybe Int
    , optEventsCountStationId :: Maybe Int
    , optEventsCountStartDay  :: Maybe Day
    , optEventsCountStartTime :: Maybe TimeOfDay
    , optEventsCountEndDay    :: Maybe Day
    , optEventsCountEndTime   :: Maybe TimeOfDay
    } deriving (Show)

data EventRangeOptions =
  EventRangeOptions
    { startDay  :: Maybe Day
    , startTime :: Maybe TimeOfDay
    , endDay    :: Maybe Day
    , endTime   :: Maybe TimeOfDay
    } deriving (Show)

-- | Parser for 'EventsOptions'.
eventsOptionsParser :: Parser EventsOptions
eventsOptionsParser = EventsOptions
  <$> hsubparser
    (  command "counts" (info (EventCounts <$> eventsCountOptionsParser) (progDesc "Counts of docking and undocking events."))
    <> command "range" (info (EventRange <$> eventRangeOptionsParser) (progDesc "Docking and undocking events within a date range."))
    )
  <*> argument auto
    ( metavar "LIMIT"
    <> showDefault
    <> value Nothing
    <> help "Limit the number of events displayed."
    )

eventsCountOptionsParser :: Parser EventCountOptions
eventsCountOptionsParser = do
  optEventsCountLimit <- eventsCountsLimit
  optEventsCountStationId <- option (optional auto)
    ( metavar "STATION_ID"
    <> long "station"
    <> short 's'
    <> showDefault
    <> value Nothing
    <> help "Restrict to a specific station (ID)."
    )
  optEventsCountStartDay <- dayParser
  optEventsCountEndDay <- dayParser
  optEventsCountStartTime <- optional timeOfDayParser
  optEventsCountEndTime <- optional timeOfDayParser
  return EventCountOptions {..}

eventsCountsLimit :: Parser (Maybe Int)
eventsCountsLimit =
  option (optional auto)
    ( metavar "LIMIT"
    <> long "limit"
    <> short 'n'
    <> showDefault
    <> value Nothing
    <> help "Limit the number of events displayed."
    )

eventRangeOptionsParser :: Parser EventRangeOptions
eventRangeOptionsParser = do
  startDay <- dayParser
  endDay <- dayParser
  startTime <- optional timeOfDayParser
  endTime <- optional timeOfDayParser
  return EventRangeOptions {..}

dayParser :: Parser (Maybe Day)
dayParser =
  argument (optional auto)
    ( metavar "DATE"
    <> help "A date in the format yyyy-mm-dd."
    )

timeOfDayParser :: Parser TimeOfDay
timeOfDayParser =
  argument auto
    ( metavar "TIME"
    <> help "A time in the format HH:MM:SS."
    )
