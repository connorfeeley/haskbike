{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}

module Main
     ( appMain
     , main
     ) where

import           API.Client
import qualified CLI.Poll               as P
import           API.ResponseWrapper
import           API.Types

import           AppEnv

import           Colog                  ( Message, WithLog, log, pattern D, pattern I, pattern W, pattern E )

import           Control.Lens
import           Control.Monad          ( unless, (<=<) )
import           Control.Monad.IO.Class ( MonadIO (liftIO) )

import           Data.Foldable          ( for_ )
import qualified Data.Text              as Text

import           Database.Beam.Postgres ( Connection )
import           Database.Migrations
import           Database.Operations
import           Database.Utils

import           Options.Applicative

import           Prelude                hiding ( log )

import           Servant.Client         ( ClientError )

import           System.Exit            ( exitSuccess )

import           UnliftIO               ( MonadUnliftIO )

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

main :: IO ()
main = runApp simpleEnv appMain

appMain :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
        => m ()
appMain = do
  -- Parse command line options.
  options <- liftIO $ customExecParser (prefs $ helpShowGlobals <> showHelpOnEmpty <> showHelpOnError) opts
  log D $ "Parsed options" <> Text.pack (show options)
  conn <- handleDatabase options

  P.pollClient conn
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Poll the Toronto Bikeshare API and inserts new records into the database."
     <> header "Toronto Bikeshare API client" )

handleInformation :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                  => Connection
                  -> m ()
handleInformation conn = do
  infoQuery <- liftIO $ queryStationInformation conn
  unless (null infoQuery) $ handleStationInformation conn

handleStationInformation :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                         => Connection
                         -> m ()
handleStationInformation conn = do
  stationInfo <- liftIO (runQueryWithEnv stationInformation :: IO (Either ClientError StationInformationResponse))
  for_ (rightToMaybe stationInfo) $ \response -> do
        let stations = response ^. response_data . info_stations
        liftIO (insertStationInformation conn stations) >>= report
  where
    report = log I . ("Line length: " <>) . Text.pack . show . length
    rightToMaybe = either (const Nothing) Just

handleDatabase :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
               => Options
               -> m Connection
handleDatabase options = case optCommand options of
  Poll
    | optEnableMigration options -> migrate dbname >>= \conn -> pure conn
    | otherwise -> connectToDatabase dbname
  Reset resetOptions
    | optResetOnly resetOptions -> reset dbname >> liftIO exitSuccess
    | otherwise -> reset dbname
  where
    connectToDatabase name = log I "Connecting to database." >> liftIO (connectDbName name)
    setupDb     name    = liftIO $ setupDatabaseName name
    reset       name    = log W "Resetting database." >> setupDb name >>= \conn -> handleInformation conn >> pure conn
    migrate     name    = log W "Migrating database." >> connectDbNameAndMigrate name
    connectDbNameAndMigrate = liftIO . (migrateDB' <=< connectDbName)
    migrateDB' :: Connection -> IO Connection
    migrateDB' conn = migrateDB conn >> return conn
    dbname = optDatabase options
