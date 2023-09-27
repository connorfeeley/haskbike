{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}

module Main
     ( main
     ) where

import           API.Client
import qualified API.Poll               as P
import           API.ResponseWrapper
import           API.Types

import           AppEnv

import           Colog                  ( Message, WithLog, log, pattern I, pattern W )
import           Colog.Message          ( logException )

import           Control.Lens
import           Control.Monad          ( when )
import           Control.Monad.IO.Class ( MonadIO (liftIO) )

import qualified Data.Text              as Text

import           Database.Beam.Postgres ( Connection )
import           Database.Migrations
import           Database.Operations
import           Database.Utils

import           Options.Applicative

import           Prelude                hiding ( log )

import           Servant.Client         ( ClientError )

import           System.Environment
import           System.Exit            ( exitSuccess )

import           UnliftIO               ( MonadUnliftIO )

-- | Top-level options.
data Options where
  Options :: { optCommand :: !Command
             , optVerbose :: Bool
             } -> Options
  deriving (Show)

-- | Parser for 'Options'.
parseOptions :: Parser Options
parseOptions = Options
  <$> commandParser
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose output" )

-- | Top-level commands.
data Command where
  Poll  :: Command
  Reset :: !ResetOptions -> Command
  deriving (Show)

-- | Parser for 'Command'.
commandParser :: Parser Command
commandParser = subparser
  (  command "poll"   (info (pure Poll)
                       (progDesc "Poll the API and insert new station status into database."))
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
     <> help "Only reset, don't insert new data" )
  <*> switch
      ( long "test"
     <> help "Run the command in test mode" )


main :: IO ()
main = runApp simpleEnv appMain

appMain :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
        => m ()
appMain = do
  options <- liftIO $ customExecParser (prefs $ helpShowGlobals <> showHelpOnEmpty <> showHelpOnError) opts
  liftIO $ print options

  -- Get command-line arguments
  args <- liftIO getArgs

  conn <- if "--migrate" `elem` args || "--migrate-only" `elem` args
    then do
      -- Perform database migrations.
      log W "Migrating database."

      conn' <- liftIO $ connectDbName dbnameProduction
      _ <- liftIO $ migrateDB conn'

      -- Exit if only migrations were requested.
      when ("--migrate-only" `elem` args) $
        log W "Migrating database." >>
        liftIO exitSuccess

      pure conn'
    else if "--reset" `elem` args then do
      -- Reset the database.
      log W "Resetting database."
      conn <- liftIO $ setupDatabaseName dbnameProduction

      -- Insert station information if missing from database.
      infoQuery <- liftIO $ queryStationInformation conn
      when (null infoQuery) $ handleStationInformation conn
      -- Exit.
      liftIO exitSuccess
    -- Otherwise, connect to the database.
    else log I "Connecting to database." >> liftIO (connectDbName dbnameProduction)

  -- Insert station information if missing from database.
  infoQuery <- liftIO $ queryStationInformation conn
  when (null infoQuery) $ handleStationInformation conn

  P.pollClient conn
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Poll the Toronto Bikeshare API."
     <> header "Toronto Bikeshare API client" )


handleStationInformation :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                         => Connection -- ^ Database connection.
                         -> m ()
handleStationInformation conn = do
  information <- liftIO (runQueryWithEnv stationInformation :: IO (Either ClientError StationInformationResponse))
  case information of
    Left err -> logException err
    Right response -> do
      let stations = response ^. response_data . info_stations
      inserted <- liftIO $ insertStationInformation conn stations
      log I $ "Line length: " <> Text.pack (show $ length inserted)
