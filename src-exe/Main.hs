{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RecordWildCards    #-}

module Main
     ( main
     ) where

import           API.Client
import qualified API.Poll               as P
import           API.ResponseWrapper
import           API.Types

import           Colog                  ( HasLog (..), LogAction, Message, Msg (..), WithLog, formatWith, log,
                                          logTextStderr, pattern I, pattern W, showSeverity, showSourceLoc )
import           Colog.Message          ( logException )

import           Control.Lens
import           Control.Monad          ( when )
import           Control.Monad.IO.Class ( MonadIO (liftIO) )
import           Control.Monad.Reader   ( MonadReader, ReaderT (..) )

import qualified Data.Text              as Text

import           Database.Beam.Postgres ( Connection )
import           Database.Migrations
import           Database.Operations
import           Database.Utils

import           Prelude                hiding ( log )

import           Servant.Client         ( ClientError )

import           System.Environment
import           System.Exit            ( exitSuccess )

import           UnliftIO               ( MonadUnliftIO )

-- Application environment
data Env m where
  Env :: { envLogAction :: !(LogAction m Message) } -> Env m

-- Implement logging for the application environment.
instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newLogAction env = env { envLogAction = newLogAction }
    {-# INLINE setLogAction #-}

-- Application type
newtype App a = App
    { unApp :: ReaderT (Env App) IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (Env App))

simpleEnv :: Env App
simpleEnv = Env
    { envLogAction  = logStdErrActionWithoutStackAction -- richMessageAction
    }

fmtMessageWithoutSourceLoc :: Message -> Text.Text
fmtMessageWithoutSourceLoc Msg{..} =
  showSeverity msgSeverity
  <> showSourceLoc msgStack
  <> msgText

logStdErrActionWithoutStackAction :: MonadIO env => LogAction env Message
logStdErrActionWithoutStackAction = formatWith fmtMessageWithoutSourceLoc logTextStderr


runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env


main :: IO ()
main = runApp simpleEnv appMain

appMain :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
        => m ()
appMain = do
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

handleStationInformation :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                         => Connection -- ^ Database connection.
                         -> m ()
handleStationInformation conn = do
  info <- liftIO (runQueryWithEnv stationInformation :: IO (Either ClientError StationInformationResponse))
  case info of
    Left err -> logException err
    Right response -> do
      let stations = response ^. response_data . info_stations
      inserted <- liftIO $ insertStationInformation conn stations
      log I $ "Line length: " <> Text.pack (show $ length inserted)
