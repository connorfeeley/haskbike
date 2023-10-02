{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}

-- | CLI interface for database operations.

module CLI.Database (dispatchDatabase) where

import           API.Client
import           API.ResponseWrapper
import           API.Types

import           AppEnv

import           CLI.Options
import qualified CLI.Poll               as P
import qualified CLI.Query               as Q

import           Colog                  ( Message, WithLog, log, pattern D, pattern E, pattern I, pattern W )

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


dispatchDatabase :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => Options -> m Connection
dispatchDatabase options = case optCommand options of
  Poll
    | optEnableMigration options -> migrate dbname >>= \conn -> pure conn
    | otherwise -> connectToDatabase dbname
  Reset resetOptions
    | optResetOnly resetOptions -> reset dbname >> liftIO exitSuccess
    | otherwise -> reset dbname
  _ -> connectToDatabase dbname
  where
    connectToDatabase name = log I "Connecting to database." >> liftIO (connectDbName name)
    setupDb     name    = liftIO $ setupDatabaseName name
    reset       name    = log W "Resetting database." >> setupDb name >>= \conn -> handleInformation conn >> pure conn
    migrate     name    = log W "Migrating database." >> connectDbNameAndMigrate name
    connectDbNameAndMigrate = liftIO . (migrateDB' <=< connectDbName)
    migrateDB' :: Connection -> IO Connection
    migrateDB' conn = migrateDB conn >> return conn
    dbname = optDatabase options

handleReset :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => Options -> ResetOptions -> m ()
handleReset options resetOptions = do
  log E $ "Reset command unimplemented. Parsed options: " <> (Text.pack . show) resetOptions

handleInformation :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => Connection -> m ()
handleInformation conn = do
  infoQuery <- liftIO $ queryStationInformation conn
  unless (null infoQuery) $ handleStationInformation conn


handleStationInformation :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)  => Connection -> m ()
handleStationInformation conn = do
  stationInfo <- liftIO (runQueryWithEnv stationInformation :: IO (Either ClientError StationInformationResponse))
  for_ (rightToMaybe stationInfo) $ \response -> do
        let stations = response ^. response_data . info_stations
        liftIO (insertStationInformation conn stations) >>= report
  where
    report = log I . ("Line length: " <>) . Text.pack . show . length
    rightToMaybe = either (const Nothing) Just
