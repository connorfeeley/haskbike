{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
CLI interface for querying the database.
-}
module CLI.Query where

import           API.Client
import           API.ResponseWrapper
import           API.Types              ( StationStatusResponse, status_stations )

import           Colog                  ( Message, Msg (msgText), WithLog, cmap, log, logException, pattern D,
                                          pattern E, pattern I, withLog )

import           Control.Concurrent     ( threadDelay )
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad          ( void )
import           Control.Monad.Cont     ( forever )

import qualified Data.Text              as Text

import           Database.Beam.Postgres ( Connection )
import           Database.BikeShare     ( d_status_last_reported, d_status_station_id )
import           Database.Operations

import           Fmt

import           Prelude                hiding ( log )

import           ReportTime             ( localToPosix, localToSystem )

import           UnliftIO               ( MonadIO, MonadUnliftIO, liftIO )
import           UnliftIO.Async         ( concurrently_ )

import           CLI.Options (QueryOptions (..))

dispatchQuery :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
              => QueryOptions
              -> Connection
              -> m ()
dispatchQuery options conn = do
  log E $ "Query command unimplemented. Parsed options: " <> (Text.pack . show) options
