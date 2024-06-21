-- | This module contains the functions to query the latest queries from the database.

module Haskbike.Database.Operations.LatestQueries
     ( queryLatestQueries
     ) where

import           Control.Monad.Catch                ( MonadCatch )

import           Database.Beam

import           Haskbike.AppEnv
import           Haskbike.Database.Expressions
import           Haskbike.Database.Tables.QueryLogs


queryLatestQueries :: (HasEnv env m, MonadIO m, MonadCatch m) => m [QueryLog]
queryLatestQueries = withPostgres . runSelectReturningList $ selectWith queryLatestQueryLogsE
