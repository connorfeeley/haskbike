{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Query logs API definitions.

module Haskbike.Server.API.QueryLogs
     ( QueryLogsAPI (..)
     , QueryLogsHistoryAPI (..)
     , queryApiHandler
     ) where

import           Colog

import           Control.Monad.Catch                       ( MonadCatch )

import           Data.Aeson                                ( ToJSON (..), Value )
import qualified Data.Text                                 as T
import           Data.Time                                 ( LocalTime )

import           Database.Beam

import           Haskbike.Database.EndpointQueried         ( EndpointQueried )
import           Haskbike.Database.Operations.QueryHistory
import           Haskbike.Server.Data.QueryHistory
import           Haskbike.ServerEnv

import           Servant
import           Servant.Server.Generic                    ( AsServerT )

import           UnliftIO


-- * API endpoint definitions.

-- | API for querying query logs.
data QueryLogsAPI mode where
  QueryHistoryAPI ::
    { history :: mode :- "history" :> NamedRoutes QueryLogsHistoryAPI
    } -> QueryLogsAPI mode
  deriving stock Generic


-- | API for querying the query log history.
data QueryLogsHistoryAPI mode where
  QueryLogsHistoryAPI ::
    { allHistory         :: mode :- "all"
                            :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
                            :> Get '[JSON] Value
    , historyForEndpoint :: mode :- "endpoint"
                            :> Capture "endpoint" EndpointQueried
                            :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
                            :> Get '[JSON] Value
    } -> QueryLogsHistoryAPI mode
  deriving stock Generic


-- * QueryLogsAPI handlers

queryApiHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => QueryLogsAPI (AsServerT m)
queryApiHandler = QueryHistoryAPI
  { history = queryLogsHistoryApiHandler }


-- * QueryLogsHistoryAPI handlers

queryLogsHistoryApiHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => QueryLogsHistoryAPI (AsServerT m)
queryLogsHistoryApiHandler = QueryLogsHistoryAPI
  { allHistory         = queryAllHistoryHandler
  , historyForEndpoint = queryEndpointHistoryHandler
  }

queryAllHistoryHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe LocalTime -> Maybe LocalTime -> m Value
queryAllHistoryHandler _startTime _endTime = do
  logInfo "Querying all endpoint query history"
  queries :: [QueryHistoryRecord] <- fmap fromRecords <$> (withPostgres . runSelectReturningList . selectWith) (queryHistoryE Nothing)
  pure $ toJSON queries

queryEndpointHistoryHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => EndpointQueried -> Maybe LocalTime -> Maybe LocalTime -> m Value
queryEndpointHistoryHandler ep _startTime _endTime = do
  logInfo "Querying all endpoint query history"
  queries :: [QueryHistoryRecord] <- fmap fromRecords <$> (withPostgres . runSelectReturningList . selectWith) (queryHistoryE Nothing)
  pure $ toJSON queries
