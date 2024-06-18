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


-- Generic handler for querying query history.
queryHistoryHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m)
                    => Maybe EndpointQueried -> Maybe LocalTime -> Maybe LocalTime
                    -> m Value
queryHistoryHandler ep _startTime _endTime = do
  logInfo $
    case ep of
      Nothing  -> "Querying all endpoint query history"
      Just ep' -> "Querying query history for endpoint " <> T.pack (show ep')

  queries :: [QueryHistoryRecord] <- fmap fromRecords <$> (withPostgres . runSelectReturningList . selectWith) (queryHistoryE ep)
  pure $ toJSON queries


-- | Handler for querying all query history.
queryAllHistoryHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m)
                       => Maybe LocalTime -> Maybe LocalTime
                       -> m Value
queryAllHistoryHandler _startTime _endTime = do
  queryHistoryHandler Nothing _startTime _endTime


-- | Handler for querying query history for a specific endpoint.
queryEndpointHistoryHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m)
                            => EndpointQueried -> Maybe LocalTime -> Maybe LocalTime
                            -> m Value
queryEndpointHistoryHandler ep _startTime _endTime = do
  queryHistoryHandler (Just ep) _startTime _endTime
