{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Query logs API definitions.

module Haskbike.Server.API.QueryLogs
     ( queryApiHandler
     ) where

import           Colog

import           Control.Monad.Catch                       ( MonadCatch )

import           Data.Aeson                                ( ToJSON (..), Value )
import qualified Data.Text                                 as T
import           Data.Time                                 ( LocalTime, UTCTime )

import           Database.Beam

import           Haskbike.Database.EndpointQueried         ( EndpointQueried )
import           Haskbike.Database.Operations.QueryHistory
import           Haskbike.Server.Data.QueryHistory
import           Haskbike.Server.Routes.QueryLogs
import           Haskbike.ServerEnv

import           Servant.Server.Generic                    ( AsServerT )

import           UnliftIO


-- * QueryLogsAPI handlers

queryApiHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => QueryLogsAPI (AsServerT m)
queryApiHandler = QueryLogsAPI
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
  tz <- getTz  -- Fetch the TimeZone from the environment
  logInfo $
    case ep of
      Nothing  -> "Querying all endpoint query history"
      Just ep' -> "Querying query history for endpoint " <> T.pack (show ep')

  queries       :: [QueryHistoryRecord UTCTime] <- fmap fromRecords <$> (withPostgres . runSelectReturningList . selectWith) (queryHistoryE ep)
  let queriesLt :: [QueryHistoryRecord LocalTime] = toLocalTime tz <$> queries
  pure $ toJSON queriesLt


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
