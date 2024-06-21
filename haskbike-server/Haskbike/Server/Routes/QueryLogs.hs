{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Route definitions for Query logs API.

module Haskbike.Server.Routes.QueryLogs
     ( QueryLogsAPI (..)
     , QueryLogsHistoryAPI (..)
     ) where

import           Data.Aeson                        ( Value )
import           Data.Time                         ( LocalTime )

import           GHC.Generics                      ( Generic )

import           Haskbike.Database.EndpointQueried ( EndpointQueried )

import           Servant


-- * API endpoint definitions.

-- | API for querying query logs.
data QueryLogsAPI mode where
  QueryLogsAPI ::
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
