{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Route definitions for the debug API.

module Haskbike.Server.Routes.Debug
     ( DebugAPI (..)
     , ErrorsAPI (..)
     , Version
     ) where



import           Data.Aeson                        ( Value )

import           Database.Beam

import           Haskbike.Database.DaysAgo
import           Haskbike.Server.API.QueryLogs
import           Haskbike.Server.Page.QueryHistory ( QueryHistoryComponent (..) )
import           Haskbike.Server.Page.SideMenu

import           Servant
import           Servant.HTML.Lucid



type Version = ((String, String), (String, String))


-- | Miscellaneous debugging API endpoints.
data DebugAPI mode where
  DebugAPI ::
    { serverVersion :: mode :- "debug" :> "version"        :> Get '[JSON] Version
    , queryApi      :: mode :- "debug" :> "query-logs"     :> NamedRoutes QueryLogsAPI
    , queryApiPage  :: mode :- "debug" :> "query-logs-page":> Get '[HTML] (PureSideMenu QueryHistoryComponent)
    , errorsApi     :: mode :- "debug" :> "errors"         :> NamedRoutes ErrorsAPI
    , sleepDatabase :: mode :- "debug" :> "sleep-database" :> Capture "seconds" Int :> Get '[JSON] ()
    } -> DebugAPI mode
  deriving stock Generic

-- | API for querying failed queries.
data ErrorsAPI mode where
  ErrorsAPI ::
    { latestErrors :: mode :- "latest" :> Capture "amount" Integer :> Get '[JSON] Value
    , errorsSince  :: mode :- "since" :> Capture "days-ago" DaysAgo :> Get '[JSON] Value
    } -> ErrorsAPI mode
  deriving stock Generic
