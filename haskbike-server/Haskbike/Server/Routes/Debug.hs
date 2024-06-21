{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Route definitions for the debug API.

module Haskbike.Server.Routes.Debug
     ( DebugAPI (..)
     , ErrorsAPI (..)
     , Version
     , debugRoutesLinks
     ) where

import           Data.Aeson                       ( Value )

import           GHC.Generics                     ( Generic )

import           Haskbike.Database.DaysAgo
import           Haskbike.Server.Routes.QueryLogs

import           Servant


-- | The version of the server.
type Version = ((String, String), (String, String))


-- | Miscellaneous debugging API endpoints.
data DebugAPI mode where
  DebugAPI ::
    { serverVersion :: mode :- "debug" :> "version"        :> Get '[JSON] Version
    , queryApi      :: mode :- "debug" :> "query-logs"     :> NamedRoutes QueryLogsAPI
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

debugRoutesLinks :: DebugAPI (AsLink Link)
debugRoutesLinks = allFieldLinks
