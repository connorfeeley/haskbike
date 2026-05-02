{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Route definitions for the main component of the debug API.

module Haskbike.Server.Routes.Debug.DebugAPI
     ( DebugAPI (..)
     , ErrorsAPI (..)
     , ServerVersion (..)
     , debugRoutesLinks
     ) where

import           Data.Aeson                             ( ToJSON (..), Value, (.=) )
import           Data.Aeson.Types                       ( object )
import qualified Data.Text                              as T

import           GHC.Generics                           ( Generic )

import           Haskbike.Database.DaysAgo
import           Haskbike.Server.Routes.Debug.QueryLogs

import           Servant


-- | The version of the server.
data ServerVersion where
  ServerVersion ::
    { _serverVersion    :: T.Text
    , _serverGitVersion :: T.Text
    , _serverGitHash    :: T.Text
    } -> ServerVersion
  deriving stock Generic

instance ToJSON ServerVersion where
  toJSON v =
    object [ "version"         .= _serverVersion    v
           , "git-version"     .= _serverGitVersion v
           , "git-hash"        .= _serverGitHash    v
           ]


-- | Miscellaneous debugging API endpoints.
data DebugAPI mode where
  DebugAPI ::
    { serverVersion :: mode :- "version"        :> Get '[JSON] ServerVersion
    , queryApi      :: mode :- "query-logs"     :> NamedRoutes QueryLogsAPI
    , errorsApi     :: mode :- "errors"         :> NamedRoutes ErrorsAPI
    , sleepDatabase :: mode :- "sleep-database" :> Capture "seconds" Int :> Get '[JSON] ()
    } -> DebugAPI mode
  deriving stock Generic

-- | API for querying failed queries.
data ErrorsAPI mode where
  ErrorsAPI ::
    { latestErrors :: mode :- "latest" :> Capture "amount"   Integer :> Get '[JSON] Value
    , errorsSince  :: mode :- "since"  :> Capture "days-ago" DaysAgo :> Get '[JSON] Value
    } -> ErrorsAPI mode
  deriving stock Generic

debugRoutesLinks :: DebugAPI (AsLink Link)
debugRoutesLinks = allFieldLinks
