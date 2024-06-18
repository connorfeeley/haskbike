{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- |

module Haskbike.Server.API.Debug
     ( DebugAPI (..)
     , debugApiHandler
     ) where

import           Colog

import           Control.Monad.Catch                    ( MonadCatch )

import           Data.Aeson                             ( ToJSON (..), Value )
import           Data.Maybe                             ( isJust )
import qualified Data.Text                              as T

import           Database.Beam

import           Haskbike.Database.DaysAgo
import           Haskbike.Database.Operations.QueryLogs
import           Haskbike.Database.Tables.QueryLogs
import           Haskbike.Server.API.QueryLogs
import           Haskbike.Server.Page.QueryHistory      ( QueryHistoryComponent (..) )
import           Haskbike.Server.Page.SideMenu
import           Haskbike.ServerEnv
import           Haskbike.Version

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic                 ( AsServerT )

import           UnliftIO


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

-- * Handlers

debugApiHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m) => DebugAPI (AsServerT m)
debugApiHandler =
  DebugAPI { serverVersion = versionHandler
           , queryApi      = queryApiHandler
           , queryApiPage  = queryApiPageHandler
           , errorsApi     = errorsApiHandler
           , sleepDatabase = sleepDatabaseHandler
           }

versionHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => m Version
versionHandler = pure (("version", getCabalVersion), ("git-version", getGitVersion))


sleepDatabaseHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Int -> m ()
sleepDatabaseHandler seconds = do
  logInfo $ "Asked to sleep database for " <> (T.pack . show) seconds <> " seconds. Not sleeping."
  -- throwString "This will print last as an error message"
  --   `finally` logInfo "This will print second"
  -- pool <- getDBConnectionPool
  -- _ :: [Only ()] <- liftIO $ withResource pool (\conn -> query_ conn (fromString ("SELECT pg_sleep(" ++ show seconds ++ ")")))
  pure ()


-- * ErrorAPI handlers

errorsApiHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => ErrorsAPI (AsServerT m)
errorsApiHandler = ErrorsAPI
  { latestErrors = latestErrorsHandler
  , errorsSince  = errorsSinceHandler
  }


errorsSinceHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => DaysAgo -> m Value
errorsSinceHandler days@(DaysAgo daysAgo) = do
  logInfo $ "Querying errors since " <> (T.pack . show) daysAgo
  errors <- withPostgres $ runSelectReturningList $ select $ limit_ 100 $ latestQueryErrorsE (val_ days)

  let x = filter (isJust . _queryLogErrJson) errors
  let e = decodeJsonErrors x
  pure $ toJSON e


latestErrorsHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Integer -> m Value
latestErrorsHandler limit = do
  logInfo "Querying latest errors"
  errors <- withPostgres $ runSelectReturningList $ select $ limit_ limit queryErrorsE

  let x = filter (isJust . _queryLogErrJson) errors
  let e = decodeJsonErrors x
  pure $ toJSON e

queryApiPageHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m)
                    => m (PureSideMenu QueryHistoryComponent)
queryApiPageHandler = do
  logInfo "Rendering performance CSV page"

  sideMenu $ QueryHistoryComponent { }
