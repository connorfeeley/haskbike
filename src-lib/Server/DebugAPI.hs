{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.DebugAPI
     ( DebugAPI (..)
     , debugApiHandler
     ) where

import           Colog

import           Control.Monad.Catch                     ( MonadCatch )

import           Data.Aeson                              ( ToJSON (..), Value )
import           Data.Maybe                              ( isJust )
import           Data.Pool
import           Data.String                             ( fromString )
import qualified Data.Text                               as T

import           Database.Beam
import           Database.BikeShare.DaysAgo
import           Database.BikeShare.Operations.QueryLogs
import           Database.BikeShare.Tables.QueryLogs
import           Database.PostgreSQL.Simple

import           Servant
import           Servant.Server.Generic                  ( AsServerT )

import           ServerEnv

import           UnliftIO

import           Version


data DebugAPI mode where
  DebugAPI ::
    { serverVersion :: mode :- "debug" :> "version"        :> Get '[JSON] Version
    , errorsApi     :: mode :- "debug" :> "errors"         :> NamedRoutes ErrorsAPI
    , sleepDatabase :: mode :- "debug" :> "sleep-database" :> Capture "seconds" Int :> Get '[JSON] ()
    } -> DebugAPI mode
  deriving stock Generic

data ErrorsAPI mode where
  ErrorsAPI ::
    { latestErrors :: mode :- "latest" :> Capture "amount" Integer :> Get '[JSON] Value
    , errorsSince :: mode :- "since" :> Capture "days-ago" DaysAgo :> Get '[JSON] Value
    } -> ErrorsAPI mode
  deriving stock Generic

debugApiHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => DebugAPI (AsServerT m)
debugApiHandler =
  DebugAPI { serverVersion = versionHandler
           , errorsApi = errorsApiHandler
           , sleepDatabase = sleepDatabaseHandler
           }

type Version = ((String, String), (String, String))


-- * Handlers

versionHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => m Version
versionHandler = pure (("version", getCabalVersion), ("git-version", getGitVersion))


sleepDatabaseHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Int -> m ()
sleepDatabaseHandler seconds = do
  logInfo $ "Sleeping database for " <> (T.pack . show) seconds <> " seconds"
  -- throwString "This will print last as an error message"
  --   `finally` logInfo "This will print second"
  pool <- getDBConnectionPool
  _ :: [Only ()] <- liftIO $ withResource pool (\conn -> query_ conn (fromString ("SELECT pg_sleep(" ++ show seconds ++ ")")))
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
  errors <- withPostgres $ runSelectReturningList $ select $ do
    limit_ 100 $ latestQueryErrorsE (val_ days)

  let x = filter (isJust . _queryLogErrJson) errors
  let e = decodeJsonErrors x
  pure $ toJSON e


latestErrorsHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Integer -> m Value
latestErrorsHandler limit = do
  logInfo "Querying latest errors"
  errors <- withPostgres $ runSelectReturningList $ select $ do
    limit_ limit queryErrorsE

  let x = filter (isJust . _queryLogErrJson) errors
  let e = decodeJsonErrors x
  pure $ toJSON e
