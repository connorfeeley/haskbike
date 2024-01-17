{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.DebugAPI
     ( DebugAPI (..)
     , debugApiHandler
     ) where

import           Colog

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

debugApiHandler :: DebugAPI (AsServerT ServerAppM)
debugApiHandler =
  DebugAPI { serverVersion = versionHandler
           , errorsApi = errorsApiHandler
           , sleepDatabase = sleepDatabaseHandler
           }

type Version = ((String, String), (String, String))


-- * Handlers

versionHandler :: ServerAppM Version
versionHandler = pure (("version", getCabalVersion), ("git-version", getGitVersion))


sleepDatabaseHandler :: Int -> ServerAppM ()
sleepDatabaseHandler seconds = do
  env <- getAppEnvFromServer -- Get the ServerEnv within ServerAppM context

  logInfo $ "Sleeping database for " <> (T.pack . show) seconds <> " seconds"
  -- throwString "This will print last as an error message"
  --   `finally` logInfo "This will print second"
  pool <- liftIO $ runAppM env withConnPool
  _ :: [Only ()] <- liftIO $ withResource pool (\conn -> query_ conn (fromString ("SELECT pg_sleep(" ++ show seconds ++ ")")))
  pure ()

-- * ErrorAPI handlers

errorsApiHandler :: ErrorsAPI (AsServerT ServerAppM)
errorsApiHandler = ErrorsAPI
  { latestErrors = latestErrorsHandler
  , errorsSince  = errorsSinceHandler
  }


errorsSinceHandler :: DaysAgo -> ServerAppM Value
errorsSinceHandler days@(DaysAgo daysAgo) = do
  appEnv <- asks serverAppEnv

  logInfo $ "Querying errors since " <> (T.pack . show) daysAgo
  errors <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ select $ do
    limit_ 100 $ latestQueryErrorsE (val_ days)

  let x = filter (isJust . _queryLogErrJson) errors
  let e = decodeJsonErrors x
  pure $ toJSON e


latestErrorsHandler :: Integer -> ServerAppM Value
latestErrorsHandler limit = do
  appEnv <- asks serverAppEnv

  logInfo "Querying latest errors"
  errors <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ select $ do
    limit_ limit queryErrorsE

  let x = filter (isJust . _queryLogErrJson) errors
  let e = decodeJsonErrors x
  pure $ toJSON e
