{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- |

module Haskbike.Server.API.Debug
     ( debugApiHandler
     ) where

import           Colog

import           Control.Monad.Catch                    ( MonadCatch )

import           Data.Aeson                             ( ToJSON (..), Value )
import           Data.Maybe                             ( isJust )
import qualified Data.Text                              as T

import           Haskbike.Database.DaysAgo
import           Haskbike.Database.Operations.Debug
import           Haskbike.Database.Operations.QueryLogs
import           Haskbike.Database.Tables.QueryLogs
import           Haskbike.Server.API.QueryLogs
import           Haskbike.Server.Routes.Debug
import           Haskbike.ServerEnv
import           Haskbike.Version

import           Servant.Server.Generic                 ( AsServerT )

import           UnliftIO


-- * Handlers

debugApiHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m) => DebugAPI (AsServerT m)
debugApiHandler =
  DebugAPI { serverVersion = versionHandler
           , queryApi      = queryApiHandler
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
  errors <- latestQueryErrors days

  let x = filter (isJust . _queryLogErrJson) errors
  let e = decodeJsonErrors x
  pure $ toJSON e


latestErrorsHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Integer -> m Value
latestErrorsHandler limit = do
  logInfo "Querying latest errors"
  errors <- queryErrors limit

  let x = filter (isJust . _queryLogErrJson) errors
  let e = decodeJsonErrors x
  pure $ toJSON e
