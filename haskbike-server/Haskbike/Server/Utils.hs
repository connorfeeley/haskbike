{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Haskbike.Server.Utils
     ( getLatestQueries
     , sideMenu
     ) where

import           Control.Monad.Catch           ( MonadCatch )

import           Database.Beam

import           Haskbike.Database.Expressions
import           Haskbike.LatestQueries
import           Haskbike.Server.Classes
import           Haskbike.Server.Page.SideMenu
import           Haskbike.Server.StaticAPI
import           Haskbike.ServerEnv
import           Haskbike.Version

import           Lucid

import           Servant


getLatestQueries :: (HasEnv env m, MonadIO m, MonadCatch m) => m LatestQueries
getLatestQueries = do
  tz <- getTz
  latest <- withPostgres $ runSelectReturningList $ selectWith queryLatestQueryLogs
  pure $ latestQueryLogsToMap tz latest

-- | 'SideMenu' smart constructor.
sideMenu :: (HasEnv env m, MonadIO m, ToHtml a, ToHtmlComponents a, MonadCatch m) => a -> m (PureSideMenu a)
sideMenu page = do
  latest <- getLatestQueries
  pure $
    PureSideMenu
    { visPageParams    = page
    , staticLink       = fieldLink staticApi
    , cabalVersionText = getCabalVersion
    , gitVersionText   = getGitHash
    , latestQueries    = latest
    }
