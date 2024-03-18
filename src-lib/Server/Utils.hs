{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.Utils
     ( getLatestQueries
     , sideMenu
     ) where

import           Control.Monad.Catch             ( MonadCatch )

import           Database.Beam
import           Database.BikeShare.Expressions

import           Lucid

import           Servant

import           Server.Classes
import           Server.Components.LatestQueries
import           Server.Page.SideMenu
import           Server.StaticAPI

import           ServerEnv

import           Version


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
    { visPageParams = page
    , staticLink    = fieldLink staticApi
    , versionText   = getGitHash
    , latestQueries = latest
    }
