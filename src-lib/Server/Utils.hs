{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
-- |

module Server.Utils
     ( StaticAPI (..)
     , sideMenu
     ) where
import           Database.Beam
import           Database.BikeShare.Expressions

import           Lucid

import           Servant

import           Server.Classes
import           Server.Components.LatestQueries
import           Server.Page.SideMenu

import           ServerEnv

import           UnliftIO

import           Version

getLatestQueries :: ServerAppM LatestQueries
getLatestQueries = do
  appEnv <- asks serverAppEnv
  latest <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith queryLatestQueryLogs
  pure $ latestQueryLogsToMap (envTimeZone appEnv) latest

-- | 'SideMenu' smart constructor.
sideMenu :: (ToHtml a, ToHtmlComponents a) => a -> ServerAppM (PureSideMenu a)
sideMenu page = do
  latest <- getLatestQueries
  pure $
    PureSideMenu
    { visPageParams = page
    , staticLink    = fieldLink staticApi
    , versionText   = getGitHash
    , latestQueries = latest
    }

-- * Serve static files.

data StaticAPI mode where
  StaticAPI ::
    { staticApi :: mode :- "static" :> Raw
    } -> StaticAPI mode
  deriving stock Generic
