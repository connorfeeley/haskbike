{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.DebugAPI
     ( DebugAPI (..)
     , debugApiHandler
     ) where

import           Colog

import           Data.String                ( fromString )

import           Database.Beam
import           Database.PostgreSQL.Simple

import           Fmt

import           Servant
import           Servant.Server.Generic     ( AsServerT )

import           ServerEnv

import           Version


data DebugAPI mode where
  DebugAPI ::
    { serverVersion :: mode :-
      "version"
        :> Get '[JSON] Version
    , sleepDatabase :: mode :-
      "sleep-database"
        :> Capture "seconds" Int
        :> Get '[JSON] ()
    } -> DebugAPI mode
  deriving stock Generic

debugApiHandler :: DebugAPI (AsServerT ServerAppM)
debugApiHandler =
  DebugAPI { serverVersion = versionHandler
           , sleepDatabase = sleepDatabaseHandler
           }

type Version = ((String, String), (String, String))

versionHandler :: ServerAppM Version
versionHandler = pure (("version", getCabalVersion), ("git-version", getGitVersion))

sleepDatabaseHandler :: Int -> ServerAppM ()
sleepDatabaseHandler seconds = do
  env <- ask -- Get the ServerEnv within ServerAppM context

  logInfo $ format "Sleeping database for {} seconds" seconds

  let conn = envDBConnection (serverAppEnv env)
  _ :: [Only ()] <- liftIO $ query_ conn $ fromString ("SELECT pg_sleep(" ++ show seconds ++ ")")
  pure ()
