{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.DebugAPI
     ( DebugAPI (..)
     , debugApiHandler
     ) where

import           Colog

import           Data.Pool
import           Data.String                ( fromString )
import qualified Data.Text                  as T

import           Database.Beam
import           Database.PostgreSQL.Simple

import           Servant
import           Servant.Server.Generic     ( AsServerT )

import           ServerEnv

import           Version


data DebugAPI mode where
  DebugAPI ::
    { serverVersion :: mode :- "debug"
      :> "version" :> Get '[JSON] Version
    , sleepDatabase :: mode :- "debug"
      :> "sleep-database" :> Capture "seconds" Int :> Get '[JSON] ()
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
  env <- getAppEnvFromServer -- Get the ServerEnv within ServerAppM context

  logInfo $ "Sleeping database for "<>(T.pack . show) seconds<>" seconds"
  -- throwString "This will print last as an error message"
  --   `finally` logInfo "This will print second"
  pool <- liftIO $ runAppM env withConnPool
  _ :: [Only ()] <- liftIO $ withResource pool (\conn -> query_ conn (fromString ("SELECT pg_sleep(" ++ show seconds ++ ")")))
  pure ()
