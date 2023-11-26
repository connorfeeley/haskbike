{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.DebugAPI
     ( DebugAPI (..)
     , debugApiHandler
     ) where

import           Database.Beam

import           Servant
import           Servant.Server.Generic ( AsServerT )

import           ServerEnv

import           Version


data DebugAPI mode where
  DebugAPI ::
    { serverVersion :: mode :-
      "version"
        :> Get '[JSON] Version
    } -> DebugAPI mode
  deriving stock Generic

debugApiHandler :: DebugAPI (AsServerT ServerAppM)
debugApiHandler =
  DebugAPI { serverVersion = versionHandler
           }

type Version = ((String, String), (String, String))

versionHandler :: ServerAppM Version
versionHandler = pure (("version", getCabalVersion), ("git-version", getGitVersion))
