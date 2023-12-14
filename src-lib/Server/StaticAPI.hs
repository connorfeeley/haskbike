{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

module Server.StaticAPI
     ( StaticAPI (..)
     , staticHandler
     ) where

import           Database.Beam

import           Servant
import           Servant.Server.Generic

import           ServerEnv


-- * API to serve static files.

data StaticAPI mode where
  StaticAPI ::
    { staticApi :: mode :- "static" :> Raw
    } -> StaticAPI mode
  deriving stock Generic

staticHandler :: StaticAPI (AsServerT ServerAppM)
staticHandler =  StaticAPI $ serveDirectoryWebApp "static-files"
