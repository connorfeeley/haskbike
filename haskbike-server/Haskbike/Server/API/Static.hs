{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

module Haskbike.Server.API.Static
     ( StaticAPI (..)
     , staticApiLink
     , staticHandler
     ) where

import           Control.Monad.Catch    ( MonadThrow )

import           Database.Beam

import           Servant
import           Servant.Server.Generic


-- * API to serve static files.

data StaticAPI mode where
  StaticAPI ::
    { staticApi :: mode :- "static" :> Raw
    } -> StaticAPI mode
  deriving stock Generic

staticHandler :: (MonadIO m, MonadThrow m) => StaticAPI (AsServerT m)
staticHandler =  StaticAPI $ serveDirectoryWebApp "static-files"

staticApiLink :: Link
staticApiLink = fieldLink staticApi
