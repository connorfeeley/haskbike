-- |

module Haskbike.Server.API.Static
     ( staticApiLink
     , staticHandler
     ) where

import           Control.Monad.Catch           ( MonadThrow )

import           Database.Beam

import           Haskbike.Server.Routes.Static

import           Servant
import           Servant.Server.Generic


-- * API to serve static files.

staticHandler :: (MonadIO m, MonadThrow m) => StaticAPI (AsServerT m)
staticHandler =  StaticAPI $ serveDirectoryWebApp "static-files"

staticApiLink :: Link
staticApiLink = fieldLink staticApi
