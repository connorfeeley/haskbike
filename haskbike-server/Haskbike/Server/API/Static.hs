-- |

module Haskbike.Server.API.Static
     ( staticApiLink
     , staticHandler
     ) where

import           Control.Monad.Catch           ( MonadThrow )

import           Haskbike.Server.Routes.Static

import           Servant
import           Servant.Server.Generic

import           UnliftIO                      ( MonadIO )


-- * API to serve static files.

staticHandler :: (MonadIO m, MonadThrow m) => StaticAPI (AsServerT m)
staticHandler =  StaticAPI $ serveDirectoryWebApp "static-files"

staticApiLink :: Link
staticApiLink = fieldLink staticApi
