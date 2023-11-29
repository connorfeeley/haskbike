-- |

module API.Pollable
     ( Pollable (..)
     ) where

import           AppEnv

import           Servant.Client

import           UnliftIO


class Pollable a where
    request :: AppM (Either ClientError a)

    requester :: Pollable a
              => TBQueue a -- ^ Queue of responses.
              -> TVar Int  -- ^ Interval between requests, in seconds.
              -> TVar Int  -- ^ Last updated time.
              -> AppM ()

    handler :: TBQueue a  -- ^ Queue of responses
            -> AppM ()
