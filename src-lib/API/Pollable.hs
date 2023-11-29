-- |

module API.Pollable
     ( Pollable (..)
     , Requestable (..)
     ) where

import           AppEnv

import           Servant.Client

import           UnliftIO


class Pollable a where
    pollData :: TBQueue a                  -- ^ Queue of responses
             -> TVar Int                   -- ^ Interval between requests in seconds
             -> TVar Int                   -- ^ Last updated time
             -> AppM ()

    handleData :: TBQueue a               -- ^ Queue of responses
               -> AppM ()

class Requestable a where
    request :: AppM (Either ClientError a)
    requester :: Requestable a
              => TBQueue a -- ^ Queue of responses.
              -> TVar Int                           -- ^ Interval between requests, in seconds.
              -> TVar Int                           -- ^ Last updated time.
              -> AppM ()
    handler :: TBQueue a  -- ^ Queue of responses
            -> AppM ()
