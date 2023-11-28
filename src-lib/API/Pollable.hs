-- |

module API.Pollable
     ( Pollable (..)
     ) where

import           AppEnv

import           UnliftIO


class Pollable a where
    pollData :: TBQueue a                  -- ^ Queue of responses
             -> TVar Int                   -- ^ Interval between requests in seconds
             -> TVar Int                   -- ^ Last updated time
             -> AppM ()

    handleData :: TBQueue a               -- ^ Queue of responses
               -> AppM ()
