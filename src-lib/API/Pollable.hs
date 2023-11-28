{-# LANGUAGE AllowAmbiguousTypes #-}
-- |

module API.Pollable
     ( Pollable (..)
     ) where
import           AppEnv

class Pollable a b where
  getDataFromResponse :: a -> b
  logData :: a -> AppM ()
