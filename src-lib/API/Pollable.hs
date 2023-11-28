-- |

module API.Pollable
     ( Pollable (..)
     ) where
import           API.ResponseWrapper

import           AppEnv

class Pollable a where
  getDataFromResponse :: ResponseWrapper a -> a
  logData :: a -> AppM ()
