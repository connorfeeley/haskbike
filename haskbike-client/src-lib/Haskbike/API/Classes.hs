-- |

module Haskbike.API.Classes
     ( HasDataField (..)
     ) where

import           Data.Aeson
import           Data.Aeson.Types ( Parser )


-- Define a class to generalize the JSON data retrieval
class HasDataField a where
  getDataField :: Object -> Parser a
