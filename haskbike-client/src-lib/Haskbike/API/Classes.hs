{-# LANGUAGE AllowAmbiguousTypes #-}
-- |

module Haskbike.API.Classes
     ( HasDataField (..)
     ) where

import           Data.Aeson
import           Data.Aeson.Types ( Parser )


-- Define a class to generalize the JSON data retrieval
class (FromJSON a, ToJSON a) => HasDataField a where
  dataFieldKey :: Key
  getDataField :: Object -> Parser a
  putDataField :: a -> Value
  getDataField obj = obj .: (dataFieldKey @a)
  putDataField obj = object [(dataFieldKey @a) .= obj]
