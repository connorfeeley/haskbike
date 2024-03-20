{-# LANGUAGE DeriveGeneric #-}

-- | Types for "versions" endpoint.

module API.SystemRegion
     ( SystemRegion (..)
     ) where

import           API.Classes  ( HasDataField (..) )

import           Data.Aeson
import qualified Data.Text    as T

import           GHC.Generics


-- | Data type for core response object of 'versions' API.
data SystemRegion where
  SystemRegion :: { regionName :: T.Text
                  , regionId   :: T.Text
                  } -> SystemRegion
  deriving (Show, Eq, Generic)


-- * JSON instances.

instance ToJSON SystemRegion where
  toJSON v =
    object [ "region_name" .= regionName v
           , "region_id"   .= regionId   v
           ]

instance FromJSON SystemRegion where
  parseJSON = withObject "SystemRegion" $ \v -> SystemRegion
    <$> v .: "region_name"
    <*> v .: "region_id"

instance HasDataField [SystemRegion] where
  -- For a list of SystemRegion, we expect to find them under the 'versions' key
  getDataField obj = obj .: "regions"
