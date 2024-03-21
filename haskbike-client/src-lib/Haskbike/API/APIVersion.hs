{-# LANGUAGE DeriveGeneric #-}

-- | Types for "versions" endpoint.

module API.APIVersion
     ( APIVersion (..)
     ) where

import           API.Classes  ( HasDataField (..) )

import           Data.Aeson
import qualified Data.Text    as T

import           GHC.Generics


-- | Data type for core response object of 'versions' API.
data APIVersion where
  APIVersion :: { apiVersion :: Double
                , apiUrl     :: T.Text
                } -> APIVersion
  deriving (Show, Eq, Generic)


-- * JSON instances.

instance ToJSON APIVersion where
  toJSON v =
    object [ "version" .= (show . apiVersion) v
           , "url"     .= apiUrl v
           ]

instance FromJSON APIVersion where
  parseJSON = withObject "APIVersion" $ \v -> APIVersion
    <$> fmap read (v .: "version")
    <*> v .:  "url"

instance HasDataField [APIVersion] where
  -- For a list of APIVersion, we expect to find them under the 'versions' key
  getDataField obj = obj .: "versions"
