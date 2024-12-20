{-# LANGUAGE DeriveGeneric #-}

-- | Types for "versions" endpoint.

module Haskbike.API.APIVersion
     ( APIVersion (..)
     ) where

import           Data.Aeson
import qualified Data.Text            as T

import           GHC.Generics

import           Haskbike.API.Classes ( HasDataField (..) )


-- | Data type for core response object of 'versions' API.
data APIVersion where
  APIVersion :: { _apiVersion :: Double
                , _apiUrl     :: T.Text
                } -> APIVersion
  deriving (Show, Eq, Generic)


-- * JSON instances.

instance ToJSON APIVersion where
  toJSON v =
    object [ "version" .= (show . _apiVersion) v
           , "url"     .= _apiUrl v
           ]

instance FromJSON APIVersion where
  parseJSON = withObject "APIVersion" $ \v -> APIVersion
    <$> fmap read (v .: "version")
    <*> v .:  "url"

instance HasDataField [APIVersion] where
  -- For a list of APIVersion, we expect to find them under the 'versions' key
  dataFieldKey = "versions"
