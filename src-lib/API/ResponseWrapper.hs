-- | This module contains the common data types for the BikeShare API response.

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module API.ResponseWrapper
     ( ResponseWrapper (..)
     , response_data
     , response_last_updated
     , response_ttl
     , response_version
     ) where

import           ReportTime

import           Control.Lens hiding ( (.=) )

import           Data.Aeson
import           Data.Time    ( LocalTime )

import           GHC.Generics


-- | A type representing a BikeShare response.
data ResponseWrapper a where
  ResponseWrapper :: { _response_last_updated :: LocalTime -- POSIX timestamp of the last time the data was updated.
                     , _response_ttl          :: Int       -- Time to live of the data in seconds.
                     , _response_version      :: String    -- GBFS version of the response.
                     , _response_data         :: a         -- The data contained in the response.
                     } -> ResponseWrapper a
  deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (ResponseWrapper a) where
  parseJSON = withObject "ResponseWrapper" $ \v -> do
    _response_last_updated <- fmap posixToLocal (v .: "last_updated")
    _response_ttl          <- v .: "ttl"
    _response_version      <- v .: "version"
    _response_data         <- v .: "data"
    return ResponseWrapper {..}

instance ToJSON a => ToJSON (ResponseWrapper a) where
  toJSON ResponseWrapper {..} =
    object [ "last_reported"    .= localToPosix _response_last_updated
           , "response_ttl"     .= _response_ttl
           , "response_version" .= _response_version
           , "response_data"    .= _response_data
           ]

-- | Lenses
makeLenses ''ResponseWrapper
