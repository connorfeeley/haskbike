-- | This module contains the common data types for the BikeShare API response.

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module API.ResponseWrapper
     ( ResponseWrapper (..)
     , respData
     , respLastUpdated
     , respTtl
     , respVer
     ) where

import           Control.Lens     hiding ( (.=) )

import           Data.Aeson
import           Data.Time
import           Data.Time.Extras

import           GHC.Generics


-- | A type representing a BikeShare response.
data ResponseWrapper a where
  ResponseWrapper :: { _respLastUpdated :: UTCTime   -- POSIX timestamp of the last time the data was updated.
                     , _respTtl         :: Int       -- Time to live of the data in seconds.
                     , _respVer         :: String    -- GBFS version of the response.
                     , _respData        :: a         -- The data contained in the response.
                     } -> ResponseWrapper a
  deriving (Show, Generic)

instance FromJSON a => FromJSON (ResponseWrapper a) where
  parseJSON = withObject "ResponseWrapper" $ \v -> do
    _respLastUpdated <- fmap posixToUtc (v .: "last_updated")
    _respTtl         <- v .: "ttl"
    _respVer         <- v .: "version"
    _respData        <- v .: "data"
    return ResponseWrapper {..}

instance ToJSON a => ToJSON (ResponseWrapper a) where
  toJSON ResponseWrapper {..} =
    object [ "last_updated" .= utcToPosix _respLastUpdated
           , "ttl"          .= _respTtl
           , "version"      .= _respVer
           , "data"         .= _respData
           ]

-- | Lenses
makeLenses ''ResponseWrapper
