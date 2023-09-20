-- | This module contains the common data types for the BikeShare API response.

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module API.ResponseWrapper
        ( ResponseWrapper (..)
        ) where

import           Common

import           Data.Aeson
import           GHC.Generics

import           Control.Lens hiding ((.=))
import           Data.Time    (LocalTime)


-- | A type representing a BikeShare response.
data ResponseWrapper a where
  ResponseWrapper :: { response_last_updated :: LocalTime
                     , response_ttl          :: Int
                     , response_version      :: String
                     , response_data         :: a
                     } -> ResponseWrapper a
  deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (ResponseWrapper a) where
  parseJSON = withObject "ResponseWrapper" $ \v -> do
    response_last_updated <- fmap posixToLocal (v .: "last_updated")
    response_ttl          <- v .: "ttl"
    response_version      <- v .: "version"
    response_data         <- v .: "data"
    return ResponseWrapper {..}

instance ToJSON a => ToJSON (ResponseWrapper a) where
  toJSON ResponseWrapper {..} =
    object [ "last_reported"    .= localToPosix response_last_updated
           , "response_ttl"     .= response_ttl
           , "response_version" .= response_version
           , "response_data"    .= response_data
           ]

-- | Lenses
makeLenses ''ResponseWrapper
