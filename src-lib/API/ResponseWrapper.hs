-- | This module contains the common data types for the BikeShare API response.

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.ResponseWrapper
        ( ResponseWrapper (..)
        ) where

import           Data.Aeson
import qualified Data.Text            as Text
import           GHC.Generics

import           Data.Attoparsec.Text (Parser, choice, parseOnly, string)
import           Data.Either          (fromRight)
import           Data.Functor         (($>))
import           Data.Text            (pack)
import           Control.Lens         hiding ((.=))


-- | A type representing a BikeShare response.
data ResponseWrapper a where
  ResponseWrapper :: { response_last_updated :: Int
                     , response_ttl          :: Int
                     , response_version      :: String
                     , response_data         :: a
                     } -> ResponseWrapper a
  deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (ResponseWrapper a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 9 }

instance ToJSON a   => ToJSON   (ResponseWrapper a) where
  toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 9 }

-- | Lenses
makeLenses ''ResponseWrapper
