{-# LANGUAGE TemplateHaskell #-}

-- |

module API.Response.StationInformationResponse
     ( StationInformationResponse
     , StationInformationResponseData (..)
     , unInfoStations
     ) where

import           API.ResponseWrapper
import           API.StationInformation

import           Control.Lens

import           Data.Aeson

import           GHC.Generics           ( Generic )

-- | A wrapper type for the station information response.
newtype StationInformationResponseData where
  StationInformationResponseData :: { _unInfoStations :: [StationInformation] } -> StationInformationResponseData
  deriving (Show, Generic)

instance FromJSON StationInformationResponseData where
  parseJSON = withObject "StationInformationResponseData" $ \v -> do
    StationInformationResponseData <$> v .: "stations"

-- | Type synonym for the wrapped station information response.
type StationInformationResponse = ResponseWrapper StationInformationResponseData

makeLenses ''StationInformationResponseData
