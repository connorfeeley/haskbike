{-# LANGUAGE TemplateHaskell #-}

-- |

module API.Response.StationStatusResponse
     ( StationStatusResponse
     , StationStatusResponseData (..)
     , unStatusStations
     ) where

import           API.ResponseWrapper
import           API.StationStatus

import           Control.Lens

import           Data.Aeson

import           GHC.Generics        ( Generic )


-- | A wrapper type for the station information response.
newtype StationStatusResponseData where
  StationStatusResponseData :: { _unStatusStations :: [StationStatus] } -> StationStatusResponseData
  deriving (Show, Generic)

instance FromJSON StationStatusResponseData where
  parseJSON = withObject "StationStatusResponseData" $ \v -> do
    StationStatusResponseData <$> v .: "stations"

-- | Type synonym for the wrapped station information response.
type StationStatusResponse = ResponseWrapper StationStatusResponseData

-- | Lenses
makeLenses ''StationStatusResponseData
