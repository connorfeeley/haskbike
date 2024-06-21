-- | This module defines a type used to serialize the station list data.

module Haskbike.Server.Data.StationList
     ( StationListRecord (..)
     ) where

import           Data.Aeson

import qualified Haskbike.Database.Tables.StationInformation as DB
import qualified Haskbike.Database.Tables.StationStatus      as DB


-- | This type represents an entry in the station list.
data StationListRecord where
  StationListRecord :: { _stationListInfo   :: DB.StationInformation
                       , _stationListStatus :: DB.StationStatus
                       } -> StationListRecord
  deriving (Show, Eq)

instance ToJSON StationListRecord where
  toJSON record =
    object [ "station_information" .= DB.fromBeamStationInformationToJSON (_stationListInfo   record)
           , "station_status"      .= DB.fromBeamStationStatusToJSON      (_stationListStatus record)
           ]
