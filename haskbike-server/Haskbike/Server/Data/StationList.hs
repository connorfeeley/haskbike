-- | This module defines a type used to serialize the station list data.

module Haskbike.Server.Data.StationList
     ( StationListRecord (..)
     , StationListable (..)
     ) where

import           Data.Aeson

import qualified Haskbike.Database.Tables.StationInformation as DB
import qualified Haskbike.Database.Tables.StationStatus      as DB


-- FIXME: currently unused. Consider removing if it doesn't fix the established pattern.
-- | Typeclass for data that can be serialized into station list data.
class StationListable a where
  toStationListData :: ToJSON a => a  -> Value

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
