-- |

module Server.Data.EmptyFullData
     ( EmptyFull (..)
     , EmptyFullRecord (..)
     , combineStations
     ) where

import           Control.Arrow                                ( first )

import           Data.Aeson                                   ( ToJSON (..), object, (.=) )
import qualified Data.Map                                     as Map
import           Data.Maybe                                   ( mapMaybe )
import           Data.Time                                    ( NominalDiffTime )

import           Database.BikeShare.Tables.StationInformation ( fromBeamStationInformationToJSON )
import qualified Database.BikeShare.Tables.StationInformation as DB
import           Database.BikeShare.Tables.StationStatus      ( fromBeamStationStatusToJSON )
import qualified Database.BikeShare.Tables.StationStatus      as DB


data EmptyFull where
  EmptyFull :: { _emptyTime :: NominalDiffTime
               , _fullTime  :: NominalDiffTime
               } -> EmptyFull

instance ToJSON EmptyFull where
  toJSON record =
    object [ "empty_seconds"       .= _emptyTime record
           , "full_seconds"        .= _fullTime  record
           ]


data EmptyFullRecord where
  EmptyFullRecord :: { _emptyFullInformation :: DB.StationInformation
                     , _emptyFullStatus      :: DB.StationStatus
                     , _emptyFullDurations   :: EmptyFull
                     } -> EmptyFullRecord

instance ToJSON EmptyFullRecord where
  toJSON record =
    object [ "station_information" .= fromBeamStationInformationToJSON  (_emptyFullInformation record)
           , "station_status"      .= fromBeamStationStatusToJSON       (_emptyFullStatus   record)
           , "durations"           .= _emptyFullDurations   record
           ]

combineStations :: [(DB.StationInformation, DB.StationStatus)]
                -> [(DB.StationInformation, EmptyFull)]
                -> [(DB.StationInformation, DB.StationStatus, EmptyFull)]
combineStations latestStatuses empties = mapMaybe combine latestStatuses
  where
    combine (info, status) = do
      emptyFull <- Map.lookup (DB._infoStationId info) empties'
      return (info, status, emptyFull)

    empties' = Map.fromList (map (first DB._infoStationId) empties)
