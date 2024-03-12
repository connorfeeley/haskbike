-- |

module Server.Data.EmptyFullData
     ( EmptyFull (..)
     , EmptyFullRecord (..)
     , combineStations
     , formatDiffTime
     ) where

import           Control.Arrow                                ( first )

import           Data.Aeson                                   ( ToJSON (..), object, (.=) )
import qualified Data.Map                                     as Map
import           Data.Maybe                                   ( mapMaybe )
import           Data.String                                  ( IsString )
import qualified Data.Text                                    as T
import           Data.Time

import           Database.BikeShare.Tables.StationInformation ( fromBeamStationInformationToJSON )
import qualified Database.BikeShare.Tables.StationInformation as DB
import           Database.BikeShare.Tables.StationStatus      ( fromBeamStationStatusToJSON )
import qualified Database.BikeShare.Tables.StationStatus      as DB

import           GHC.Generics                                 ( Generic )


data EmptyFull where
  EmptyFull :: { _emptyTime :: NominalDiffTime
               , _fullTime  :: NominalDiffTime
               } -> EmptyFull
  deriving (Generic, Show, Eq)

instance ToJSON EmptyFull where
  toJSON record =
    object [ "empty"       .= (formatDiffTime . _emptyTime) record
           , "full"        .= (formatDiffTime . _fullTime)  record
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

-- | Format a 'NominalDiffTime' as 'Text' with a human-readable format.
formatDiffTime :: NominalDiffTime -> T.Text
formatDiffTime dt = (T.pack . formatTime defaultTimeLocale (shortestFormatString dt)) dt

shortestFormatString :: IsString a => NominalDiffTime -> a
shortestFormatString dt =
  case (days, hours, minutes, dt) of -- if dt >= nominalDay then "%dd %Hh %Mm %Ss" else "%Hh %Mm %Ss"
    (0 :: Integer, 0 :: Integer, 0 :: Integer, 0) -> "-"               -- no value
    (0, 0, 0, _)                                  -> "%Ss"             -- seconds
    (0, 0, _, _)                                  -> "%Mm %Ss"         -- minutes and seconds
    (0, _, _, _)                                  -> "%Hh %Mm %Ss"     -- hours, minutes, seconds
    (_, _, _, _)                                  -> "%dd %Hh %Mm %Ss" -- days, hours, minutes, seconds
  where
    nominalHour   = secondsToNominalDiffTime (60 * 60)
    nominalMinute = secondsToNominalDiffTime 60
    days    = floor (dt / nominalDay)
    hours   = floor (dt / nominalHour)
    minutes = floor (dt / nominalMinute)
