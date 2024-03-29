-- |

module Haskbike.Server.Data.EmptyFullData
     ( EmptyFull (..)
     , EmptyFullRecord (..)
     , combineStations
     , emptyFullFromSecs
     , formatDiffTime
     ) where

import           Control.Arrow                               ( first )

import           Data.Aeson                                  ( ToJSON (..), object, (.=) )
import qualified Data.Map                                    as Map
import           Data.Maybe                                  ( mapMaybe )
import           Data.String                                 ( IsString )
import qualified Data.Text                                   as T
import           Data.Time

import           GHC.Generics                                ( Generic )

import           Haskbike.Database.Tables.StationInformation ( fromBeamStationInformationToJSON )
import qualified Haskbike.Database.Tables.StationInformation as DB
import           Haskbike.Database.Tables.StationStatus      ( fromBeamStationStatusToJSON )
import qualified Haskbike.Database.Tables.StationStatus      as DB


data EmptyFull where
  EmptyFull :: { _emptyTime :: Maybe NominalDiffTime
               , _fullTime  :: Maybe NominalDiffTime
               } -> EmptyFull
  deriving (Generic, Show, Eq)

emptyFullFromSecs :: (Integral a1, Integral a2) => Maybe a2 -> Maybe a1 -> EmptyFull
emptyFullFromSecs empty full = EmptyFull emptyTime fullTime
  where
    emptyTime = secondsToNominalDiffTime . fromIntegral <$> empty
    fullTime  = secondsToNominalDiffTime . fromIntegral <$> full

instance ToJSON EmptyFull where
  toJSON record =
    object [ "empty"       .= (formatDiffTime . _emptyTime) record
           , "full"        .= (formatDiffTime . _fullTime)  record
           ]

-- | Type for serializing to JSON for the station empty/full list.
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

-- | Combine station information with their corresponding statuses and empty/full data.
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
formatDiffTime :: Maybe NominalDiffTime -> T.Text
formatDiffTime (Just dt) = (T.pack . formatTime defaultTimeLocale (shortestFormatString dt)) dt
formatDiffTime Nothing   = T.pack "-"

-- | Determine the shortest format string for a given 'NominalDiffTime'.
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
