-- |

module Haskbike.Server.Data.EmptyFullData
     ( combineStations
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
import qualified Haskbike.Database.Tables.StationOccupancy   as DB
import           Haskbike.Database.Tables.StationStatus      ( fromBeamStationStatusToJSON )
import qualified Haskbike.Database.Tables.StationStatus      as DB


-- | Combine station information with their corresponding statuses and empty/full data.
combineStations :: [(DB.StationInformation, DB.StationStatus)]
                -> [(DB.StationInformation, DB.EmptyFull)]
                -> [(DB.StationInformation, DB.StationStatus, DB.EmptyFull)]
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
