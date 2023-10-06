-- | This module contains expressions for querying the database.

module Database.BikeShare.Expressions
     ( disabledDocksExpr
     , infoByIdExpr
     , insertStationInformationExpr
     , queryStationIdExpr
     , queryStationIdLikeExpr
     , queryStationStatusExpr
     , statusBetweenExpr
     ) where

import qualified API.Types              as AT

import           Control.Lens           hiding ( reuse, (<.) )

import           Data.Int               ( Int32 )
import           Data.Maybe             ( fromMaybe )
import qualified Data.Text              as Text

import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Query
import           Database.Beam.Query.Internal
import           Database.Beam.Postgres
import           Database.BikeShare


-- | Expression to query the statuses for a station between two times.
statusBetweenExpr :: Int32 -> ReportTime -> ReportTime -> Q Postgres BikeshareDb s (StationStatusT (QGenExpr QValueContext Postgres s))
statusBetweenExpr station_id start_time end_time =
  do
    info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
    status <- orderBy_ (asc_ . _d_status_last_reported)
              (all_ (bikeshareDb ^. bikeshareStationStatus))
    guard_ (_d_status_info_id status `references_` info &&.
            _info_station_id info ==. val_ (fromIntegral station_id) &&.
            (status ^. d_status_last_reported) >=. val_ (Just start_time) &&.
            (status ^. d_status_last_reported) <=. val_ (Just end_time))
    pure status

-- | Expression to query information for stations by their IDs.
infoByIdExpr :: [Int32] -> Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s))
infoByIdExpr stationIds =
    filter_ (\info -> _info_station_id info `in_` map val_ stationIds)
    (all_ (bikeshareDb ^. bikeshareStationInformation))

-- | Insert station information into the database.
insertStationInformationExpr :: [AT.StationInformation] -> SqlInsert Postgres StationInformationT
insertStationInformationExpr stations =
  insert (bikeshareDb ^. bikeshareStationInformation) $
  insertExpressions $ map fromJSONToBeamStationInformation stations

disabledDocksExpr :: Q Postgres BikeshareDb s (QGenExpr QValueContext Postgres s Text.Text, QGenExpr QValueContext Postgres s Int32)
disabledDocksExpr = do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- all_ (bikeshareDb ^. bikeshareStationStatus)
  guard_ (_d_status_info_id status `references_` info &&. status^.d_status_num_docks_disabled >. 0)
  pure ( info^.info_name
       , status^.d_status_num_docks_disabled
       )

queryStationStatusExpr :: Maybe Integer -> Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s), StationStatusT (QGenExpr QValueContext Postgres s))
queryStationStatusExpr limit = do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- case limit of
    Just limit' -> limit_ limit' $ all_ (bikeshareDb ^. bikeshareStationStatus)
    Nothing     ->                 all_ (bikeshareDb ^. bikeshareStationStatus)
  guard_ (_d_status_info_id status `references_` info)
  pure (info, status)

queryStationIdExpr :: String -> Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s))
queryStationIdExpr station_name = do
  info <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_info_name info ==. val_ (Text.pack station_name))
  pure info

queryStationIdLikeExpr :: String -> Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s))
queryStationIdLikeExpr station_name = do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_info_name info `like_` val_ (Text.pack station_name))
  pure info
