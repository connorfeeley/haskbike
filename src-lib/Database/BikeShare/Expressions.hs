{-# LANGUAGE PartialTypeSignatures #-}

-- | This module contains expressions for querying the database.

module Database.BikeShare.Expressions
     ( disabledDocksExpr
     , infoByIdExpr
     , insertStationInformationExpr
     , queryLatestStatusBeforeTimeExpr
     , queryStationIdExpr
     , queryStationIdLikeExpr
     , queryStationStatusExpr
     , statusBetweenExpr
     , systemStatusBetweenExpr
     ) where

import qualified API.Types                                as AT

import           Control.Lens                             hiding ( reuse, (<.) )

import           Data.Int                                 ( Int32 )
import qualified Data.Text                                as Text
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions ( BeamHasInsertOnConflict (anyConflict, onConflictDoNothing),
                                                            insertOnConflict )
import           Database.Beam.Postgres
import           Database.BikeShare


-- | Expression to query the all statuses for the system between two times.
systemStatusBetweenExpr :: UTCTime -> UTCTime -> Q Postgres BikeshareDb s (StationStatusT (QGenExpr QValueContext Postgres s))
systemStatusBetweenExpr start_time end_time =
  do
    info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
    status <- all_ (bikeshareDb ^. bikeshareStationStatus)
    guard_ (_statusStationId status `references_` info &&.
            (status ^. statusLastReported) >=. val_ start_time &&.
            (status ^. statusLastReported) <=. val_ end_time)
    pure status

-- | Expression to query the statuses for a station between two times.
statusBetweenExpr :: Int32 -> UTCTime -> UTCTime -> Q Postgres BikeshareDb s (StationStatusT (QGenExpr QValueContext Postgres s))
statusBetweenExpr station_id start_time end_time =
  do
    info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
    status <- orderBy_ (asc_ . _statusLastReported)
              (all_ (bikeshareDb ^. bikeshareStationStatus))
    guard_ (_statusStationId status `references_` info &&.
            (info   ^. infoStationId) ==. val_ (fromIntegral station_id) &&.
            (status ^. statusLastReported) >=. val_ start_time &&.
            (status ^. statusLastReported) <=. val_ end_time)
    pure status

-- | Expression to query information for stations by their IDs.
infoByIdExpr :: [Int32] -> Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s))
infoByIdExpr stationIds =
    filter_ (\info -> _infoStationId info `in_` map val_ stationIds)
    (all_ (bikeshareDb ^. bikeshareStationInformation))

-- | Insert station information into the database.
insertStationInformationExpr :: [AT.StationInformation] -> SqlInsert Postgres StationInformationT
insertStationInformationExpr stations =
  insertOnConflict (bikeshareDb ^. bikeshareStationInformation)
  (insertExpressions (map fromJSONToBeamStationInformation stations))
  anyConflict
  onConflictDoNothing

disabledDocksExpr :: Q Postgres BikeshareDb s (QGenExpr QValueContext Postgres s Text.Text, QGenExpr QValueContext Postgres s Int32)
disabledDocksExpr = do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- all_ (bikeshareDb ^. bikeshareStationStatus)
  guard_ (_statusStationId status `references_` info &&. status ^. statusNumDocksDisabled >. 0)
  pure ( info   ^. infoName
       , status ^. statusNumDocksDisabled
       )

queryStationStatusExpr :: Maybe Integer -> Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s), StationStatusT (QGenExpr QValueContext Postgres s))
queryStationStatusExpr limit = do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- case limit of
    Just limit' -> limit_ limit' $ all_ (bikeshareDb ^. bikeshareStationStatus)
    Nothing     ->                 all_ (bikeshareDb ^. bikeshareStationStatus)
  guard_ (_statusStationId status `references_` info)
  pure (info, status)

queryStationIdExpr :: String -> Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s))
queryStationIdExpr station_name = do
  info <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_infoName info ==. val_ (Text.pack station_name))
  pure info

queryStationIdLikeExpr :: String -> Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s))
queryStationIdLikeExpr station_name = do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_infoName info `like_` val_ (Text.pack station_name))
  pure info


-- | Expression to query the latest statuse not later than a given time for each station.
queryLatestStatusBeforeTimeExpr :: UTCTime -> Q Postgres BikeshareDb s (StationStatusT (QExpr Postgres s))
queryLatestStatusBeforeTimeExpr latestTime = do
  (stationId, maxTime) <-
    aggregate_ (\s -> ( group_ (_statusStationId    s)
                      , max_   (_statusLastReported s)
                      )
               ) $
    filter_ (\ss -> _statusLastReported ss <=. val_ latestTime) $
    all_ (bikeshareDb ^. bikeshareStationStatus)

  join_'
    (bikeshareDb ^. bikeshareStationStatus)
    (\status ->
       (stationId ==?. _statusStationId status            ) &&?.
       (maxTime ==?. just_ (_statusLastReported status))
    )
