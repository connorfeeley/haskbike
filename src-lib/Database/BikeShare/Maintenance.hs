{-# LANGUAGE PartialTypeSignatures #-}

{- |
Collection of functions to perform various maintainance tasks on the database
(fixing problems with tables, manual migrations, etc).

Should never be used from the main application.
-}

module Database.BikeShare.Maintenance
     ( statusWithInvalidInfo
     ) where

import           Control.Lens

import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres                       ( Postgres )
import           Database.BikeShare
import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationStatus


{-
Select all `station_status` rows which reference invalid `station_information` rows.

Matches SQL:
SELECT
    ss.*
FROM
    public.station_status AS ss
LEFT JOIN
    public.station_information AS si
    ON ss.info_station_id = si.station_id
    AND ss.info_reported = si.reported
WHERE
    ss.last_reported >= ('2024-01-01 00:00:00+00') AND
    (si.station_id IS NULL
    OR si.reported IS NULL);

Example:
>>> runWithAppMDebug "haskbike" $ withPostgres $ runSelectReturningList $ select $ statusWithInvalidInfo (UTCTime (fromGregorian 2024 01 01) (timeOfDayToTime midnight))
-}
statusWithInvalidInfo newerThanTimestamp = do
  status <- all_ (bikeshareDb ^. bikeshareStationStatus)
  info <- leftJoin_
          (all_ (bikeshareDb ^. bikeshareStationInformation))
          (\info ->
             let statusFk = (_statusInfoId . _statusCommon) status
             in
               -- statusFk `references_` info
               (_unInformationStationId statusFk ==._infoStationId info) &&.
               (_unInformationReported  statusFk ==._infoReported  info)
          )
  guard_' (sqlBool_ ((_statusLastReported . _statusCommon) status >=. val_ newerThanTimestamp))
  guard_ (isNothing_ (_infoStationId info) ||. isNothing_ (_infoReported  info))
  pure (status, info)
