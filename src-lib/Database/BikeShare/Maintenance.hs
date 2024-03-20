{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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
statusWithInvalidInfo :: UTCTime -> Q Postgres BikeshareDb s ( StationStatusT (QExpr Postgres s)
                                                             , StationInformationT (Nullable (QGenExpr QValueContext Postgres s))
                                                             )
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


{-
Get station information nearest to time parameter, without being newer.
Example:
>>> runWithAppMDebug "haskbike" $ withPostgres $ runSelectReturningList $ select $ infoNearest (val_ (UTCTime (fromGregorian 2024 01 01) (timeOfDayToTime midnight)))
-}
-- infoNearest :: _
--             -> Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s))
-- infoNearest status = do
--   Pg.pgNubBy_ _infoStationId $
--           orderBy_ (desc_ . _infoReported) $
--           filter_ (\info -> (_infoStationId info ==. (_statusStationId . _statusCommon)    status) &&.
--                             (_infoReported  info <=. (_statusLastReported . _statusCommon) status)
--                   ) $
--           all_ (bikeshareDb ^. bikeshareStationInformation)

-- statusInvalidWithValidInfo newerThanTimestamp = do
--   statusInvalid <- do
--     status <- all_ (bikeshareDb ^. bikeshareStationStatus)
--     info <- leftJoin_
--             (all_ (bikeshareDb ^. bikeshareStationInformation))
--             (\info ->
--                let statusFk = (_statusInfoId . _statusCommon) status
--                in
--                  -- statusFk `references_` info
--                  (_unInformationStationId statusFk ==._infoStationId info) &&.
--                  (_unInformationReported  statusFk ==._infoReported  info)
--             )
--     guard_' (sqlBool_ ((_statusLastReported . _statusCommon) status >=. val_ newerThanTimestamp))
--     guard_ (isNothing_ (_infoStationId info) ||. isNothing_ (_infoReported  info))
--     pure status

--   infoNearest <-
--           orderBy_ (desc_ . _infoReported) $
--           filter_ (\info -> (_infoStationId info ==. (_statusStationId . _statusCommon)    statusInvalid) &&.
--                             (_infoReported  info <=. (_statusLastReported . _statusCommon) statusInvalid)
--                   ) $
--           all_ (bikeshareDb ^. bikeshareStationInformation)
--   pure (statusInvalid, infoNearest)
