{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedRecordDot   #-}
-- | Database operations to determine how long a station is empty for.

module Database.BikeShare.Operations.StationEmpty
     ( queryStationEmptyFullTime
     ) where

import           Control.Lens                                 hiding ( reuse, (<.) )

import           Data.Int                                     ( Int32 )
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres                       ( Postgres )
import qualified Database.Beam.Postgres                       as Pg
import           Database.Beam.Postgres.Full                  ( lateral_ )
import           Database.BikeShare
import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationStatus

-- | PostgreSQL GREATEST and LEAST.
greatest_, least_ :: QGenExpr ctxt Postgres s a -> QGenExpr ctxt Postgres s a -> QGenExpr ctxt Postgres s a
greatest_ = customExpr_ (\a b -> "GREATEST(" <> a <> ", " <> b <> ")")
least_    = customExpr_ (\a b -> "LEAST("    <> a <> ", " <> b <> ")")


-- | Difference between two epochs.
timeDelta :: (HasSqlTime tgt1, HasSqlTime tgt2, Integral b) => QGenExpr ctxt1 Postgres s tgt1 -> QGenExpr ctxt2 Postgres s tgt2 -> QGenExpr ctxt3 Postgres s b
timeDelta a b = cast_ (extract_ Pg.epoch_ a - extract_ Pg.epoch_ b) int


-- | Query how long each station has been both empty and full for.
queryStationEmptyFullTime :: (Integral a)
                          => Maybe a
                          -> UTCTime
                          -> UTCTime
                          -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s), (QGenExpr QValueContext Postgres s Int32, QGenExpr QValueContext Postgres s Int32)))
queryStationEmptyFullTime stationId startTime endTime = do
  statusCte <- selecting $
            filter_ (stationIdCond stationId) $
            -- Widen our query a bit to get the previous and next status reports.
            filter_ (\row -> between_ (row ^. statusLastReported) (val_ (addUTCTime (-60 * 60 * 24) startTime)) (val_ (addUTCTime (60 * 60 * 24) endTime))) $
            all_ (bikeshareDb ^. bikeshareStationStatus)

  emptyFullCte <- selecting $
    aggregate_ (\(row, nReported, _pReported) ->
                  let period_start = greatest_ (row ^. statusLastReported) (val_ startTime)
                      period_end = least_ nReported (val_ endTime)
                   in ( group_ ((_unInformationStationId  . _statusInfoId) row)
                      -- 0 bikes available means the station is empty
                      , fromMaybe_ 0 (sum_ (ifThenElse_ (row ^. statusNumBikesAvailable ==. val_ 0)
                                             (timeDelta period_end period_start) 0
                                           ) `filterWhere_` (period_start <. period_end))
                      -- 0 docks available means the station is full
                      , fromMaybe_ 0 (sum_ (ifThenElse_ (row ^. statusNumDocksAvailable ==. val_ 0)
                                             (timeDelta period_end period_start) 0
                                           ) `filterWhere_` (period_start <. period_end))
                      )) $
        filter_ (\row -> (row ^. _1 . statusLastReported) <. val_ endTime &&.
                         (row ^. _2) >=. val_ startTime ||. -- lead > start
                         (row ^. _3  <.  val_ startTime &&. (row ^. _1 . statusLastReported) >=. val_ startTime) -- lag < start && row > start
                ) $
        withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId  . _statusInfoId) row)) (orderPartitionBy_ ((asc_ . _statusLastReported) row)) noBounds_)
                    (\row w -> ( row
                               , leadWithDefault_ (row ^. statusLastReported) (val_ 1) (val_ endTime) `over_` w
                               , lagWithDefault_  (row ^. statusLastReported) (val_ 1) (val_ endTime) `over_` w
                               )
                    ) $
        reuse statusCte

  pure $ do
    -- Get latest status so that we can get only the latest reference station info.
    latestStatus <- Pg.pgNubBy_ (\inf -> cast_ (_statusStationId inf) int) $ orderBy_ (desc_ . _statusLastReported) $ reuse statusCte

    -- Get latest info.
    info <- filter_ (\inf -> _statusInfoId latestStatus `references_` inf) $
            all_ (bikeshareDb ^. bikeshareStationInformation)

    -- Get station ID and amount of seconds the station was empty and full.
    (sId, empty, full) <- reuse emptyFullCte

    guard_ ((info ^. infoStationId ==. sId) &&.
           (latestStatus ^. statusStationId ==. sId))

    pure (info, (empty, full))

-- | Possible filter condition for station ID.
stationIdCond :: ( HaskellLiteralForQExpr (expr Bool) ~ Bool
                 , SqlEq expr (Columnar f Int32)
                 , Integral a
                 , SqlValable (expr Bool)
                 , SqlValable (Columnar f Int32)
                 , Num (HaskellLiteralForQExpr (Columnar f Int32))
                 )
              => Maybe a
              -> StationStatusT f
              -> expr Bool
stationIdCond (Just stationId') row = (_unInformationStationId  . _statusInfoId) row ==. val_ (fromIntegral stationId')
stationIdCond Nothing           _   = val_ True
