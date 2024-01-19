{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- | Database operations to determine how long a station is empty for.

module Database.BikeShare.Operations.StationEmpty
     ( queryStationEmptyTime
     ) where

import           Control.Lens                                 hiding ( reuse, (<.) )

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


-- | Query how long each station has been empty for.
queryStationEmptyTime :: UTCTime -> UTCTime -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s), QGenExpr QValueContext Postgres s Integer))
queryStationEmptyTime startTime endTime = do
  statusCte <- selecting $
            filter_ (\row -> between_ (row ^. statusLastReported) (val_ (addUTCTime (-60 * 60 * 24) startTime)) (val_ (addUTCTime (60 * 60 * 24) endTime))) $
            all_ (bikeshareDb ^. bikeshareStationStatus)

  emptyIntervals <- selecting $
    aggregate_ (\(row, nReported, _pReported) ->
                  let period_start = greatest_ (row ^. statusLastReported) (val_ startTime)
                      period_end = least_ nReported (val_ endTime)
                   in ( group_ ((_unInformationStationId  . _statusInfoId) row)
                      , fromMaybe_ 0 (sum_ (ifThenElse_ (row ^. statusNumBikesAvailable ==. val_ 0)
                                                (timeDelta period_end period_start)
                                                0
                                           ) `filterWhere_` (period_start <. period_end))
                      )) $
        filter_ (\row -> (row ^. _1 . statusLastReported) <. val_ endTime &&.
                          (row ^. _2) >=. val_ startTime
                         ||. (row ^. _3 <. val_ startTime &&. (row ^. _1 . statusLastReported) >=. val_ startTime)
                ) $
        withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId  . _statusInfoId) row)) (orderPartitionBy_ ((asc_ . _statusLastReported) row)) noBounds_)
                    (\row w -> ( row
                               , leadWithDefault_ (row ^. statusLastReported) (val_ 1) (val_ endTime) `over_` w
                               , lagWithDefault_  (row ^. statusLastReported) (val_ 1) (val_ endTime) `over_` w
                               )
                    ) $
        reuse statusCte

  pure $ do
    empty <- reuse emptyIntervals
    status <- reuse statusCte

    info <- do
      info' <- lateral_ status $ \status' -> do
        filter_ (\inf -> _statusInfoId status' `references_` (inf ^. _1) &&. inf ^. _2 ==. val_ 1) $
          withWindow_ (\row -> frame_ (partitionBy_ (_infoStationId row)) (orderPartitionBy_ ((desc_ . _infoReported) row)) noBounds_)
                         (\row w -> ( row
                                    , rank_ `over_` w
                                    )
                         ) $
          all_ (bikeshareDb ^. bikeshareStationInformation)
      pure (info' ^. _1)

    guard_ (_statusInfoId status `references_` info &&. empty ^. _1 ==. _statusStationId status &&. empty ^. _1 ==. _infoStationId info)

    pure (info, empty ^. _2)
