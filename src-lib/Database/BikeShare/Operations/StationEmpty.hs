{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- | Database operations to determine how long a station is empty for.

module Database.BikeShare.Operations.StationEmpty
     ( queryStationEmptyFullTime
     ) where

import           Control.Lens                                 hiding ( reuse, (<.) )

import           Data.Int                                     ( Int32 )
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend                        ( BeamSqlBackend, HasSqlValueSyntax )
import           Database.Beam.Postgres                       ( Postgres )
import qualified Database.Beam.Postgres                       as Pg
import qualified Database.Beam.Postgres.Full                  as Pg
import           Database.Beam.Postgres.Syntax                ( PgValueSyntax )
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
_queryStationEmptyFullTimeOld :: (Integral a)
                          => Maybe a
                          -> UTCTime
                          -> UTCTime
                          -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s), (QGenExpr QValueContext Postgres s Int32, QGenExpr QValueContext Postgres s Int32)))
_queryStationEmptyFullTimeOld stationId startTime endTime = do
  statusCte <- selecting $
            filter_ (stationIdCond stationId) $
            -- Widen our query a bit to get the previous and next status reports.
            filter_ (\row -> between_ (row ^. statusLastReported) (val_ (addUTCTime (-60 * 60 * 24) startTime)) (val_ (addUTCTime (60 * 60 * 24) endTime))) $
            all_ (bikeshareDb ^. bikeshareStationStatus)

  emptyFullCte <- selecting $
    aggregate_ (\(row, nReported, _pReported) ->
                  let period_start = greatest_ (row ^. statusLastReported) (val_ startTime)
                      period_end = least_ nReported (val_ endTime)
                   in ( group_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)
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
        withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row))
                                    (orderPartitionBy_ ((asc_ . _statusLastReported . _statusCommon) row))
                                    noBounds_)
                    (\row w -> ( row
                               , leadWithDefault_ (row ^. statusLastReported) (val_ 1) (val_ endTime) `over_` w
                               , lagWithDefault_  (row ^. statusLastReported) (val_ 1) (val_ endTime) `over_` w
                               )
                    ) $
        reuse statusCte

  pure $ do
    -- Get latest status so that we can get only the latest reference station info.
    latestStatus <- Pg.pgNubBy_ (\status -> cast_ ((_statusStationId . _statusCommon) status) int) $ orderBy_ (desc_ . _statusLastReported . _statusCommon) $ reuse statusCte

    -- Get latest info.
    info <- filter_ (\inf -> (_statusInfoId . _statusCommon) latestStatus `references_` inf) $
            all_ (bikeshareDb ^. bikeshareStationInformation)

    -- Get station ID and amount of seconds the station was empty and full.
    (sId, empty, full) <- reuse emptyFullCte

    guard_ ((info ^. infoStationId ==. sId) &&.
           (latestStatus ^. statusStationId ==. sId))

    pure (info, (empty, full))

-- | Query how long each station has been both empty and full for.
-- queryStationEmptyFullTime :: (Integral a)
--                           => Maybe a
--                           -> UTCTime
--                           -> UTCTime
--                           -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s), (QGenExpr QValueContext Postgres s Int32, QGenExpr QValueContext Postgres s Int32)))
-- queryStationEmptyFullTime :: (Integral a,  SqlJustable    (QGenExpr QValueContext Postgres s Int32)    (QGenExpr QValueContext Postgres s Int32))
--                           => Maybe a -> UTCTime -> UTCTime
--                           -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s), StationInformationT (Nullable (QGenExpr QValueContext Postgres s))))
-- queryStationEmptyFullTime :: (Integral a, SqlJustable (QGenExpr QValueContext Postgres s Int32) (QGenExpr QValueContext Postgres s Int32))
--                           => Maybe a -> UTCTime -> UTCTime
--                           -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (StationInformationT (Nullable (QGenExpr QValueContext Postgres s))))
-- queryStationEmptyFullTime :: (HasSqlValueSyntax PgValueSyntax a1, HasSqlValueSyntax PgValueSyntax a2, Integral a1, Integral a2, Integral a3, SqlJustable (QGenExpr QValueContext Postgres s Int32) (QGenExpr QValueContext Postgres s Int32))
--                           => Maybe a3 -> UTCTime -> UTCTime
--                           -> With Postgres BikeshareDb (Q Postgres BikeshareDb s ( StationInformationT (QGenExpr QValueContext Postgres s)
--                                                                                  , (QGenExpr QValueContext Postgres s (Maybe a1), QGenExpr QValueContext Postgres s (Maybe a2))
--                                                                                  )
--                                                        )
queryStationEmptyFullTime :: (Integral a)
                          => Maybe a -> UTCTime -> UTCTime
                          -> With Postgres BikeshareDb (Q Postgres BikeshareDb s ( StationInformationT (QGenExpr QValueContext Postgres s)
                                                                                 , (QGenExpr QValueContext Postgres s Int32, QGenExpr QValueContext Postgres s Int32)
                                                                                 )
                                                       )
queryStationEmptyFullTime stationId startTime endTime = do
  -- Get station ID and amount of seconds the station was empty and full.
  emptyFullCte <- selecting $
    aggregate_ (\(row, (nReported, _, _), _pReported) ->
                  let period_start = greatest_ (row ^. statusLastReported) (val_ startTime)
                      period_end = least_ nReported (val_ endTime)
                   in ( group_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)
                      -- 0 bikes available means the station is empty
                      , fromMaybe_ 0 (sum_ (ifThenElse_ (row ^. statusNumBikesAvailable ==. val_ 0)
                                             (timeDelta period_end period_start) 0
                                           ) `filterWhere_` (period_start <. period_end))
                      -- 0 docks available means the station is full
                      , fromMaybe_ 0 (sum_ (ifThenElse_ (row ^. statusNumDocksAvailable ==. val_ 0)
                                             (timeDelta period_end period_start) 0
                                           ) `filterWhere_` (period_start <. period_end))
                      )) $
      filter_ (\(row, lead, lag) -> keepRow row lead lag) $
      filter_ (\(row, (lead, _, _), (lag, _, _)) -> (row  ^. statusLastReported) <. val_ endTime &&.
                                    lead >=. val_ startTime ||. -- lead > start
                                    (lag  <.  val_ startTime &&. (row ^. statusLastReported) >=. val_ startTime) -- lag < start && row > start
              ) $
      withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)) (orderPartitionBy_ ((asc_ . _statusLastReported . _statusCommon) row)) noBounds_)
                  (\row w -> ( row
                             -- , leadWithDefault_ (_statusLastReported row, _statusLastReported row) (val_ 1, val_ 1) (val_ endTime, val_ endTime) `over_` w
                             , ( leadWithDefault_ (row ^. statusLastReported     ) (val_ 1) (val_ endTime) `over_` w
                               , leadWithDefault_ (row ^. statusNumBikesAvailable) (val_ 1) (val_ 0) `over_` w
                               , leadWithDefault_ (row ^. statusNumDocksAvailable) (val_ 1) (val_ 0) `over_` w
                               )
                             , ( lagWithDefault_ (row ^. statusLastReported     ) (val_ 1) (val_ endTime) `over_` w
                               , lagWithDefault_ (row ^. statusNumBikesAvailable) (val_ 1) (val_ 0) `over_` w
                               , lagWithDefault_ (row ^. statusNumDocksAvailable) (val_ 1) (val_ 0) `over_` w
                               )
                             )
                  ) $
          filter_ (stationIdCond stationId) $
          -- Widen our query a bit to get the previous and next status reports.
          filter_ (\row -> between_ (row ^. statusLastReported) (val_ (addUTCTime (-24 * 60 * 60) startTime)) (val_ (addUTCTime (24 * 60 * 60) endTime))) $
          all_ (bikeshareDb ^. bikeshareStationStatus)

  pure $ do
    (i, ef) <- do
      -- Get the latest info not newer than the end time.
      info <- Pg.pgNubBy_ (\inf -> cast_ (_infoStationId inf) int) $
              orderBy_ (\inf -> (asc_ (_infoStationId inf), desc_ (_infoReported inf))) $
              filter_ (\inf -> _infoReported inf <=. val_ endTime) $
              all_ (bikeshareDb ^. bikeshareStationInformation)

      -- Join the station info with the station empty/full results, always including the info rows.
      (_sId, empty, full) <- leftJoin_'
                          (reuse emptyFullCte)
                          (\(sId', _, _) -> sId' ==?. (info ^. infoStationId))

      -- If there are no status rows for a given station then the empty/full results will be NULL;
      -- default the empty time to be the time range, and default the full time to be 0.
      pure (info, ( fromMaybe_ nominalTimeRangeSeconds empty
                  , fromMaybe_ 0 full
                  )
           )
    pure (i, ef)
  where
    nominalTimeRangeSeconds = (fromInteger . round . nominalDiffTimeToSeconds) (diffUTCTime endTime startTime)


keepRow :: ( BeamSqlBackend be, SqlValable b1, SqlValable b2, SqlValable a1,  SqlValable a2
           , SqlEq (QGenExpr context be s) b1,  SqlEq (QGenExpr context be s) b2, SqlEq (QGenExpr context be s) a1,  SqlEq (QGenExpr context be s) a2
           , SqlEq (QGenExpr context be s) (Columnar f Int32)
           , Num (Columnar f Int32), Num (HaskellLiteralForQExpr b1), Num (HaskellLiteralForQExpr b2), Num (HaskellLiteralForQExpr a1), Num (HaskellLiteralForQExpr a2)
           ) => StationStatusT f -> (t1, a1, b1) -> (t2, a2, b2) -> QGenExpr context be s Bool
keepRow row (lead, leadBikes, leadDocks) (lag, lagBikes, lagDocks) = keepForEmpty row (lead, leadBikes, leadDocks) (lag, lagBikes, lagDocks) ||. keepForFull row (lead, leadBikes, leadDocks) (lag, lagBikes, lagDocks)

-- Keep row if significant to bikes available (empty: == 0) query.
keepForEmpty :: ( BeamSqlBackend be, SqlValable b1, SqlValable b2, SqlValable a1,  SqlValable a2
                , SqlEq (QGenExpr context be s) b1,  SqlEq (QGenExpr context be s) b2, SqlEq (QGenExpr context be s) a1,  SqlEq (QGenExpr context be s) a2
                , SqlEq (QGenExpr context be s) (Columnar f Int32)
                , Num (Columnar f Int32), Num (HaskellLiteralForQExpr b1), Num (HaskellLiteralForQExpr b2), Num (HaskellLiteralForQExpr a1), Num (HaskellLiteralForQExpr a2)
                ) => StationStatusT f -> (t1, a1, b1) -> (t2, a2, b2) -> QGenExpr context be s Bool
keepForEmpty row (_lead, leadBikes, _leadDocks) (_lag, lagBikes, _lagDocks) = (isEmpty &&. nextNotEmpty) ||. (notEmpty &&. prevNotEmpty)
  where isEmpty = row ^. statusNumBikesAvailable ==. 0
        notEmpty = not_ isEmpty
        prevNotEmpty = not_ (lagBikes ==. val_ 0)
        nextNotEmpty = not_ (leadBikes ==. val_ 0)

-- Keep row if significant to docks available (full: == 0) query.
keepForFull :: ( BeamSqlBackend be, SqlValable b1, SqlValable b2, SqlValable a1,  SqlValable a2
               , SqlEq (QGenExpr context be s) b1,  SqlEq (QGenExpr context be s) b2, SqlEq (QGenExpr context be s) a1,  SqlEq (QGenExpr context be s) a2
               , SqlEq (QGenExpr context be s) (Columnar f Int32)
               , Num (Columnar f Int32), Num (HaskellLiteralForQExpr b1), Num (HaskellLiteralForQExpr b2), Num (HaskellLiteralForQExpr a1), Num (HaskellLiteralForQExpr a2)
               ) => StationStatusT f -> (t1, a1, b1) -> (t2, a2, b2) -> QGenExpr context be s Bool
keepForFull row (_lead, _leadBikes, leadDocks) (_lag, _lagBikes, lagDocks) = (isFull &&. nextNotFull) ||. (notFull &&. prevNotFull)
  where isFull = row ^. statusNumDocksAvailable ==. 0
        notFull = not_ isFull
        prevNotFull = not_ (lagDocks ==. val_ 0)
        nextNotFull = not_ (leadDocks ==. val_ 0)

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
stationIdCond (Just stationId') row = (_unInformationStationId . _statusInfoId . _statusCommon) row ==. val_ (fromIntegral stationId')
stationIdCond Nothing           _   = val_ True
