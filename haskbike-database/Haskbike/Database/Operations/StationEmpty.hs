{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- | Database operations to determine how long a station is empty for.

module Haskbike.Database.Operations.StationEmpty
     ( queryStationEmptyFullTime
     ) where

import           Control.Lens                                hiding ( reuse, (<.) )

import           Data.Int                                    ( Int32 )
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend                       ( BeamSqlBackend )
import           Database.Beam.Postgres                      ( Postgres )
import qualified Database.Beam.Postgres                      as Pg

import           Haskbike.AppEnv
import           Haskbike.Database.BikeShare
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus

-- | PostgreSQL GREATEST and LEAST.
greatest_, least_ :: QGenExpr ctxt Postgres s a -> QGenExpr ctxt Postgres s a -> QGenExpr ctxt Postgres s a
greatest_ = customExpr_ (\a b -> "GREATEST(" <> a <> ", " <> b <> ")")
least_    = customExpr_ (\a b -> "LEAST("    <> a <> ", " <> b <> ")")


-- | Difference between two epochs.
timeDelta :: (HasSqlTime tgt1, HasSqlTime tgt2, Integral b) => QGenExpr ctxt1 Postgres s tgt1 -> QGenExpr ctxt2 Postgres s tgt2 -> QGenExpr ctxt3 Postgres s b
timeDelta a b = cast_ (extract_ Pg.epoch_ a - extract_ Pg.epoch_ b) int

-- | Query how long each station has been both empty and full for.
queryStationEmptyFullTime :: (Integral a)
                          => Maybe a -> UTCTime -> UTCTime
                          -> Q Postgres BikeshareDb s ( StationInformationT (QGenExpr QValueContext Postgres s)
                                                      , (QGenExpr QValueContext Postgres s (Maybe Int32), QGenExpr QValueContext Postgres s (Maybe Int32))
                                                      )
queryStationEmptyFullTime stationId startTime endTime = do
  let statusCount = aggregate_ (\row -> ( group_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)
                                        , as_ @Int32 countAll_
                                        )
                               ) $
                    filter_ (\row -> between_ (row ^. statusLastReported) (val_ startTime) (val_ endTime)) $
                    all_ (bikeshareDb ^. bikeshareStationStatus)
  let infoStations = Pg.pgNubBy_ (\inf -> cast_ (_infoStationId inf) int) $
                     orderBy_ (\inf -> (asc_ (_infoStationId inf), desc_ (_infoReported inf))) $
                     filter_ (\inf -> _infoReported inf <=. val_ endTime) $
                     filter_ (infoStationIdCond stationId) $
                     all_ (bikeshareDb ^. bikeshareStationInformation)

  -- Get station ID and amount of seconds the station was empty and full.
  let emptyFullQuery =
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
          emptyFullRows stationId startTime endTime
  do
    (info, _statusCount') <- do
      -- Get the latest info not newer than the end time.
      info <- infoStations
      -- Join the station info with the status rows counts for each station, always including the info rows.
      (ssId, count) <- leftJoin_'
                       statusCount
                       (\(ssId, statusCount') -> ssId ==?. (info ^. infoStationId) &&?. statusCount' /=?. val_ 0)
      guard_ (isJust_ ssId &&. isJust_ count)
      pure (info, count)

    -- Join the station info with the station empty/full results, always including the info rows.
    (_sId, empty, full) <- leftJoin_'
                           emptyFullQuery
                           (\(sId', _, _) -> sId' ==?. (info ^. infoStationId))

    pure (info, (empty, full))

notEmptyRows stationId startTime endTime = do
  rows <- runWithAppM "haskbike" . withPostgres . runSelectReturningList . select $
    emptyFullRows stationId startTime endTime
  let notEmpty = filter (\r -> 0 /= (_statusNumBikesAvailable . _statusCommon . (^. _1)) r) rows
  pure notEmpty

filterKeepRows = filter_ (\(row, lead, lag) -> keepRow row lead lag)

emptyRows stationId startTime endTime = do
  rows <- runWithAppM "haskbike" . withPostgres . runSelectReturningList . select . filterKeepRows $
    emptyFullRows stationId startTime endTime
  let empty = filter (\r -> 0 == (_statusNumBikesAvailable . _statusCommon . (^. _1)) r) rows
  pure empty

allRows stationId startTime endTime = do
  runWithAppM "haskbike" $ withPostgres . runSelectReturningList . select $ emptyFullRows stationId startTime endTime

emptyFullRows stationId' startTime' endTime' =
  filter_ (\(row, (lead, _, _), (lag, _, _)) -> (row  ^. statusLastReported) <. val_ endTime' &&.
                                                lead >=. val_ startTime' ||.
                                                (lag  <. val_ startTime' &&. (row ^. statusLastReported) >=. val_ startTime')
          ) $
    withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)) (orderPartitionBy_ ((asc_ . _statusLastReported . _statusCommon) row)) noBounds_)
                (\row w -> ( row
                           , ( leadWithDefault_ (row ^. statusLastReported     ) (val_ 1) (val_ endTime')  `over_` w
                             , leadWithDefault_ (row ^. statusNumBikesAvailable) (val_ 1) (val_ 0) `over_` w
                             , leadWithDefault_ (row ^. statusNumDocksAvailable) (val_ 1) (val_ 0) `over_` w
                             )
                           , ( lagWithDefault_ (row ^. statusLastReported     ) (val_ 1) (val_ startTime') `over_` w
                             , lagWithDefault_ (row ^. statusNumBikesAvailable) (val_ 1) (val_ 0) `over_` w
                             , lagWithDefault_ (row ^. statusNumDocksAvailable) (val_ 1) (val_ 0) `over_` w
                             )
                           )
                ) $
        filter_ (stationIdCond stationId') $
        filter_ (\row -> between_ (row ^. statusLastReported) (val_ (addUTCTime (-12 * 60 * 60) startTime')) (val_ (addUTCTime (1 * 60 * 60) endTime'))) $
        all_ (bikeshareDb ^. bikeshareStationStatus)

keepRow :: ( BeamSqlBackend be, SqlValable b1, SqlValable b2, SqlValable a1,  SqlValable a2
           , SqlEq (QGenExpr context be s) b1,  SqlEq (QGenExpr context be s) b2, SqlEq (QGenExpr context be s) a1,  SqlEq (QGenExpr context be s) a2
           , SqlEq (QGenExpr context be s) (Columnar f Int32)
           , Num (Columnar f Int32), Num (HaskellLiteralForQExpr b1), Num (HaskellLiteralForQExpr b2), Num (HaskellLiteralForQExpr a1), Num (HaskellLiteralForQExpr a2)
           ) => StationStatusT f -> (t1, a1, b1) -> (t2, a2, b2) -> QGenExpr context be s Bool
keepRow row (leadReported, leadBikes, leadDocks) (lag, lagBikes, lagDocks) =
  keepForEmpty row (leadReported, leadBikes, leadDocks) (lag, lagBikes, lagDocks) -- ||.
  -- keepForFull  row (leadReported, leadBikes, leadDocks) (lag, lagBikes, lagDocks)

-- Keep row if significant to bikes available (empty: == 0) query.
keepForEmpty :: ( BeamSqlBackend be, SqlValable b1, SqlValable b2, SqlValable a1,  SqlValable a2
                , SqlEq (QGenExpr context be s) b1,  SqlEq (QGenExpr context be s) b2, SqlEq (QGenExpr context be s) a1,  SqlEq (QGenExpr context be s) a2
                , SqlEq (QGenExpr context be s) (Columnar f Int32)
                , Num (Columnar f Int32), Num (HaskellLiteralForQExpr b1), Num (HaskellLiteralForQExpr b2), Num (HaskellLiteralForQExpr a1), Num (HaskellLiteralForQExpr a2)
                ) => StationStatusT f -> (t1, a1, b1) -> (t2, a2, b2) -> QGenExpr context be s Bool
keepForEmpty row (_leadReported, leadBikes, _leadDocks) (_lagReported, lagBikes, _lagDocks) =
  isEmpty ||. (notEmpty &&. prevEmpty)
  where isEmpty      = row ^. statusNumBikesAvailable ==. 0
        notEmpty     = not_ isEmpty
        prevEmpty    = lagBikes  ==. val_ 0
        nextNotEmpty = not_ (leadBikes ==. val_ 0)

testStartTime = UTCTime (fromGregorian 2024 01 03) (timeOfDayToTime midnight)
testEndTime   = UTCTime (fromGregorian 2024 01 04) (timeOfDayToTime midnight)

-- Keep row if significant to docks available (full: == 0) query.
keepForFull :: ( BeamSqlBackend be, SqlValable b1, SqlValable b2, SqlValable a1,  SqlValable a2
               , SqlEq (QGenExpr context be s) b1,  SqlEq (QGenExpr context be s) b2, SqlEq (QGenExpr context be s) a1,  SqlEq (QGenExpr context be s) a2
               , SqlEq (QGenExpr context be s) (Columnar f Int32)
               , Num (Columnar f Int32), Num (HaskellLiteralForQExpr b1), Num (HaskellLiteralForQExpr b2), Num (HaskellLiteralForQExpr a1), Num (HaskellLiteralForQExpr a2)
               ) => StationStatusT f -> (t1, a1, b1) -> (t2, a2, b2) -> QGenExpr context be s Bool
keepForFull row (_leadReported, _leadBikes, leadDocks) (_lagReported, _lagBikes, lagDocks) =
  isFull ||. (notFull &&. prevFull)
  where isFull      = row ^. statusNumDocksAvailable ==. 0
        notFull     = not_ isFull
        prevFull    = lagDocks  ==. val_ 0
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


infoStationIdCond :: (HaskellLiteralForQExpr (expr Bool) ~ Bool, SqlEq expr (Columnar f Int32), Integral a, SqlValable (expr Bool), SqlValable (Columnar f Int32), Num (HaskellLiteralForQExpr (Columnar f Int32)))
                  => Maybe a -> StationInformationT f -> expr Bool
infoStationIdCond (Just stationId') row = _infoStationId row ==. val_ (fromIntegral stationId')
infoStationIdCond Nothing           _   = val_ True
