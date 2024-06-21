-- | Database queries and expressions for station occupancy.

module Haskbike.Database.Operations.StationOccupancy
     ( lookupStationOccupancy
     , queryStationOccupancy
     , queryStationOccupancyE
     , stationOccupancyE
     ) where

import           Control.Lens                                hiding ( reuse, (<.) )
import           Control.Monad
import           Control.Monad.Catch                         ( MonadCatch )

import           Data.Int                                    ( Int32 )
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions    ( MonadBeamInsertReturning, conflictingFields,
                                                               runInsertReturningList )
import           Database.Beam.Postgres                      ( Postgres )
import qualified Database.Beam.Postgres                      as Pg
import qualified Database.Beam.Postgres.Full                 as Pg

import           Haskbike.AppEnv
import           Haskbike.Database.BikeShare
import           Haskbike.Database.Expressions
import           Haskbike.Database.Operations.Utils
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationOccupancy
import           Haskbike.Database.Tables.StationStatus

-- | PostgreSQL GREATEST and LEAST.
greatest_, least_ :: QGenExpr ctx Postgres s a -> QGenExpr ctx Postgres s a -> QGenExpr ctx Postgres s a
greatest_ = customExpr_ (\a b -> "GREATEST(" <> a <> ", " <> b <> ")")
least_    = customExpr_ (\a b -> "LEAST("    <> a <> ", " <> b <> ")")


-- | Query how long each station has been both empty and full for.
stationOccupancyE :: Integral a
                  => Maybe a -> UTCTime -> UTCTime
                  -> Q Postgres BikeshareDb s ( StationInformationT (QGenExpr QValueContext Postgres s)
                                              , (QGenExpr QValueContext Postgres s (Maybe Int32), QGenExpr QValueContext Postgres s (Maybe Int32))
                                              )
stationOccupancyE = stationOccupancyThreshE 0 0

-- | Query how long each station has been both empty and full for, with variable thresholds for being empty or full.
stationOccupancyThreshE :: Integral a
                        => Int32 -> Int32 -> Maybe a -> UTCTime -> UTCTime
                        -> Q Postgres BikeshareDb s ( StationInformationT (QGenExpr QValueContext Postgres s)
                                                    , (QGenExpr QValueContext Postgres s (Maybe Int32), QGenExpr QValueContext Postgres s (Maybe Int32))
                                                    )
stationOccupancyThreshE emptyThresh fullThresh stationId startTime endTime = do
  let statusCount = aggregate_ (\row -> ( group_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)
                                        , as_ @Int32 countAll_
                                        )
                               ) $
                    filter_ (\row -> between_ (row ^. statusLastReported) (val_ startTime) (val_ endTime)) $
                    all_ (bikeshareDb ^. bikeshareStationStatus)
  let infoStations = Pg.pgNubBy_ (\inf -> cast_ (_infoStationId inf) (int :: DataType Postgres Int32)) $
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
                          , fromMaybe_ 0 (sum_ (ifThenElse_ (row ^. statusNumBikesAvailable ==. val_ emptyThresh)
                                                 (timeDelta period_end period_start) 0
                                               ) `filterWhere_` (period_start <. period_end))
                          -- 0 docks available means the station is full
                          , fromMaybe_ 0 (sum_ (ifThenElse_ (row ^. statusNumDocksAvailable ==. val_ fullThresh)
                                                 (timeDelta period_end period_start) 0
                                               ) `filterWhere_` (period_start <. period_end))
                          )) $
          filter_ (\(row, lead, lag) -> keepRow emptyThresh fullThresh row lead lag) $
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


emptyFullRows :: Integral a
              => Maybe a -> UTCTime -> UTCTime
              -> Q Postgres BikeshareDb s (StationStatusT (QGenExpr QValueContext Postgres s), (QGenExpr QValueContext Postgres s UTCTime, QGenExpr QValueContext Postgres s Int32, QGenExpr QValueContext Postgres s Int32), (QGenExpr QValueContext Postgres s UTCTime, QGenExpr QValueContext Postgres s Int32, QGenExpr QValueContext Postgres s Int32))
emptyFullRows stationId' startTime' endTime' =
  filter_ (\(row, (lead, _, _), (lag, _, _)) -> (row  ^. statusLastReported) <. val_ endTime' &&.
                                                lead >=. val_ startTime' ||.
                                                (lag  <. val_ startTime' &&. (row ^. statusLastReported) >=. val_ startTime')
          ) $
    withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row))
                                (orderPartitionBy_
                                 ( (asc_ . _unInformationStationId . _statusInfoId . _statusCommon) row
                                 , (asc_ . _statusLastReported . _statusCommon) row)
                                )
                                noBounds_
                )
                (\row w -> ( row
                           , ( leadWithDefault_ (row ^. statusLastReported     ) (val_ (1 :: Int32)) (val_ endTime')  `over_` w
                             , leadWithDefault_ (row ^. statusNumBikesAvailable) (val_ (1 :: Int32)) (val_ 0) `over_` w
                             , leadWithDefault_ (row ^. statusNumDocksAvailable) (val_ (1 :: Int32)) (val_ 0) `over_` w
                             )
                           , ( lagWithDefault_ (row ^. statusLastReported     ) (val_ (1 :: Int32)) (val_ startTime') `over_` w
                             , lagWithDefault_ (row ^. statusNumBikesAvailable) (val_ (1 :: Int32)) (val_ 0) `over_` w
                             , lagWithDefault_ (row ^. statusNumDocksAvailable) (val_ (1 :: Int32)) (val_ 0) `over_` w
                             )
                           )
                ) $
        filter_ (stationIdCond stationId') $
        filter_ (\row -> between_ (row ^. statusLastReported) (val_ (addUTCTime (-12 * 60 * 60) startTime')) (val_ (addUTCTime (1 * 60 * 60) endTime'))) $
        all_ (bikeshareDb ^. bikeshareStationStatus)

-- * Functions to determine if a row should be considered in occupancy calculation.

keepRow :: (SqlValable (Columnar f Int32), SqlOrd (QGenExpr ctx Postgres s) (Columnar f Int32))
        => HaskellLiteralForQExpr (Columnar f Int32)
        -> HaskellLiteralForQExpr (Columnar f Int32)
        -> StationStatusT f
        -> (C f UTCTime, C f Int32, C f Int32)
        -> (C f UTCTime, C f Int32, C f Int32)
        -> QGenExpr ctx Postgres s Bool
keepRow emptyThresh fullThresh row (leadReported, leadBikes, leadDocks) (lag, lagBikes, lagDocks) =
  keepForEmpty emptyThresh fullThresh row (leadReported, leadBikes, leadDocks) (lag, lagBikes, lagDocks) ||.
  keepForFull  emptyThresh fullThresh row (leadReported, leadBikes, leadDocks) (lag, lagBikes, lagDocks)

-- Keep row if significant to bikes available (empty: == 0) query.
keepForEmpty :: (SqlValable (Columnar f Int32), SqlOrd (QGenExpr ctx Postgres s) (Columnar f Int32))
             => HaskellLiteralForQExpr (Columnar f Int32)
             -> HaskellLiteralForQExpr (Columnar f Int32)
             -> StationStatusT f
             -> (C f UTCTime, C f Int32, C f Int32)
             -> (C f UTCTime, C f Int32, C f Int32)
             -> QGenExpr ctx Postgres s Bool
keepForEmpty emptyThresh _fullThresh row (_leadReported, leadBikes, _leadDocks) (_lagReported, lagBikes, _lagDocks) =
  isEmpty ||. (notEmpty &&. prevEmpty)
  where isEmpty       = (row ^. statusNumBikesAvailable) <=. val_ emptyThresh
        notEmpty      = not_ isEmpty
        prevEmpty     = lagBikes  <=. val_ emptyThresh
        _nextNotEmpty = not_ (leadBikes <=. val_ emptyThresh)

-- Keep row if significant to docks available (full: == 0) query.
keepForFull :: (SqlValable (Columnar f Int32), SqlOrd (QGenExpr ctx Postgres s) (Columnar f Int32))
            => HaskellLiteralForQExpr (Columnar f Int32)
            -> HaskellLiteralForQExpr (Columnar f Int32)
            -> StationStatusT f
            -> (C f UTCTime, C f Int32, C f Int32)
            -> (C f UTCTime, C f Int32, C f Int32)
            -> QGenExpr ctx Postgres s Bool
keepForFull _emptyThresh fullThresh row (_leadReported, _leadBikes, leadDocks) (_lagReported, _lagBikes, lagDocks) =
  isFull ||. (notFull &&. prevFull)
  where isFull       = row ^. statusNumDocksAvailable <=. val_ fullThresh
        notFull      = not_ isFull
        prevFull     = lagDocks  <=. val_ fullThresh
        _nextNotFull = not_ (leadDocks <=. val_ fullThresh )


-- * Functions for querying station occupancy and caching results.

cacheStationOccupancy :: Integral a
                      => Int32 -> Int32 -> Maybe a -> UTCTime -> UTCTime
                      -> SqlInsert Postgres StationOccupancyT
cacheStationOccupancy emptyThresh fullThresh stationId startT endT =
    -- Can't return 'default_' from a query, so have to only insert specific columns with 'insertOnly'
    Pg.insertOnlyOnConflict (bikeshareDb ^. bikeshareStationOccupancy)
                            (\occ -> ( _stnOccInfo         occ
                                     , _stnOccRangeStart   occ
                                     , _stnOccRangeEnd     occ
                                     , _stnOccEmptyThresh  occ
                                     , _stnOccFullThresh   occ
                                     , _stnOccEmptySec     occ
                                     , _stnOccFullSec      occ
                                     ))
    (insertFrom (do
      (info, (empty, full)) <- stationOccupancyThreshE emptyThresh fullThresh stationId startT endT
      pure ( primaryKey info
           , as_ @UTCTime $ val_ startT
           , as_ @UTCTime $ val_ endT
           , as_ @Int32   $ val_ emptyThresh
           , as_ @Int32   $ val_ fullThresh
           , empty
           , full
           )))
    (conflictingFields primaryKey) Pg.onConflictUpdateAll


-- | Lookup station occupancy record from cache table.
lookupStationOccupancy :: Integral a
                       => Int32 -> Int32 -> Maybe a -> UTCTime -> UTCTime
                       -> Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s), StationOccupancyT (QExpr Postgres s))
lookupStationOccupancy emptyThresh fullThresh stationId startT endT = do
  occupancy <- all_ (_bikeshareStationOccupancy bikeshareDb)
  guard_' (_stnOccRangeStart occupancy  ==?. val_ startT        &&?.
           _stnOccRangeEnd   occupancy  ==?. val_ endT          &&?.
           _stnOccEmptyThresh occupancy ==?. val_ emptyThresh   &&?.
           _stnOccFullThresh  occupancy ==?. val_ fullThresh
          )

  info <- filter_' (\inf -> _stnOccInfo occupancy `references_'` inf) $
          filter_  (infoStationIdCond stationId) $
          all_ (_bikeshareStationInformation bikeshareDb)
  guard_ (_stnOccInfo occupancy ==. primaryKey info)
  pure (info, occupancy)


-- | Query the station occupancy, either returning cached data or caching it and returning the result.
queryStationOccupancy :: (HasEnv env m, MonadCatch m, Integral a)
                      => Int32 -> Int32 -> Maybe a -> UTCTime -> UTCTime
                      -> m [(StationInformation, StationOccupancy)]
queryStationOccupancy emptyThresh fullThresh stationId startT endT = do
  withPostgresTransaction $ queryStationOccupancyE emptyThresh fullThresh stationId startT endT

queryStationOccupancyE :: (Integral a, MonadBeamInsertReturning Postgres m)
                      => Int32 -> Int32 -> Maybe a -> UTCTime -> UTCTime
                      -> m [(StationInformation, StationOccupancy)]
queryStationOccupancyE emptyThresh fullThresh stationId startT endT = do
  occLookup <- runSelectReturningList . select $ lookupStationOccupancy 0 0 stationId startT endT
  case occLookup of
    -- No cached data found: calculate, store, and return it.
    [] -> do
      void $ runInsertReturningList $ cacheStationOccupancy emptyThresh fullThresh stationId startT endT
      runSelectReturningList . select $ lookupStationOccupancy emptyThresh fullThresh stationId startT endT

    -- Cached data found: return it.
    xs -> pure xs
