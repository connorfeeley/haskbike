{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE PartialTypeSignatures #-}

-- | This module contains expressions for querying the database.

module Haskbike.Database.Expressions
     ( disabledDocksExpr
     , infoByIdExpr
     , insertStationInformationExpr
     , integrateColumns
     , queryChargingInfrastructure
     , queryLatestInfo
     , queryLatestInfoBefore
     , queryLatestInfoLookup
     , queryLatestQueryLogs
     , queryLatestStatusBetweenExpr
     , queryLatestStatusLookup
     , queryLatestStatuses
     , queryLatestSystemInfo
     , queryStationBeforeExpr
     , queryStationIdExpr
     , queryStationIdLikeExpr
     , queryStationStatusExpr
     , querySystemStatusAtRangeExpr
     , statusBetweenExpr
     , statusInfoBetweenExpr
     , systemStatusBetweenExpr
     , timeDelta
     ) where

import           Control.Arrow                               ( (&&&) )
import           Control.Lens                                hiding ( reuse, (<.) )

import           Data.Containers.ListUtils
import           Data.Int                                    ( Int32 )
import qualified Data.Map                                    as Map
import           Data.Maybe                                  ( fromMaybe )
import           Data.String                                 ( IsString )
import qualified Data.Text                                   as T
import qualified Data.Text                                   as Text
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.Beam.Backend                       ( HasSqlValueSyntax, timestampType )
import           Database.Beam.Backend.SQL                   ( BeamSqlBackend, HasSqlValueSyntax (..) )
import           Database.Beam.Backend.SQL.BeamExtensions    ( BeamHasInsertOnConflict (anyConflict, onConflictDoNothing),
                                                               insertOnConflict )
import           Database.Beam.Postgres                      ( Postgres )
import qualified Database.Beam.Postgres                      as Pg
import           Database.Beam.Postgres.Full                 hiding ( insert )
import           Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query.Adhoc                   as Adhoc
import           Database.Beam.Query.CTE                     ( QAnyScope )

import qualified Haskbike.API.ResponseWrapper                as AT
import qualified Haskbike.API.StationInformation             as AT
import qualified Haskbike.API.StationStatus                  as AT
import qualified Haskbike.API.SystemInformation              as AT
import           Haskbike.AppEnv
import           Haskbike.Database.BikeShare
import           Haskbike.Database.DaysAgo
import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Tables.QueryLogs
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationLookup
import           Haskbike.Database.Tables.StationStatus
import           Haskbike.Database.Tables.SystemInformation
import           Haskbike.TimeInterval

import           Text.Pretty.Simple.Extras


-- | Difference between two epochs.
timeDelta :: (HasSqlTime t, Integral b)
          => QGenExpr ctx Postgres s t -> QGenExpr ctx Postgres s t -> QGenExpr ctx Postgres s b
timeDelta a b = cast_ (extract_ Pg.epoch_ a - extract_ Pg.epoch_ b) int


-- | Expression to query the all statuses for the system between two times.
systemStatusBetweenExpr :: UTCTime -> UTCTime -> Q Postgres BikeshareDb s (StationStatusT (QGenExpr QValueContext Postgres s))
systemStatusBetweenExpr start_time end_time =
  do
    info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
    status <- all_ (bikeshareDb ^. bikeshareStationStatus)
    guard_ ((_unInformationStationId . _statusInfoId . _statusCommon) status ==. _infoStationId info &&.
            between_  (status ^. statusLastReported) (val_ start_time) (val_ end_time)
           )
    pure status

-- | Expression to query the statuses for a station between two times.
statusBetweenExpr :: Int32 -> UTCTime -> UTCTime -> Q Postgres BikeshareDb s (StationStatusT (QGenExpr QValueContext Postgres s))
statusBetweenExpr stationId startTime endTime = do
  -- Never return two statuses for the same (_statusLastReported, (_unInformationStationId . _statusInfoId . _statusCommon)) - this breaks the graph rendering.
  -- This can happen if the station information changes (reported) without the status changing.
  status <- Pg.pgNubBy_ (_statusLastReported . _statusCommon &&& (_unInformationStationId . _statusInfoId . _statusCommon)) $
            orderBy_ (asc_ . _statusLastReported . _statusCommon)
            (all_ (bikeshareDb ^. bikeshareStationStatus))
  guard_ ((_unInformationStationId . _statusInfoId . _statusCommon) status ==. val_ stationId &&.
          between_ (status ^. statusLastReported) (val_ startTime) (val_ endTime))
  pure status

-- | Expression to query the station information and statuse for a station between two times.
statusInfoBetweenExpr :: Int32 -> UTCTime -> UTCTime
                      -> Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s), StationStatusT (QGenExpr QValueContext Postgres s))
statusInfoBetweenExpr stationId startTime endTime = do
  -- Never return two statuses for the same (_statusLastReported, (_unInformationStationId . _statusInfoId . _statusCommon)) - this breaks the graph rendering.
  -- This can happen if the station information changes (reported) without the status changing.
  status <- Pg.pgNubBy_ (_statusLastReported . _statusCommon &&& (_unInformationStationId . _statusInfoId . _statusCommon)) $
            orderBy_ (asc_ . _statusLastReported . _statusCommon)
            (all_ (bikeshareDb ^. bikeshareStationStatus))
  info   <- lateral_ status $ \status' -> do
            nub_ $ related_ (bikeshareDb ^. bikeshareStationInformation) ((_statusInfoId . _statusCommon) status')
  guard_ ((_unInformationStationId . _statusInfoId . _statusCommon) status ==. val_ stationId &&.
          between_ (status ^. statusLastReported) (val_ startTime) (val_ endTime))
  pure (info, status)

-- | Expression to query information for stations by their IDs.
infoByIdExpr :: [Int32] -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s)))
infoByIdExpr stationIds = do
  info         <- selecting $ all_ (bikeshareDb ^. bikeshareStationInformation)
  status       <- selecting $ all_ (bikeshareDb ^. bikeshareStationStatus)
  statusLookup <- selecting $ all_ (bikeshareDb ^. bikeshareStationLookup)
  pure $ do
    status' <- reuse status
    info' <- reuse info
    statusLookup' <- reuse statusLookup
    guard_' (_stnLookup statusLookup' `references_'` status')
    guard_' ((_statusInfoId . _statusCommon) status' `references_'` info')
    guard_ (_infoStationId info' `in_` map val_ stationIds)
    pure info'

-- | Insert station information into the database.
insertStationInformationExpr :: [(UTCTime, AT.StationInformation)] -> SqlInsert Postgres StationInformationT
insertStationInformationExpr stations =
  insert (bikeshareDb ^. bikeshareStationInformation)
  (insertExpressions (map (uncurry fromJSONToBeamStationInformation) stations))
  -- (conflictingFields primaryKey) (onConflictUpdateInstead (\i -> ( _infoName                    i
  --                                                                , _infoPhysicalConfiguration   i
  --                                                                , _infoCapacity                i
  --                                                                , _infoIsChargingStation       i
  --                                                                , _infoIsValetStation          i
  --                                                                , _infoIsVirtualStation        i
  --                                                                , _infoActive                  i
  --                                                                )
  --                                                         ))

disabledDocksExpr :: Q Postgres BikeshareDb s (QGenExpr QValueContext Postgres s Text.Text, QGenExpr QValueContext Postgres s Int32)
disabledDocksExpr = do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- all_ (bikeshareDb ^. bikeshareStationStatus)
  guard_ ((_unInformationStationId . _statusInfoId . _statusCommon) status ==. _infoStationId info &&. status ^. statusNumDocksDisabled >. 0)
  pure ( info   ^. infoName
       , status ^. statusNumDocksDisabled
       )

queryStationStatusExpr :: Maybe Integer -> Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s), StationStatusT (QGenExpr QValueContext Postgres s))
queryStationStatusExpr limit = do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- case limit of
    Just limit' -> limit_ limit' $ all_ (bikeshareDb ^. bikeshareStationStatus)
    Nothing     ->                 all_ (bikeshareDb ^. bikeshareStationStatus)
  guard_ ((_unInformationStationId . _statusInfoId . _statusCommon) status ==. _infoStationId info)
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


-- | Expression to query the latest statuses not later than a given time for each station.
queryLatestStatusBetweenExpr :: UTCTime -> UTCTime -> Q Postgres BikeshareDb s (StationStatusT (QExpr Postgres s))
queryLatestStatusBetweenExpr earliestTime latestTime = do
  (stationId, maxTime) <-
    aggregate_ (\s -> ( group_ ((_unInformationStationId . _statusInfoId . _statusCommon)    s)
                      , max_   (s ^. statusLastReported)
                      )
               ) $
    filter_ (\ss -> between_ (ss ^. statusLastReported) (val_ earliestTime) (val_ latestTime)) $
    all_ (bikeshareDb ^. bikeshareStationStatus)

  join_'
    (bikeshareDb ^. bikeshareStationStatus)
    (\status ->
       sqlBool_ (between_ (status ^. statusLastReported) (val_ earliestTime) (val_ latestTime)) &&?.
       (stationId ==?. (_unInformationStationId . _statusInfoId . _statusCommon) status            ) &&?.
       (maxTime ==?. just_ (status ^. statusLastReported))
    )


-- | Expression to query the latest statuses not later than a given time for each station.
queryStationBeforeExpr :: UTCTime
                       -> Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s), StationStatusT (QExpr Postgres s))
queryStationBeforeExpr latestTime = do
  -- Get latest statuses before the given time (excluding status records older than a day prior).
  status <- queryLatestStatusBetweenExpr (addUTCTime (-60*60*24) latestTime) latestTime

  -- Query all station information.
  info   <- -- lateral_ status $ \status' -> do
              related_ (bikeshareDb ^. bikeshareStationInformation) ((_statusInfoId . _statusCommon) status)
  guard_' ((_statusInfoId . _statusCommon) status `references_'` info)
  pure (info, status)

mkTime :: UTCTime -> QGenExpr ctxt Postgres s b
mkTime  = (`cast_` (DataType $ timestampType Nothing True)) . val_

{- Expression to query aggregate information from the latest statuses not later than a given time for each station.

>>> (AppEnv.runWithAppMDebug "haskbike" . withPostgres . runSelectReturningList . selectWith) $
      querySystemStatusAtRangeExpr
      (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 06 00 00)))
      (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 06 45 00)))
      (minsPerHourlyInterval 4) -- 15 minutes
[ (2023-10-27 06:00:00 UTC, Just 5998, Just 301, Just 7216, Just 69, Just 5742, Just 70, Just 186)
, (2023-10-27 06:15:00 UTC, Just 5991, Just 304, Just 7221, Just 69, Just 5731, Just 70, Just 190)
, (2023-10-27 06:30:00 UTC, Just 6004, Just 296, Just 7192, Just 70, Just 5739, Just 70, Just 195)
, (2023-10-27 06:45:00 UTC, Just 6023, Just 285, Just 7186, Just 68, Just 5755, Just 73, Just 195)
]
-}
querySystemStatusAtRangeExpr :: UTCTime -> UTCTime -> Integer -> With Postgres BikeshareDb (Q Postgres
                    BikeshareDb
                    s
                    (QGenExpr QValueContext Postgres s UTCTime,
                     QGenExpr QValueContext Postgres s Int32,
                     QGenExpr QValueContext Postgres s Int32,
                     QGenExpr QValueContext Postgres s Int32,
                     QGenExpr QValueContext Postgres s Int32,
                     QGenExpr QValueContext Postgres s Int32,
                     QGenExpr QValueContext Postgres s Int32,
                     QGenExpr QValueContext Postgres s Int32))
querySystemStatusAtRangeExpr earliestTime latestTime interval = do
  timeIntervals <- selecting $
    values_ $ map (\t -> (mkTime (addUTCTime (-60 * fromIntegral interval) t), mkTime t)) $
    generateTimeRange earliestTime latestTime (fromIntegral interval)

  pure $ aggregate_ (\(i, s) -> ( group_ i
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (s ^. statusNumBikesAvailable)))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (s ^. statusNumBikesDisabled )))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (s ^. statusNumDocksAvailable)))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (s ^. statusNumDocksDisabled )))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (s ^. vehicleTypesAvailableIconic)))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (s ^. vehicleTypesAvailableEfit  )))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (s ^. vehicleTypesAvailableEfitG5)))
                                ) ) $ do
    intervals' <- reuse timeIntervals
    latestStatusKeys <-
      aggregate_ (\(i, s) -> ( group_ (snd i)
                             , group_ ((_unInformationStationId . _statusInfoId . _statusCommon) s)
                             , maxOver_ distinctInGroup_ (s ^. statusLastReported) `filterWhere_` uncurry (between_ (s ^. statusLastReported)) i
                             )
                 ) $ do
      intervals <- reuse timeIntervals
      statuses' <- join_ (bikeshareDb ^. bikeshareStationStatus)
                (\status -> (status ^. statusLastReported) >=. fst intervals &&. (status ^. statusLastReported) <. snd intervals)
      guard_ (uncurry (between_ (statuses' ^. statusLastReported)) intervals)
      pure (intervals, statuses')

    latestStatuses <- join_' (bikeshareDb ^. bikeshareStationStatus)
                      (\status -> (_unInformationStationId . _statusInfoId . _statusCommon) status ==?. (latestStatusKeys ^. _2) &&?. just_ (status ^. statusLastReported) ==?. (latestStatusKeys ^. _3))
    intervals <- reuse timeIntervals

    guard_ (latestStatusKeys ^. _1 ==. snd intervals' &&.
            (_unInformationStationId . _statusInfoId . _statusCommon) latestStatuses ==. latestStatusKeys ^. _2 &&.
            uncurry (between_ (latestStatuses ^. statusLastReported)) intervals)
    pure (snd intervals, latestStatuses)


-- * Integrals of fields.

-- Run with:
--   withPostgres $ runSelectReturningList $ selectWith $ integrateColumns (StatusVariationQuery (Just (7001)) [EarliestTime (UTCTime (fromGregorian 2023 11 18) (timeOfDayToTime midnight)), LatestTime (UTCTime (fromGregorian 2023 11 19) (timeOfDayToTime midnight))])
-- | Query the number of charging events for a station (returning status record and tuples of (dDisabled, dEfit, dEfitG5, sumDisabled, sumEfit, sumEfitG5).
integrateColumns :: be ~ Postgres
                 => StatusVariationQuery
                 -> With
                      be
                      BikeshareDb
                      (Q be BikeshareDb s
                        ( QGenExpr QValueContext be s Int32
                        , QGenExpr QValueContext be s Bool    -- ^ Charging station
                        , QGenExpr QValueContext be s Int32   -- ^ Station capacity
                        , QGenExpr QValueContext be s Int32   -- ^ Total seconds
                        , ( QGenExpr QValueContext be s Int32 -- ^ Integral of number of bikes available (sum of (time delta * bikes available) over rows)
                          , QGenExpr QValueContext be s Int32 -- ^ Integral of number of bikes disabled  (sum of (time delta * bikes disabled)  over rows)
                          , QGenExpr QValueContext be s Int32 -- ^ Integral of number of docks available (sum of (time delta * docks available) over rows)
                          , QGenExpr QValueContext be s Int32 -- ^ Integral of number of docks disabled  (sum of (time delta * docks disabled)  over rows)
                        )
                        , ( QGenExpr QValueContext be s Int32 -- ^ Integral of number of iconic   bikes available (sum of (time delta * iconic   available) over rows)
                          , QGenExpr QValueContext be s Int32 -- ^ Integral of number of e-fit    bikes disabled  (sum of (time delta * e-fit    available) over rows)
                          , QGenExpr QValueContext be s Int32 -- ^ Integral of number of e-fit g5 bikes disabled  (sum of (time delta * e-fit g5 available) over rows)
                          )
                        )
                      )
integrateColumns variation = do
  -- Lag expression
  lagged <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)) (orderPartitionBy_ ((asc_ . _statusLastReported . _statusCommon) row)) noBounds_)
                     (\row w -> ( row
                                , lagWithDefault_ (row ^. statusLastReported) (val_ (1 :: Integer)) (row ^. statusLastReported) `over_` w
                                )
                     ) statusForStation

  -- Difference between row values and lagged values
  withDeltas <- selecting $ do
    -- Calculate delta between current and previous availability.
    withWindow_ (\(row, _) -> frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)) noOrder_ noBounds_)
       (\(row, pLastReported) _w ->
           ( row                                                                                  -- _1
           , as_ @Int32 (timeDelta (row ^. statusLastReported) pLastReported)                     -- _2
           , ( row ^. statusNumBikesAvailable * timeDelta (row ^. statusLastReported) pLastReported
             , row ^. statusNumBikesDisabled  * timeDelta (row ^. statusLastReported) pLastReported
             , row ^. statusNumDocksAvailable * timeDelta (row ^. statusLastReported) pLastReported
             , row ^. statusNumDocksDisabled  * timeDelta (row ^. statusLastReported) pLastReported
             )
           , ( row ^. vehicleTypesAvailableIconic  * timeDelta (row ^. statusLastReported) pLastReported
             , row ^. vehicleTypesAvailableEfit    * timeDelta (row ^. statusLastReported) pLastReported
             , row ^. vehicleTypesAvailableEfitG5  * timeDelta (row ^. statusLastReported) pLastReported
             )
           )
       ) (reuse lagged)
  chargings <- selecting $ reuse withDeltas

  pure $ do
    chargingsSum <-
      aggregate_ (\( status
                   , dLastReported
                   , ( secondsBikesAvailable
                     , secondsBikesDisabled
                     , secondsDocksAvailable
                     , secondsDocksDisabled)
                   , ( secondsIconicAvailable
                     , secondsEfitAvailable
                     , secondsEfitG5Available)
                   ) -> ( group_       ((_unInformationStationId . _statusInfoId . _statusCommon) status)
                        , fromMaybe_ 0 (sum_ dLastReported)
                        , ( fromMaybe_ 0 (sum_ secondsBikesAvailable)
                          , fromMaybe_ 0 (sum_ secondsBikesDisabled)
                          , fromMaybe_ 0 (sum_ secondsDocksAvailable)
                          , fromMaybe_ 0 (sum_ secondsDocksDisabled)
                          )
                        , ( fromMaybe_ 0 (sum_ secondsIconicAvailable)
                          , fromMaybe_ 0 (sum_ secondsEfitAvailable)
                          , fromMaybe_ 0 (sum_ secondsEfitG5Available)
                          )
                        )
                 ) (reuse chargings)

    info <- all_ (bikeshareDb ^. bikeshareStationInformation)
    guard_ ((chargingsSum ^. _1) ==. _infoStationId info &&. _infoActive info ==. val_ True)

    pure ( chargingsSum ^. _1            -- Station ID
         , info ^. infoIsChargingStation -- Is charging station
         , info ^. infoCapacity          -- Station capacity
         , chargingsSum ^. _2            -- Total seconds
         , chargingsSum ^. _3            -- Integrals of (bikes available, bikes disabled, docks available, docks disabled)
         , chargingsSum ^. _4            -- Integrals of (iconic available, efit available, efit g5 available)
         )

-- | Get the latest status records for each station.
-- queryLatestStatuses :: be ~ Postgres
--                     => With be BikeshareDb
--                     (Q be BikeshareDb s (StationInformationT (QExpr be s), StationStatusT (QGenExpr QValueContext be s)))
queryLatestStatuses :: Q Postgres BikeshareDb s (StationInformationT (QExpr Postgres s), StationStatusT (QExpr Postgres s))
queryLatestStatuses = do
  info    <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status  <- -- filter_ (\s -> s ^. statusLastReported >=. (daysAgo_ . val_ . DaysAgo) 1) $
             all_ (bikeshareDb ^. bikeshareStationStatus)
  station <- all_ (bikeshareDb ^. bikeshareStationLookup)

  guard_' ( _stnLookup station                       `references_'` status &&?.
           (_unStatusStationId . _stnLookup) station `references_'` info
          )
  pure (info, status)

-- | Get the latest status records for each station.
queryLatestSystemInfo :: be ~ Postgres
                      => With be BikeshareDb
                      (Q be BikeshareDb s
                       (SystemInformationCountT (QGenExpr QValueContext Postgres s)))
queryLatestSystemInfo = do
  sysInfoCte <- selecting $
    Pg.pgNubBy_ (\cnt -> (_sysInfCntStationCount cnt, _sysInfCntMechanicalCount cnt, _sysInfCntEbikeCount cnt)) $
    orderBy_ (asc_ . (_sysInfKeyReported . _sysInfCntKey))
    (all_ (bikeshareDb ^. bikeshareSystemInformationCount))

  pure $ reuse sysInfoCte

-- | Get the latest query logs for each endpoint.
queryLatestQueryLogs :: be ~ Postgres
                     => With be BikeshareDb
                     (Q be BikeshareDb s
                      (QueryLogT (QExpr be s)))
queryLatestQueryLogs = do
  ranked <- selecting $ do
    withWindow_ (\row -> frame_ (partitionBy_ (_queryLogEndpoint row)) (orderPartitionBy_ (desc_ $ _queryLogTime row)) noBounds_)
                (\row w -> ( row
                           , rank_ `over_` w
                           )
                ) (all_ (bikeshareDb ^. bikeshareQueryLog))
  pure $ do
    partitioned <- reuse ranked
    guard_ (partitioned ^. _2 ==. 1) -- Is max rank (latest record in partition)
    pure (partitioned ^. _1)

-- | Get the latest query logs for each endpoint.
queryLatestInfo :: be ~ Postgres
                      => With be BikeshareDb
                      (Q be BikeshareDb s
                       (StationInformationT (QExpr be s)))
queryLatestInfo = do
  ranked <- selecting $ do
    withWindow_ (\row -> frame_ (partitionBy_ (_infoStationId row)) (orderPartitionBy_ (desc_ $ _infoId row)) noBounds_)
                (\row w -> ( row
                           , rank_ `over_` w
                           )
                ) (all_ (bikeshareDb ^. bikeshareStationInformation))
  pure $ do
    partitioned <- reuse ranked
    guard_ (partitioned ^. _2 ==. 1) -- Is max rank (latest record in partition)
    pure (partitioned ^. _1)

queryLatestInfoBefore :: (be ~ Postgres, ctx ~ QValueContext, db ~ BikeshareDb, expr ~ QGenExpr)
                      => UTCTime
                      -> With be db (Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s)))
queryLatestInfoBefore t = do
  ranked <- selecting $ do
    withWindow_ (\row -> frame_ (partitionBy_ (_infoStationId row)) (orderPartitionBy_ (desc_ $ _infoId row)) noBounds_)
                (\row w -> ( row
                           , rank_ `over_` w
                           )
                ) $
      filter_' (\inf -> sqlBool_ (_infoReported inf  <=. val_ t)
               ) (all_ (bikeshareDb ^. bikeshareStationInformation))
  pure $ do
    info <- filter_ (\inf -> inf ^. _2 ==. val_ 1) (reuse ranked)
    pure (info ^. _1)

queryLatestStatusLookup :: Maybe DaysAgo -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (StationStatusT (QGenExpr QValueContext Postgres s)))
queryLatestStatusLookup days = do
  status <- selecting $ maybeFilterDaysAgo statusLastReported days $ all_ (bikeshareDb ^. bikeshareStationStatus)
  statusLookup <- selecting $ all_ (bikeshareDb ^. bikeshareStationLookup)
  pure $ do
    status' <- reuse status
    statusLookup' <- reuse statusLookup
    guard_' (_stnLookup statusLookup' `references_'` status')
    pure status'

queryLatestInfoLookup :: Maybe DaysAgo -> With Postgres BikeshareDb (Q Postgres BikeshareDb s (StationInformationT (QGenExpr QValueContext Postgres s)))
queryLatestInfoLookup days = do
  info    <- selecting $ all_ (bikeshareDb ^. bikeshareStationInformation)
  status  <- selecting $ maybeFilterDaysAgo statusLastReported days $ all_ (bikeshareDb ^. bikeshareStationStatus)
  station <- selecting $ all_ (bikeshareDb ^. bikeshareStationLookup)
  pure $ do
    status' <- reuse status
    info' <- reuse info
    station' <- reuse station
    guard_' (_stnLookup station' `references_'` status')
    guard_' ((_unStatusStationId . _stnLookup) station' `references_'` info')
    pure info'

-- | If 'days' is Nothing, don't filter. Otherwise, filter such that 'reportedField' is as recent as 'days' ago.
maybeFilterDaysAgo :: Getting (QGenExpr QValueContext Postgres s1 UTCTime) s2 (QGenExpr QValueContext Postgres s1 UTCTime) -> Maybe DaysAgo -> Q Postgres db s1 s2 -> Q Postgres db s1 s2
maybeFilterDaysAgo reportedField (Just days) = filter_ (\s -> s ^. reportedField >=. (daysAgo_ . val_) days)
maybeFilterDaysAgo _ Nothing                 = id

-- | Query charging infrastructure at given time.
queryChargingInfrastructure :: (be ~ Postgres, ctx ~ QValueContext, db ~ BikeshareDb, expr ~ QGenExpr)
                            => UTCTime
                            -> With be db (Q be db s (expr ctx be s Int32, expr ctx be s Int32))
                            -- ^ (Maybe) a tuple of (number of charging stations, number of charging docks)
queryChargingInfrastructure t = do
  ranked <- selecting $ do
    withWindow_ (\row -> frame_ (partitionBy_ (_infoStationId row)) (orderPartitionBy_ (desc_ $ _infoId row)) noBounds_)
                (\row w -> ( row
                           , rank_ `over_` w
                           )
                ) $
      filter_' (\inf -> sqlBool_ (_infoReported inf  <=. val_ t) &&?.
                        (_infoIsChargingStation inf ==?. val_ True)
               ) (all_ (bikeshareDb ^. bikeshareStationInformation))
  pure $
    aggregate_ (\(inf, _rank) -> ( as_ @Int32 countAll_
                                 , fromMaybe_ 0 (sum_ (_infoCapacity inf))
                                 )
               ) $
    filter_ (\inf -> inf ^. _2 ==. val_ 1) (reuse ranked)
