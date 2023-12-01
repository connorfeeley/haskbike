{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | This module contains expressions for querying the database.

module Database.BikeShare.Expressions
     ( disabledDocksExpr
     , infoByIdExpr
     , insertStationInformationExpr
     , integrateColumns
     , queryLatestStatusBetweenExpr
     , queryStationIdExpr
     , queryStationIdLikeExpr
     , queryStationStatusExpr
     , querySystemStatusAtRangeExpr
     , statusBetweenExpr
     , systemStatusBetweenExpr
     ) where


import qualified API.Types                                as AT

import           AppEnv

import           Control.Lens                             hiding ( reuse, (<.) )

import           Data.Containers.ListUtils
import           Data.Int                                 ( Int32 )
import           Data.String                              ( IsString )
import qualified Data.Text                                as T
import qualified Data.Text                                as Text
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.Beam.Backend                    ( timestampType )
import           Database.Beam.Backend.SQL                ( BeamSqlBackend )
import           Database.Beam.Backend.SQL.BeamExtensions ( BeamHasInsertOnConflict (anyConflict, onConflictDoNothing),
                                                            insertOnConflict )
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Full
import           Database.Beam.Postgres.Syntax
import qualified Database.Beam.Query.Adhoc                as Adhoc
import           Database.BikeShare
import           Database.BikeShare.StatusVariationQuery

import           Text.Pretty.Simple.Extras

import           TimeInterval


-- | Expression to query the all statuses for the system between two times.
systemStatusBetweenExpr :: UTCTime -> UTCTime -> Q Postgres BikeshareDb s (StationStatusT (QGenExpr QValueContext Postgres s))
systemStatusBetweenExpr start_time end_time =
  do
    info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
    status <- all_ (bikeshareDb ^. bikeshareStationStatus)
    guard_ (_statusStationId status `references_` info &&.
            between_  (status ^. statusLastReported) (val_ start_time) (val_ end_time)
           )
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
            between_ (status ^. statusLastReported) (val_ start_time) (val_ end_time))
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
  (conflictingFields primaryKey) (onConflictUpdateInstead (\i -> ( _infoName                    i
                                                                 , _infoPhysicalConfiguration   i
                                                                 , _infoCapacity                i
                                                                 , _infoIsChargingStation       i
                                                                 , _infoIsValetStation          i
                                                                 , _infoIsVirtualStation        i
                                                                 )
                                                          ))

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


-- | Expression to query the latest statuses not later than a given time for each station.
queryLatestStatusBetweenExpr :: UTCTime -> UTCTime -> Q Postgres BikeshareDb s (StationStatusT (QExpr Postgres s))
queryLatestStatusBetweenExpr earliestTime latestTime = do
  (stationId, maxTime) <-
    aggregate_ (\s -> ( group_ (_statusStationId    s)
                      , max_   (_statusLastReported s)
                      )
               ) $
    filter_ (\ss -> between_ (_statusLastReported ss) (val_ earliestTime) (val_ latestTime)) $
    all_ (bikeshareDb ^. bikeshareStationStatus)

  join_'
    (bikeshareDb ^. bikeshareStationStatus)
    (\status ->
       (stationId ==?. _statusStationId status            ) &&?.
       (maxTime ==?. just_ (_statusLastReported status))
    )

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
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (_statusNumBikesAvailable s)))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (_statusNumBikesDisabled  s)))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (_statusNumDocksAvailable s)))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (_statusNumDocksDisabled  s)))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (s ^. vehicleTypesAvailableIconic)))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (s ^. vehicleTypesAvailableEfit  )))
                                , as_ @Int32 (fromMaybe_ 0 (sumOver_ allInGroup_ (s ^. vehicleTypesAvailableEfitG5)))
                                ) ) $ do
    intervals' <- reuse timeIntervals
    latestStatusKeys <-
      aggregate_ (\(i, s) -> ( group_ (snd i)
                             , group_ (_statusStationId s)
                             , maxOver_   distinctInGroup_ (_statusLastReported s) `filterWhere_` uncurry (between_ (_statusLastReported s)) i
                             )
                 ) $ do
      intervals <- reuse timeIntervals
      statuses' <- join_ (bikeshareDb ^. bikeshareStationStatus)
                (\status -> _statusLastReported status >=. fst intervals &&. _statusLastReported status <. snd intervals)
      guard_ (uncurry (between_ (_statusLastReported statuses')) intervals)
      pure (intervals, statuses')

    latestStatuses <- join_' (bikeshareDb ^. bikeshareStationStatus)
                      (\status -> _statusStationId status ==?. (latestStatusKeys ^. _2) &&?. just_ (_statusLastReported status) ==?. (latestStatusKeys ^. _3))
    intervals <- reuse timeIntervals

    guard_ (latestStatusKeys ^. _1 ==. snd intervals' &&.
            _statusStationId latestStatuses ==. latestStatusKeys ^. _2 &&.
            uncurry (between_ (_statusLastReported latestStatuses)) intervals)
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
                        ( PrimaryKey StationInformationT (QGenExpr QValueContext be s)
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
      in withWindow_ (\row -> frame_ (partitionBy_ (_statusStationId row)) (orderPartitionBy_ (asc_ $ _statusLastReported row)) noBounds_)
                     (\row w -> ( row
                                , lagWithDefault_ (row ^. statusLastReported) (val_ (1 :: Integer)) (row ^. statusLastReported) `over_` w
                                )
                     ) statusForStation

  -- Difference between row values and lagged values
  withDeltas <- selecting $ do
    -- as seconds:
    let timeDelta column column' = cast_ (extract_ epoch_ column - extract_ epoch_ column') int
    -- Calculate delta between current and previous availability.
      in withWindow_ (\(row, _) -> frame_ (partitionBy_ (_statusStationId row)) noOrder_ noBounds_)
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
                   ) -> ( group_       (_statusStationId status)
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
    guard_ ((chargingsSum ^. _1) `references_` info)

    pure ( chargingsSum ^. _1            -- Station ID
         , info ^. infoIsChargingStation -- Is charging station
         , info ^. infoCapacity          -- Station capacity
         , chargingsSum ^. _2            -- Total seconds
         , chargingsSum ^. _3            -- Integrals of (bikes available, bikes disabled, docks available, docks disabled)
         , chargingsSum ^. _4            -- Integrals of (iconic available, efit available, efit g5 available)
         )
