{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE Rank2Types                #-}

-- Signatures of beam-related functions are incredibly verbose, so let's settle for partial type signatures.
-- Sometimes it is straight up impossible to write the types down because of ambiguous types.
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | This module contains operations to query the number of dockings and undockings for a station.

module Database.BikeShare.Operations.Dockings
     ( AvailabilityCountVariation (..)
     , StatusThreshold (..)
     , StatusVariationQuery (..)
     , queryDockingEventsCount
     ) where

import qualified API.Types                as AT

import           Control.Lens             hiding ( reuse, (<.) )

import           Data.Int                 ( Int32 )

import           Database.Beam
import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.BikeShare.Utils


-- | Data type representing a query for station status dockings or undockings.
data StatusVariationQuery where
  StatusVariationQuery :: { _status_query_station_id    :: Int32
                          , _status_query_variation     :: AvailabilityCountVariation
                          , _status_query_bike_type     :: AT.TorontoVehicleType
                          , _status_query_thresholds    :: [StatusThreshold]
                          } -> StatusVariationQuery
  -- deriving (Show, Eq)

-- | Varient representing the type of threshold to apply to the query.
data StatusThreshold where
  OldestID      :: Int32        -> StatusThreshold
  NewestID      :: Int32        -> StatusThreshold
  EarliestTime  :: ReportTime   -> StatusThreshold
  LatestTime    :: ReportTime   -> StatusThreshold
  deriving (Show, Eq)

-- | Convert a 'StatusQuery' to a fragment of a filter expression.
thresholdCondition :: StatusThreshold -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
thresholdCondition (OldestID id_threshold) status =
  status ^. d_status_id >=. val_ (fromIntegral id_threshold)
thresholdCondition (NewestID id_threshold) status =
  status ^. d_status_id <=. val_ (fromIntegral id_threshold)
thresholdCondition (EarliestTime time_threshold) status =
  status ^. d_status_last_reported >=. val_ (Just time_threshold)
thresholdCondition (LatestTime time_threshold) status =
  status ^. d_status_last_reported <=. val_ (Just time_threshold)

-- | Construct a filter expression for a 'StatusQuery'.
filterFor_ :: StatusVariationQuery -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
filterFor_ (StatusVariationQuery stationId _ bike_type thresholds) status =
  let stationCondition = status ^. d_status_station_id ==. val_ (fromIntegral stationId)
      thresholdConditions = map (`thresholdCondition` status) thresholds
  -- in foldr (&&.) stationCondition thresholdConditions
  in foldr (&&.) (val_ True) thresholdConditions

-- | Data type representing the type of statistic to query.
data AvailabilityCountVariation where
  Undocking      :: AvailabilityCountVariation -- ^ Bike undocked (ride began at this station)
  Docking        :: AvailabilityCountVariation -- ^ Bike docked   (ride ended at this station)
  deriving (Show, Eq)


-- conn <- connectDbName dbnameTest "" "" "" ""
-- queryDockingEventsCount conn (StatusVariationQuery 7148 Docking AT.Iconic [ OldestID 1890764 ])
-- queryDockingEventsCount :: Connection -> StatusVariationQuery -> IO [(StationStatusT Identity, Maybe Int32)]
-- queryDockingEventsCount :: Connection -> StatusVariationQuery -> IO [Maybe Int32]

-- Query Docking counts for all stations:
-- λ> res <- queryDockingEventsCount <$> (connectDbName dbnameTest "" "" "" "") <*> pure (StatusVariationQuery 7148 Docking AT.Iconic [ OldestID 0 ]) >>= liftIO
-- λ> length res
-- 165
queryDockingEventsCount conn variation@(StatusVariationQuery stationId queryVariation bikeType thresholds) =
  let bikeType' = case bikeType of
        AT.Iconic -> vehicle_types_available_iconic
        AT.Boost  -> vehicle_types_available_boost
        AT.EFit   -> vehicle_types_available_efit
        AT.EFitG5 -> vehicle_types_available_efit_g5
  in runBeamPostgresDebug' conn $ runSelectReturningList $ selectWith $ do
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ (row ^. d_status_info_id)) (orderPartitionBy_ (asc_ $ row ^. d_status_id)) noBounds_)
                     (\row w -> (row, lagWithDefault_ (row ^. bikeType') (val_ 1) (row ^. bikeType') `over_` w))
                     statusForStation
  withDeltas <- selecting $ do
    -- Only rows where the availability increased.
    let availNEqPrev = filter_ (\(s, prevAvail) -> s ^. bikeType' /=. prevAvail)
                     (reuse cte)
          -- Delta between current and previous iconic availability.
          in withWindow_ (\(row, _prev) -> frame_ (partitionBy_ (row ^. d_status_info_id)) noOrder_ noBounds_)
                         (\(row, prev) _w -> (row, row ^. bikeType' - prev))
                         availNEqPrev

  dockings <- selecting $ do
    let f = filter_ (\(_s, delta) -> delta >. 0)
            (reuse withDeltas)

        agg = aggregate_ (\(status, delta) -> (group_ (status ^. d_status_info_id), fromMaybe_ 0 $ sum_ delta))
                         f
      in orderBy_ (\(sId, _sum) -> asc_ sId) agg
  undockings <- selecting $ do
    let f = filter_ (\(_s, delta) -> delta <. 0)
            (reuse withDeltas)

        agg = aggregate_ (\(status, delta) -> (group_ (status ^. d_status_info_id), fromMaybe_ 0 $ sum_ delta))
                         f
      in orderBy_ (\(sId, _sum) -> asc_ sId) agg

  pure $ do
    dockings' <- reuse dockings
    undockings' <- reuse undockings
    stationInfo <- filter_ (\i -> i ^. info_station_id ==. dockings' ^. _1 &&. (i ^. info_station_id ==. undockings' ^. _1))
                   (all_ (bikeshareDb ^. bikeshareStationInformation))
    let dockingsCount   = dockings' ^. _2
    let undockingsCount = undockings' ^. _2
    pure (stationInfo, (undockingsCount, dockingsCount))
