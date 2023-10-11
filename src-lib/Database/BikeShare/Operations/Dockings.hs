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
     , DockingEventsCount (..)
     , StatusThreshold (..)
     , StatusVariationQuery (..)
     , queryDockingEventsCount
     ) where

import qualified API.Types              as AT

import           AppEnv

import           Control.Lens           hiding ( reuse, (<.) )

import           Data.Int               ( Int32 )

import           Database.Beam
import           Database.Beam.Postgres
import           Database.BikeShare


-- | Data type representing a query for station status dockings or undockings.
data StatusVariationQuery where
  StatusVariationQuery :: { _status_query_station_id    :: Int32
                          , _status_query_variation     :: AvailabilityCountVariation
                          , _status_query_bike_type     :: AT.TorontoVehicleType
                          , _status_query_thresholds    :: [StatusThreshold]
                          } -> StatusVariationQuery
  deriving (Generic, Show, Eq)

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
filterFor_ (StatusVariationQuery _ _ _ thresholds) status =
  let thresholdConditions = map (`thresholdCondition` status) thresholds
  -- in foldr (&&.) stationCondition thresholdConditions
  in foldr (&&.) (val_ True) thresholdConditions

-- | Data type representing the type of statistic to query.
data AvailabilityCountVariation where
  Undocking      :: AvailabilityCountVariation -- ^ Bike undocked (ride began at this station)
  Docking        :: AvailabilityCountVariation -- ^ Bike docked   (ride ended at this station)
  deriving (Show, Eq)


-- | Wrapper for a station and its undocking and docking counts.
data DockingEventsCount where
  DockingEventsCount :: { station     :: StationInformation
                        , variation   :: StatusVariationQuery
                        , undockings  :: Int
                        , dockings    :: Int
                        } -> DockingEventsCount
  deriving (Generic, Show, Eq)


queryDockingEventsCount :: StatusVariationQuery -> App [DockingEventsCount]
queryDockingEventsCount variation =  do
  counts <- queryDockingEventsCountExpr variation
  pure $ map (\(station, (undockings, dockings))
              -> DockingEventsCount station variation (fromIntegral undockings) (fromIntegral dockings)
             ) counts
queryDockingEventsCountExpr :: StatusVariationQuery -> App [(StationInformation, (Int32, Int32))]
queryDockingEventsCountExpr variation@(StatusVariationQuery _ _ bikeType _) =
  let bikeType' = case bikeType of
        AT.Iconic -> vehicle_types_available_iconic
        AT.Boost  -> vehicle_types_available_boost
        AT.EFit   -> vehicle_types_available_efit
        AT.EFitG5 -> vehicle_types_available_efit_g5
  in withPostgres $ runSelectReturningList $ selectWith $ do
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ (row ^. d_status_info_id)) (orderPartitionBy_ (asc_ $ row ^. d_status_id)) noBounds_)
                     (\row w -> (row, lagWithDefault_ (row ^. bikeType') (val_ 1) (row ^. bikeType') `over_` w))
                     statusForStation
  withDeltas <- selecting $ do
    -- Delta between current and previous iconic availability.
    withWindow_ (\(row, _prev) -> frame_ (partitionBy_ (row ^. d_status_info_id)) noOrder_ noBounds_)
                (\(row, prev) _w -> (row, row ^. bikeType' - prev))
                (reuse cte)

  dockings <- selecting $ do
    let increased = filter_ (\(_s, delta) -> delta >=. 0)
                    (reuse withDeltas)

        agg = aggregate_ (\(status, delta) -> (group_ (status ^. d_status_info_id), fromMaybe_ 0 $ sum_ delta))
                         increased
      in orderBy_ (\(sId, _sum) -> asc_ sId) agg
  undockings <- selecting $ do
    let decreased = filter_ (\(_s, delta) -> delta <=. 0)
                    (reuse withDeltas)

        agg = aggregate_ (\(status, delta) -> (group_ (status ^. d_status_info_id), fromMaybe_ 0 $ sum_ delta))
                         decreased
      in orderBy_ (\(sId, _sum) -> asc_ sId) agg

  pure $ do
    dockings' <- reuse dockings
    undockings' <- reuse undockings
    -- NOTE: Required, or else result is massive.
    stationInfo <- filter_ (\i -> i ^. info_station_id ==. dockings' ^. _1 &&. (i ^. info_station_id ==. undockings' ^. _1))
                   (all_ (bikeshareDb ^. bikeshareStationInformation))
    let dockingsCount   = dockings' ^. _2
    let undockingsCount = undockings' ^. _2
    pure (stationInfo, (undockingsCount, dockingsCount))
