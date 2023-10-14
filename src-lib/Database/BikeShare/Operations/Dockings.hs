{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}

-- Signatures of beam-related functions are incredibly verbose, so let's settle for partial type signatures.
-- Sometimes it is straight up impossible to write the types down because of ambiguous types.
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | This module contains operations to query the number of dockings and undockings for a station.

module Database.BikeShare.Operations.Dockings
     ( AvailabilityCountVariation (..)
     , DockingEventsCount (..)
     , EventsCountResult (..)
     , StatusThreshold (..)
     , StatusVariationQuery (..)
       -- Lenses
     , eventsBoostCount
     , eventsCountBikeType
     , eventsCountDockings
     , eventsCountUndockings
     , eventsEfitCount
     , eventsEfitG5Count
     , eventsIconicCount
     , eventsStation
     , eventsVariation
     , queryDockingEventsCount
     ) where

import           API.Types              ( TorontoVehicleType (..) )

import           AppEnv

import           Control.Lens           hiding ( reuse, (<.) )

import           Data.Int               ( Int32 )

import           Database.Beam
import           Database.Beam.Postgres
import           Database.BikeShare


-- | Data type representing a query for station status dockings or undockings.
data StatusVariationQuery where
  StatusVariationQuery :: { _status_query_station_id    :: Maybe Int32
                          , _status_query_thresholds    :: [StatusThreshold]
                          } -> StatusVariationQuery
  deriving (Generic, Show, Eq)


-- | Varient representing the type of threshold to apply to the query.
data StatusThreshold where
  EarliestTime  :: ReportTime   -> StatusThreshold
  LatestTime    :: ReportTime   -> StatusThreshold
  deriving (Show, Eq)


-- | Convert a 'StatusQuery' to a fragment of a filter expression.
thresholdCondition :: StatusThreshold -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
thresholdCondition (EarliestTime threshold) status = status ^. statusLastReported >=. val_ threshold
thresholdCondition (LatestTime threshold) status   = status ^. statusLastReported <=. val_ threshold


-- | Construct a filter expression corresponding to the station ID.
stationIdCondition :: Maybe Int32 -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
stationIdCondition (Just stationId) status = _statusStationId status ==. val_ (StationInformationId stationId)
stationIdCondition Nothing _               = val_ True


-- | Construct a filter expression for a 'StatusQuery'.
filterFor_ :: StatusVariationQuery -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
filterFor_ (StatusVariationQuery stationId thresholds) status =
  let thresholdConditions = map (`thresholdCondition` status) thresholds
  in foldr (&&.) (stationIdCondition stationId status) thresholdConditions


-- | Data type representing the type of statistic to query.
data AvailabilityCountVariation where
  Undocking      :: AvailabilityCountVariation -- ^ Bike undocked (ride began at this station)
  Docking        :: AvailabilityCountVariation -- ^ Bike docked   (ride ended at this station)
  deriving (Show, Eq)


-- | Wrapper for a station and its undocking and docking counts.
data DockingEventsCount where
  DockingEventsCount :: { _eventsStation     :: StationInformation
                        , _eventsVariation   :: StatusVariationQuery
                        , _eventsBoostCount  :: EventsCountResult
                        , _eventsIconicCount :: EventsCountResult
                        , _eventsEfitCount   :: EventsCountResult
                        , _eventsEfitG5Count :: EventsCountResult
                        } -> DockingEventsCount
  deriving (Generic, Show, Eq)


-- | Wrapper for the undocking and docking counts for a bike type.
data EventsCountResult =
  EventsCountResult { _eventsCountBikeType   :: TorontoVehicleType
                    , _eventsCountUndockings :: Int
                    , _eventsCountDockings   :: Int
                    }
  deriving (Generic, Show, Eq)


-- | Lenses
makeLenses ''DockingEventsCount
makeLenses ''EventsCountResult


-- | Query the number of dockings and undockings for a station.
queryDockingEventsCount :: StatusVariationQuery -> App [DockingEventsCount]
queryDockingEventsCount variation =  do
  counts <- queryDockingEventsCountExpr variation
  pure $ map (\( station
               , (boostUndockingsCount, boostDockingsCount)
               , (iconicUndockingsCount, iconicDockingsCount)
               , (efitUndockingsCount,   efitDockingsCount)
               , (efitG5UndockingsCount, efitG5DockingsCount)
               )
              -> DockingEventsCount station variation
                        (EventsCountResult Boost  (fromIntegral  boostUndockingsCount) (fromIntegral  boostDockingsCount))
                        (EventsCountResult Iconic (fromIntegral iconicUndockingsCount) (fromIntegral iconicDockingsCount))
                        (EventsCountResult EFit   (fromIntegral   efitUndockingsCount) (fromIntegral   efitDockingsCount))
                        (EventsCountResult EFitG5 (fromIntegral efitG5UndockingsCount) (fromIntegral efitG5DockingsCount))
             ) counts


-- | Query the number of dockings and undockings for a station (returning tuples of each count for each bike type).
queryDockingEventsCountExpr :: StatusVariationQuery -> App [(StationInformation, (Int32, Int32), (Int32, Int32), (Int32, Int32), (Int32, Int32))]
queryDockingEventsCountExpr variation =
  withPostgres $ runSelectReturningList $ selectWith $ do
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ (_statusStationId row)) (orderPartitionBy_ (asc_ $ _unInformationStationId (_statusStationId row))) noBounds_)
                     (\row w -> ( row
                                , lagWithDefault_ (row ^. vehicle_types_available_boost  ) (val_ 1) (row ^. vehicle_types_available_boost  ) `over_` w
                                , lagWithDefault_ (row ^. vehicle_types_available_iconic ) (val_ 1) (row ^. vehicle_types_available_iconic ) `over_` w
                                , lagWithDefault_ (row ^. vehicle_types_available_efit   ) (val_ 1) (row ^. vehicle_types_available_efit   ) `over_` w
                                , lagWithDefault_ (row ^. vehicle_types_available_efit_g5) (val_ 1) (row ^. vehicle_types_available_efit_g5) `over_` w
                                ))
                     statusForStation
  withDeltas <- selecting $ do
    -- Calculate delta between current and previous availability.
    withWindow_ (\(row, _, _, _, _) -> frame_ (partitionBy_ (_statusStationId row)) noOrder_ noBounds_)
                (\(row, pBoost, pIconic, pEFit, pEFitG5) _w -> ( row
                                                               , row ^. vehicle_types_available_boost   - pBoost
                                                               , row ^. vehicle_types_available_iconic  - pIconic
                                                               , row ^. vehicle_types_available_efit    - pEFit
                                                               , row ^. vehicle_types_available_efit_g5 - pEFitG5
                                                               ))
                (reuse cte)

  dockings   <- selecting $ sumDeltasAggregate_ (>.) (reuse withDeltas)
  undockings <- selecting $ sumDeltasAggregate_ (<.) (reuse withDeltas)

  pure $ do
    -- Join the station information with the dockings and undockings.
    dockings'   <- reuse dockings
    undockings' <- reuse undockings

    stationInfo <- all_ (bikeshareDb ^. bikeshareStationInformation)
    -- NOTE: Required, otherwise result is massive.
    guard_ ( (dockings'   ^. _1) `references_` stationInfo  &&.
             (undockings' ^. _1) `references_` stationInfo
           )

    -- Return tuples of station information and the dockings and undockings.
    pure ( stationInfo
         , (undockings' ^. _2, dockings' ^. _2) -- Boost
         , (undockings' ^. _3, dockings' ^. _3) -- Iconic
         , (undockings' ^. _4, dockings' ^. _4) -- E-Fit
         , (undockings' ^. _5, dockings' ^. _5) -- E-Fit G5
         )
  where
    -- Aggregate expression for unidirectionally summing deltas (only where delta is positive, or only where delta is negative).
    sumDeltasAggregate_ binOp =
      aggregate_ (\(status, dBoost, dIconic, dEFit, dEFitG5) ->
                     ( group_ (_statusStationId status)
                     , fromMaybe_ 0 $ sum_ dBoost  `filterWhere_` (dBoost  `binOp` 0)
                     , fromMaybe_ 0 $ sum_ dIconic `filterWhere_` (dIconic `binOp` 0)
                     , fromMaybe_ 0 $ sum_ dEFit   `filterWhere_` (dEFit   `binOp` 0)
                     , fromMaybe_ 0 $ sum_ dEFitG5 `filterWhere_` (dEFitG5 `binOp` 0)
                     ))
