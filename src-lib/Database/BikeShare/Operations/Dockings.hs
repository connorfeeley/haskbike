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
     , formatDockingEventsCount
     , queryDockingEventsCount
     ) where

import qualified API.Types                                as AT

import           Control.Lens                             hiding ( reuse, (<.) )

import           Data.Int                                 ( Int32 )

import           Database.Beam
import           Database.Beam.Backend                    ( BeamSqlBackend )
import           Database.Beam.Backend.SQL.BeamExtensions
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
  in foldr (&&.) stationCondition thresholdConditions

-- | Data type representing the type of statistic to query.
data AvailabilityCountVariation where
  Undocking      :: AvailabilityCountVariation -- ^ Bike undocked (ride began at this station)
  Docking        :: AvailabilityCountVariation -- ^ Bike docked   (ride ended at this station)
  deriving (Show, Eq)


queryDockingEventsCount :: Connection -> StatusVariationQuery -> IO [(StationStatusT Identity, Int32)]
queryDockingEventsCount conn variation@(StatusVariationQuery stationId queryVariation bikeType thresholds) =
  let bikeType' = case bikeType of
        AT.Iconic -> vehicle_types_available_iconic
        AT.Boost  -> vehicle_types_available_boost
        AT.EFit   -> vehicle_types_available_efit
        AT.EFitG5 -> vehicle_types_available_efit_g5
  in runBeamPostgres' conn $ runSelectReturningList $ selectWith $ do
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ (row ^. d_status_station_id)) (orderPartitionBy_ (asc_ $ row ^. d_status_id)) noBounds_)
                     (\row w -> (row, lagWithDefault_ (row ^. bikeType') (val_ 1) (row ^. bikeType') `over_` w))
                     statusForStation
  dockings <- selecting $ do
    -- Only rows where the availability increased.
    let increments = filter_ (\(s, prevAvail) -> s ^. bikeType' `deltaOp_` prevAvail)
                     (reuse cte)
          -- Delta between current and previous iconic availability.
          in withWindow_ (\(row, _prev) -> frame_ (partitionBy_ (row ^. d_status_station_id)) noOrder_ noBounds_)
                         (\(row, prev) _w -> (row, row ^. bikeType' - prev))
                         increments

  pure $ do
    counts <- reuse cte -- [(status, previous availability)]

    -- Rows where the delta was either:
    -- - positive ('deltaOp_': '(>.)')
    -- - negative ('deltaOp_': '(<.)')
    -- ... depending on the statisticType paramater.
    changed <- filter_ (\(_s, delta) -> delta `deltaOp_` 0)
                       (reuse dockings)

    guard_ ((counts ^. _1 . d_status_id) ==. (changed ^. _1 . d_status_id))

    pure changed
  where
    infixl 4 `deltaOp_` -- same as '(<.)' and '(>.)'.
    deltaOp_ :: (BeamSqlBackend be) => QGenExpr context be s a -> QGenExpr context be s a -> QGenExpr context be s Bool
    deltaOp_ = case _status_query_variation variation of
      Undocking -> (<.)
      Docking   -> (>.)

formatDockingEventsCount :: [(StationStatusT Identity, Int32)] -> IO ()
formatDockingEventsCount events = pPrintCompact $ map (\(s, l) ->
                                             ( s ^. d_status_id & unSerial
                                             , s ^. d_status_station_id
                                             , s ^. d_status_last_reported
                                             , l
                                             )
                                          ) events
