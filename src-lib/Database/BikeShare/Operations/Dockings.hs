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
     ( AvailabilityCountChanged (..)
     , StatusQuery (..)
     , StatusThreshold (..)
     , formatDockingEventsCount
     , queryDockingEventsCount
     ) where

import           Control.Lens                             hiding ( reuse, (<.) )

import           Data.Int                                 ( Int32 )

import           Database.Beam
import           Database.Beam.Backend                    ( BeamSqlBackend )
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.BikeShare.Utils


data StatusQuery = StatusQuery
  { _status_query_station_id :: Int32
  , _status_query_thresholds :: [StatusThreshold] -- Update from single StatusThreshold to list
  } deriving (Show, Eq)

data StatusThreshold =
    OldestID Int32
  | SinceTime ReportTime
  deriving (Show, Eq)

thresholdCondition :: StatusThreshold -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
thresholdCondition (OldestID id_threshold) status =
  status ^. d_status_id >=. val_ (fromIntegral id_threshold)
thresholdCondition (SinceTime time_threshold) status =
  status ^. d_status_last_reported >=. val_ (Just time_threshold)

filterFor_ :: StatusQuery -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
filterFor_ (StatusQuery stationId thresholds) status =
  let stationCondition = status ^. d_status_station_id ==. val_ (fromIntegral stationId)
      thresholdConditions = map (`thresholdCondition` status) thresholds
  in foldr (&&.) stationCondition thresholdConditions

-- | Data type representing the type of statistic to query.
data AvailabilityCountChanged where
  Undocked      :: AvailabilityCountChanged -- ^ Bike undocked (ride began at this station)
  Docked        :: AvailabilityCountChanged -- ^ Bike docked   (ride ended at this station)
  deriving (Show, Eq)

-- | TODO: parameterize over column.
queryDockingEventsCount :: Connection -> AvailabilityCountChanged -> StatusQuery -> IO [(StationStatusT Identity, Int32)]
queryDockingEventsCount conn statisticType conditions =
  runBeamPostgres' conn $ runSelectReturningList $ selectWith $ do
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ conditions)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ (row ^. d_status_station_id)) (orderPartitionBy_ (asc_ $ row ^. d_status_id)) noBounds_)
                     (\row w -> (row, lagWithDefault_ (row ^. vehicle_types_available_iconic) (val_ 1) (row ^. vehicle_types_available_iconic) `over_` w))
                     statusForStation
  dockings <- selecting $ do
    -- Only rows where the availability increased.
    let increments = filter_ (\(s, prevAvail) -> s ^. vehicle_types_available_iconic `deltaOp_` prevAvail)
                     (reuse cte)
          -- Delta between current and previous iconic availability.
          in withWindow_ (\(row, _prev) -> frame_ (partitionBy_ (row ^. d_status_station_id)) noOrder_ noBounds_)
                         (\(row, prev) _w -> (row, row ^. vehicle_types_available_iconic - prev))
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
    deltaOp_ = case statisticType of
      Undocked -> (<.)
      Docked   -> (>.)

formatDockingEventsCount :: [(StationStatusT Identity, Int32)] -> IO ()
formatDockingEventsCount events = pPrintCompact $ map (\(s, l) ->
                                             ( s ^. d_status_id & unSerial
                                             , s ^. d_status_station_id
                                             , s ^. d_status_last_reported
                                             , l
                                             )
                                          ) events
