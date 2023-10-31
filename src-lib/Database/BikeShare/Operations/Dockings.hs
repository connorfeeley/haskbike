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
     , ChargingEvent (..)
     , DockingEventsCount (..)
     , EventsCountResult (..)
     , StatusThreshold (..)
     , StatusVariationQuery (..)
       -- Lenses
     , allBikeEvents
     , efitEvents
     , efitG5Events
     , eventsBoostCount
     , eventsCountBikeType
     , eventsCountDockings
     , eventsCountUndockings
     , eventsEfitCount
     , eventsEfitG5Count
     , eventsIconicCount
     , eventsStation
     , eventsVariation
     , iconicEvents
     , queryChargingEventsCount
     , queryChargingEventsCountExpr
     , queryDockingEventsCount
     , sumAllCharging
     , sumChargings
     , sumEfitCharging
     , sumEfitG5Charging
     , sumEvents
     ) where

import           API.Types              ( TorontoVehicleType (..) )

import           AppEnv

import           Control.Lens           hiding ( reuse, (<.) )

import           Data.Int               ( Int32 )
import           Data.Time

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
  EarliestTime  :: UTCTime   -> StatusThreshold
  LatestTime    :: UTCTime   -> StatusThreshold
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

-- | Wrapper for a station and its undocking and docking counts.
data ChargingEvent where
  ChargingEvent :: { _chargedBikeType     :: TorontoVehicleType
                   , _chargedBikeNumber   :: Int
                   } -> ChargingEvent
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


-- | Get events for a specific bike type (all, Iconic, E-Fit, or E-Fit G5).
allBikeEvents, iconicEvents, efitEvents, efitG5Events :: [DockingEventsCount] -> [EventsCountResult]
allBikeEvents ev = iconicEvents ev <> efitEvents ev <> efitG5Events ev
iconicEvents = map _eventsIconicCount
efitEvents   = map _eventsEfitCount
efitG5Events = map _eventsEfitG5Count

{-
Sum bike events for 'Docking' or 'Undocking'.

>>> sumEvents Docking (allBikeEvents ev)
150
-}
sumEvents :: AvailabilityCountVariation -> [EventsCountResult] -> Int
sumEvents Docking   = abs . sum . map _eventsCountDockings
sumEvents Undocking = abs . sum . map _eventsCountUndockings

-- | Query the number of dockings and undockings for a station.
queryDockingEventsCount :: StatusVariationQuery -> AppM [DockingEventsCount]
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
queryDockingEventsCountExpr :: StatusVariationQuery -> AppM [(StationInformation, (Int32, Int32), (Int32, Int32), (Int32, Int32), (Int32, Int32))]
queryDockingEventsCountExpr variation =
  withPostgres $ runSelectReturningList $ selectWith $ do
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ (_statusStationId row)) (orderPartitionBy_ (asc_ $ _statusLastReported row)) noBounds_)
                     (\row w -> ( row
                                , lagWithDefault_ (row ^. vehicleTypesAvailableBoost ) (val_ 1) (row ^. vehicleTypesAvailableBoost ) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableIconic) (val_ 1) (row ^. vehicleTypesAvailableIconic) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableEfit  ) (val_ 1) (row ^. vehicleTypesAvailableEfit  ) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableEfitG5) (val_ 1) (row ^. vehicleTypesAvailableEfitG5) `over_` w
                                ))
                     statusForStation
  withDeltas <- selecting $ do
    -- Calculate delta between current and previous availability.
    withWindow_ (\(row, _, _, _, _) -> frame_ (partitionBy_ (_statusStationId row)) noOrder_ noBounds_)
                (\(row, pBoost, pIconic, pEFit, pEFitG5) _w -> ( row
                                                               , row ^. vehicleTypesAvailableBoost  - pBoost
                                                               , row ^. vehicleTypesAvailableIconic - pIconic
                                                               , row ^. vehicleTypesAvailableEfit   - pEFit
                                                               , row ^. vehicleTypesAvailableEfitG5 - pEFitG5
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

---------------------------------

-- | Query the number of charging events for a station (returning status record and charging event records).
queryChargingEventsCount :: StatusVariationQuery -> AppM [(StationStatus, [ChargingEvent])]
queryChargingEventsCount variation =  do
  counts <- queryChargingEventsCountExpr variation
  pure $ map (\( status
               , _dBikesDisabled
               , dEFit
               , dEFitG5
               , _sumDisabled
               , _sumEfit
               , _sumEfitG5
               )
              -> (status, filter notZero
                   [ ChargingEvent { _chargedBikeType = EFit,   _chargedBikeNumber = fromIntegral dEFit   }
                   , ChargingEvent { _chargedBikeType = EFitG5, _chargedBikeNumber = fromIntegral dEFitG5 }
                   ]
                 )
             ) counts
  where
    notZero c = _chargedBikeNumber c /= 0

-- | Query the number of charging events for a station (returning status record and tuples of (dDisabled, dEfit, dEfitG5, sumDisabled, sumEfit, sumEfitG5).
queryChargingEventsCountExpr :: StatusVariationQuery -> AppM [(StationStatus, Int32, Int32, Int32, Int32, Int32, Int32)]
queryChargingEventsCountExpr variation =
  withPostgres $ runSelectReturningList $ selectWith $ do
  -- Lag expression
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ (_statusStationId row)) (orderPartitionBy_ (asc_ $ _statusLastReported row)) noBounds_)
                     (\row w -> ( row
                                , lagWithDefault_ (row ^. statusNumBikesDisabled     ) (val_ 1) (row ^. statusNumBikesDisabled     ) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableEfit  ) (val_ 1) (row ^. vehicleTypesAvailableEfit  ) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableEfitG5) (val_ 1) (row ^. vehicleTypesAvailableEfitG5) `over_` w
                                ))
                     statusForStation

  -- Difference between row values and lagged values
  withDeltas <- selecting $ do
    -- Calculate delta between current and previous availability.
    withWindow_ (\(row, _, _, _) -> frame_ (partitionBy_ (_statusStationId row)) noOrder_ noBounds_)
                (\(row, pBikesDisabled, pEFit, pEFitG5) _w -> ( row                                                  -- _1
                                                               , row ^. statusNumBikesDisabled      - pBikesDisabled -- _2
                                                               , row ^. vehicleTypesAvailableEfit   - pEFit          -- _3
                                                               , row ^. vehicleTypesAvailableEfitG5 - pEFitG5        -- _4
                                                               ))
                (reuse cte)
  chargings <- selecting $ reuse withDeltas

  pure $ do
    chargings' <- reuse chargings
    chargingsSum <-
      aggregate_ (\(status, dBikesDisabled, dEFit, dEFitG5) ->
                     ( group_ (_statusStationId status)
                     -- Sum of all instances where an e-bike was disabled, then enabled.
                     , fromMaybe_ 0 $ sum_ dBikesDisabled  `filterWhere_` (dBikesDisabled  <. 0 &&. (dEFit >. 0 ||. dEFitG5 >. 0))
                     -- Sum of all instances where an E-Fit bike was disabled, then enabled.
                     , fromMaybe_ 0 $ sum_ dEFit   `filterWhere_`         (dBikesDisabled  <. 0 &&. dEFit >. 0)
                     -- Sum of all instances where an E-Fit G5 bike was disabled, then enabled.
                     , fromMaybe_ 0 $ sum_ dEFitG5 `filterWhere_`         (dBikesDisabled  <. 0 &&. dEFitG5 >. 0)
                     ))
                  (reuse chargings)

    stationInfo <- all_ (bikeshareDb ^. bikeshareStationInformation)
    guard_ ((chargings'   ^. _1 & _statusStationId) `references_` stationInfo &&.
            (chargingsSum ^. _1)                    `references_` stationInfo &&.
            (chargings'   ^. _2) <. 0 &&.
            (((chargings' ^. _3) >. 0) ||. ((chargings' ^. _4) >. 0))
           )

    pure ( chargings' ^. _1    -- row
         , chargings' ^. _2    -- dBikesDisabled
         , chargings' ^. _3    -- dEfit
         , chargings' ^. _4    -- dEfitG5
         , chargingsSum ^. _2 -- sum of charging events over queried range (negative; reflects change in disabled bikes)
         , chargingsSum ^. _3 -- sum of E-Fit charging events over queried range (positive; reflects change in available e-fit)
         , chargingsSum ^. _4 -- sum of E-Fit G5 charging events over queried range (positive; reflects change in available e-fit g5)
         )

sumAllCharging, sumEfitCharging, sumEfitG5Charging :: [(StationStatus, [ChargingEvent])] -> Int
sumAllCharging    events = sumChargings (const True)                         (map snd events)
sumEfitCharging   events = sumChargings (\c -> _chargedBikeType c == EFit)   (map snd events)
sumEfitG5Charging events = sumChargings (\c -> _chargedBikeType c == EFitG5) (map snd events)

-- | Sum number of chargings (for a given filter condition).
sumChargings :: (ChargingEvent -> Bool) -> [[ChargingEvent]] -> Int
sumChargings cond chargings = sumBikes (filter cond (concat chargings))
  where
    sumBikes = sum . map _chargedBikeNumber


-- * Disabled-bike-seconds.

-- -- | Query the number of disabled-bike-seconds.
-- queryDisabledBikeSeconds :: StatusVariationQuery -> AppM [(StationStatus, (Int, Int))]
-- queryDisabledBikeSeconds variation =  do
--   counts <- queryDisabledBikeSecondsExpr variation
--   pure $ map (\( status
--                , dLastReported
--                , bikesDisabled
--                )
--               -> (status, (0, 0) )
--              ) counts
--   where
--     notZero c = _chargedBikeNumber c /= 0

-- -- | Query the number of disabled-bike-seconds.
-- queryDisabledBikeSecondsExpr :: StatusVariationQuery -> AppM [(StationStatus, Int32, UTCTime)]
-- queryDisabledBikeSecondsExpr variation =
--   withPostgres $ runSelectReturningList $ selectWith $ do
--   -- Lag expression
--   cte <- selecting $ do
--     let statusForStation = filter_ (filterFor_ variation)
--                                    (all_ (bikeshareDb ^. bikeshareStationStatus))
--       in withWindow_ (\row -> frame_ (partitionBy_ (_statusStationId row)) (orderPartitionBy_ (asc_ $ _statusLastReported row)) noBounds_)
--                      (\row w -> ( row
--                                 , lagWithDefault_ (row ^. statusNumBikesDisabled) (val_ 1) (row ^. statusNumBikesDisabled) `over_` w
--                                 , lagWithDefault_ (row ^. statusLastReported  ) (row ^. statusLastReported) (row ^. statusLastReported) `over_` w
--                                 ))
--                      statusForStation

--   -- Difference between row values and lagged values
--   withDeltas <- selecting $ do
--     -- Calculate delta between current and previous availability.
--     withWindow_ (\(row, _, _) -> frame_ (partitionBy_ (_statusStationId row)) noOrder_ noBounds_)
--                 (\(row, pBikesDisabled, pLastReported) _w -> ( row            -- _1
--                                                              , pBikesDisabled -- _2
--                                                              , pLastReported  -- _3
--                                                              ))
--                 (reuse cte)
--   deltas <- selecting $ reuse withDeltas

--   pure $ do
--     deltas' <- reuse deltas
--     -- disabled <-
--     --   aggregate_ (\(status, dBikesDisabled, dLastReported) ->
--     --                  ( group_ (_statusStationId status)
--     --                  , dBikesDisabled
--     --                  , dLastReported
--     --                  ))
--     --               (reuse deltas)

--     stationInfo <- all_ (bikeshareDb ^. bikeshareStationInformation)
--     guard_ ((deltas'   ^. _1 & _statusStationId) `references_` stationInfo)

--     pure ( deltas' ^. _1    -- row
--          , deltas' ^. _2    -- dBikesDisabled
--          , deltas' ^. _3     -- dLastReported
--          )
-- -- diffUTCTime_ :: QGenExpr ctxt PgExpressionSyntax s UTCTime -> QGenExpr ctxt PgExpressionSyntax s NominalDiffTime -> QGenExpr ctxt PgExpressionSyntax s UTCTime
-- -- diffUTCTime_ = customExpr_ (\tm offs -> "(" <> tm <> " - INTERVAL '" <> offs <> " SECONDS')")
