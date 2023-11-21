{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
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
     , queryDockingEventsCount
     , sumAllCharging
     , sumChargings
     , sumEfitCharging
     , sumEfitG5Charging
     , sumEvents
     ) where

import           API.Types                               ( TorontoVehicleType (..) )

import           AppEnv

import           Control.Lens                            hiding ( reuse, (<.) )

import           Data.Int                                ( Int32 )

import           Database.Beam
import           Database.BikeShare
import           Database.BikeShare.StatusVariationQuery


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

-- | Query the number of charging events for a station (returning status record and tuples of (dDisabled, dEfit, dEfitG5, sumDisabled, sumEfit, sumEfitG5).
queryChargingEventsCount :: StatusVariationQuery -> AppM [(StationInformation, Int32, Int32, Int32)]
queryChargingEventsCount variation = withPostgres $ runSelectReturningList $ selectWith $ do
  stationInfo <- selecting $ all_ (bikeshareDb ^. bikeshareStationInformation)
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

  pure $ do
    stationInfo' <- reuse stationInfo
    chargingsSum <-
      aggregate_ (\(row, pBikesDisabled, pEFit, pEFitG5) ->
                    let dBikesDisabled = row ^. statusNumBikesDisabled - pBikesDisabled
                        dEFit          = row ^. vehicleTypesAvailableEfit   - pEFit
                        dEFitG5        = row ^. vehicleTypesAvailableEfitG5 - pEFitG5
                    in
                     ( group_ (_statusStationId row)
                     -- Sum of all instances where an e-bike was disabled, then enabled.
                     , fromMaybe_ 0 $ sum_ dBikesDisabled  `filterWhere_` (dBikesDisabled  <. 0 &&. (dEFit >. 0 ||. dEFitG5 >. 0))
                     -- Sum of all instances where an E-Fit bike was disabled, then enabled.
                     , fromMaybe_ 0 $ sum_ dEFit   `filterWhere_`         (dBikesDisabled  <. 0 &&. dEFit >. 0)
                     -- Sum of all instances where an E-Fit G5 bike was disabled, then enabled.
                     , fromMaybe_ 0 $ sum_ dEFitG5 `filterWhere_`         (dBikesDisabled  <. 0 &&. dEFitG5 >. 0)
                     ))
                  (reuse cte)

    guard_ ( _infoIsChargingStation stationInfo' ==. val_ True &&.
            (chargingsSum ^. _1)                    `references_` stationInfo'
           )

    pure ( stationInfo'
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
