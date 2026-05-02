{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

-- Signatures of beam-related functions are incredibly verbose, so let's settle for partial type signatures.
-- Sometimes it is straight up impossible to write the types down because of ambiguous types.
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE PartialTypeSignatures     #-}

-- | This module contains operations to query the number of dockings and undockings for a station.

module Haskbike.Database.Operations.Dockings
     ( queryChargingEventsCount
     , queryDockingEventsCount
     ) where


import           Control.Lens                                hiding ( reuse, (.=), (<.) )
import           Control.Monad.Catch                         ( MonadCatch, MonadThrow )

import           Data.Int                                    ( Int32 )

import           Database.Beam

import           Haskbike.API.VehicleType
import           Haskbike.AppEnv
import           Haskbike.Database.BikeShare
import           Haskbike.Database.EventCounts
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus

import           UnliftIO                                    ( MonadUnliftIO )


-- | Query the number of dockings and undockings for a station.
queryDockingEventsCount :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                        => StatusVariationQuery -> m [DockingEventsCount]
queryDockingEventsCount variation =  do
  counts <- queryDockingEventsCountExpr' variation

  pure $ map (\( station
               , ( (boostUn,  boostDk)
                 , (iconicUn, iconicDk)
                 , (efitUn,   efitDk)
                 , (efitG5Un, efitG5Dk)
                 )
               , ( (chloeUn, chloeDk)
                 , (cosmoUn, cosmoDk)
                 , (astroUn, astroDk)
                 , (metroUn, metroDk)
                 )
               )
              -> DockingEventsCount station variation
                 (EventsCountResult Boost  (fromIntegral boostUn)  (fromIntegral boostDk))
                 (EventsCountResult Iconic (fromIntegral iconicUn) (fromIntegral iconicDk))
                 (EventsCountResult EFit   (fromIntegral efitUn)   (fromIntegral efitDk))
                 (EventsCountResult EFitG5 (fromIntegral efitG5Un) (fromIntegral efitG5Dk))
                 (EventsCountResult CHLOE  (fromIntegral chloeUn)  (fromIntegral chloeDk))
                 (EventsCountResult Cosmo  (fromIntegral cosmoUn)  (fromIntegral cosmoDk))
                 (EventsCountResult Astro  (fromIntegral astroUn)  (fromIntegral astroDk))
                 (EventsCountResult Metro  (fromIntegral metroUn)  (fromIntegral metroDk))
             ) counts

-- | Query the number of dockings and undockings for a station (returning per-type (undocking, docking) pairs).
--
-- The result is shaped as a 3-tuple to stay within beam's 8-tuple Beamable limit:
--   (StationInformation, (Boost, Iconic, EFit, EFitG5), (CHLOE, Cosmo, Astro, Metro))
-- where each per-type slot is itself an (undocking, docking) pair.
queryDockingEventsCountExpr' :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                             => StatusVariationQuery
                             -> m [( StationInformation
                                   , ((Int32, Int32), (Int32, Int32), (Int32, Int32), (Int32, Int32))
                                   , ((Int32, Int32), (Int32, Int32), (Int32, Int32), (Int32, Int32))
                                   )]
queryDockingEventsCountExpr' variation = withPostgres $ runSelectReturningList $ selectWith $ do
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)) (orderPartitionBy_ ((asc_ . _statusLastReported . _statusCommon) row)) noBounds_)
                     (\row w -> ( row
                                , ( lagWithDefault_ (row ^. vehicleTypesAvailableBoost ) (val_ 1) (row ^. vehicleTypesAvailableBoost ) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableIconic) (val_ 1) (row ^. vehicleTypesAvailableIconic) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableEfit  ) (val_ 1) (row ^. vehicleTypesAvailableEfit  ) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableEfitG5) (val_ 1) (row ^. vehicleTypesAvailableEfitG5) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableChloe ) (val_ 1) (row ^. vehicleTypesAvailableChloe ) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableCosmo ) (val_ 1) (row ^. vehicleTypesAvailableCosmo ) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableAstro ) (val_ 1) (row ^. vehicleTypesAvailableAstro ) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableMetro ) (val_ 1) (row ^. vehicleTypesAvailableMetro ) `over_` w
                                  )
                                ))
                     statusForStation
  rankedInfo' <- selecting $ do
    withWindow_ (\row -> frame_ (partitionBy_ (_infoStationId row)) (orderPartitionBy_ (desc_ $ _infoId row)) noBounds_)
            (\row w -> ( row
                       , rank_ `over_` w
                       )
            )
      (filter_ (infoFilterForLatest_ variation)
      (all_ (bikeshareDb ^. bikeshareStationInformation)))

  pure $ do
    -- Join the station information with the dockings and undockings.
    do
      statusSums <-
        aggregate_ (\(row, (pBoost, pIconic, pEFit, pEFitG5, pChloe, pCosmo, pAstro, pMetro)) ->
                      let
                          dBoost  = row ^. vehicleTypesAvailableBoost  - pBoost
                          dIconic = row ^. vehicleTypesAvailableIconic - pIconic
                          dEFit   = row ^. vehicleTypesAvailableEfit   - pEFit
                          dEFitG5 = row ^. vehicleTypesAvailableEfitG5 - pEFitG5
                          dChloe  = row ^. vehicleTypesAvailableChloe  - pChloe
                          dCosmo  = row ^. vehicleTypesAvailableCosmo  - pCosmo
                          dAstro  = row ^. vehicleTypesAvailableAstro  - pAstro
                          dMetro  = row ^. vehicleTypesAvailableMetro  - pMetro
                      in
                       ( group_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)
                       -- Undockings (negative deltas)
                       , ( fromMaybe_ 0 $ sum_ dBoost  `filterWhere_` (dBoost   <. 0)
                         , fromMaybe_ 0 $ sum_ dIconic `filterWhere_` (dIconic  <. 0)
                         , fromMaybe_ 0 $ sum_ dEFit   `filterWhere_` (dEFit    <. 0)
                         , fromMaybe_ 0 $ sum_ dEFitG5 `filterWhere_` (dEFitG5  <. 0)
                         , fromMaybe_ 0 $ sum_ dChloe  `filterWhere_` (dChloe   <. 0)
                         , fromMaybe_ 0 $ sum_ dCosmo  `filterWhere_` (dCosmo   <. 0)
                         , fromMaybe_ 0 $ sum_ dAstro  `filterWhere_` (dAstro   <. 0)
                         , fromMaybe_ 0 $ sum_ dMetro  `filterWhere_` (dMetro   <. 0)
                         )
                       -- Dockings (positive deltas)
                       , ( fromMaybe_ 0 $ sum_ dBoost  `filterWhere_` (dBoost   >. 0)
                         , fromMaybe_ 0 $ sum_ dIconic `filterWhere_` (dIconic  >. 0)
                         , fromMaybe_ 0 $ sum_ dEFit   `filterWhere_` (dEFit    >. 0)
                         , fromMaybe_ 0 $ sum_ dEFitG5 `filterWhere_` (dEFitG5  >. 0)
                         , fromMaybe_ 0 $ sum_ dChloe  `filterWhere_` (dChloe   >. 0)
                         , fromMaybe_ 0 $ sum_ dCosmo  `filterWhere_` (dCosmo   >. 0)
                         , fromMaybe_ 0 $ sum_ dAstro  `filterWhere_` (dAstro   >. 0)
                         , fromMaybe_ 0 $ sum_ dMetro  `filterWhere_` (dMetro   >. 0)
                         )
                       ))
        (reuse cte)

      rankedInfo <- filter_ (\inf -> inf ^. _2 ==. val_ 1) (reuse rankedInfo')

      guard_' ( _infoStationId (rankedInfo ^. _1) ==?. (statusSums ^. _1)
                &&?. (statusSums ^. _1)           ==?. _infoStationId (rankedInfo ^. _1)
              )

      pure ( rankedInfo ^. _1
           , ( (statusSums ^. _2 . _1, statusSums ^. _3 . _1) -- Boost
             , (statusSums ^. _2 . _2, statusSums ^. _3 . _2) -- Iconic
             , (statusSums ^. _2 . _3, statusSums ^. _3 . _3) -- E-Fit
             , (statusSums ^. _2 . _4, statusSums ^. _3 . _4) -- E-Fit G5
             )
           , ( (statusSums ^. _2 . _5, statusSums ^. _3 . _5) -- CHLOE
             , (statusSums ^. _2 . _6, statusSums ^. _3 . _6) -- Cosmo
             , (statusSums ^. _2 . _7, statusSums ^. _3 . _7) -- Astro
             , (statusSums ^. _2 . _8, statusSums ^. _3 . _8) -- Metro
             )
           )


---------------------------------

-- | Query the number of charging events for a station.
--
-- Returns 7-tuple per row: (StationInformation, totalDisabledChange, dEFit, dEFitG5, dCosmo, dAstro, dMetro).
-- CHLOE is mechanical and not tracked here. Boost is treated as mechanical for charging purposes (matches existing API behaviour).
queryChargingEventsCount :: (HasEnv env m, MonadIO m, MonadCatch m)
                         => StatusVariationQuery
                         -> m [(StationInformation, Int32, Int32, Int32, Int32, Int32, Int32)]
queryChargingEventsCount variation = withPostgres $ runSelectReturningList $ selectWith $ do
  stationInfo <- selecting $ all_ (bikeshareDb ^. bikeshareStationInformation)
  -- Lag expression
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)) (orderPartitionBy_ ((asc_ . _statusLastReported . _statusCommon) row)) noBounds_)
                     (\row w -> ( row
                                , ( lagWithDefault_ (row ^. statusNumBikesDisabled     ) (val_ 1) (row ^. statusNumBikesDisabled     ) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableEfit  ) (val_ 1) (row ^. vehicleTypesAvailableEfit  ) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableEfitG5) (val_ 1) (row ^. vehicleTypesAvailableEfitG5) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableCosmo ) (val_ 1) (row ^. vehicleTypesAvailableCosmo ) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableAstro ) (val_ 1) (row ^. vehicleTypesAvailableAstro ) `over_` w
                                  , lagWithDefault_ (row ^. vehicleTypesAvailableMetro ) (val_ 1) (row ^. vehicleTypesAvailableMetro ) `over_` w
                                  )
                                ))
                     statusForStation

  pure $ do
    stationInfo' <- reuse stationInfo
    guard_ (_infoIsChargingStation stationInfo' ==. val_ True &&. _infoActive stationInfo' ==. val_ True)
    chargingsSum <-
      aggregate_ (\(row, (pBikesDisabled, pEFit, pEFitG5, pCosmo, pAstro, pMetro)) ->
                    let dBikesDisabled = row ^. statusNumBikesDisabled       - pBikesDisabled
                        dEFit          = row ^. vehicleTypesAvailableEfit    - pEFit
                        dEFitG5        = row ^. vehicleTypesAvailableEfitG5  - pEFitG5
                        dCosmo         = row ^. vehicleTypesAvailableCosmo   - pCosmo
                        dAstro         = row ^. vehicleTypesAvailableAstro   - pAstro
                        dMetro         = row ^. vehicleTypesAvailableMetro   - pMetro
                        anyEbikeUp     = dEFit   >. 0 ||. dEFitG5 >. 0 ||. dCosmo >. 0 ||. dAstro >. 0 ||. dMetro >. 0
                    in
                     ( group_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)
                     -- Total bikes-disabled change when any e-bike count went up (i.e. a bike was charged).
                     , fromMaybe_ 0 $ sum_ dBikesDisabled  `filterWhere_` (dBikesDisabled  <. 0 &&. anyEbikeUp)
                     -- Per-e-bike-type charging counts.
                     , fromMaybe_ 0 $ sum_ dEFit    `filterWhere_` (dBikesDisabled <. 0 &&. dEFit   >. 0)
                     , fromMaybe_ 0 $ sum_ dEFitG5  `filterWhere_` (dBikesDisabled <. 0 &&. dEFitG5 >. 0)
                     , fromMaybe_ 0 $ sum_ dCosmo   `filterWhere_` (dBikesDisabled <. 0 &&. dCosmo  >. 0)
                     , fromMaybe_ 0 $ sum_ dAstro   `filterWhere_` (dBikesDisabled <. 0 &&. dAstro  >. 0)
                     , fromMaybe_ 0 $ sum_ dMetro   `filterWhere_` (dBikesDisabled <. 0 &&. dMetro  >. 0)
                     ))
                  (reuse cte)

    guard_' ( (chargingsSum ^. _1) ==?. _infoStationId stationInfo'
           )

    pure ( stationInfo'
         , chargingsSum ^. _2 -- total disabled-bike change for charging events (negative)
         , chargingsSum ^. _3 -- E-Fit
         , chargingsSum ^. _4 -- E-Fit G5
         , chargingsSum ^. _5 -- Cosmo
         , chargingsSum ^. _6 -- Astro
         , chargingsSum ^. _7 -- Metro
         )
