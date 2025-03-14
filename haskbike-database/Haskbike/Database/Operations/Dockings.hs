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
               , (iconicUndockingsCount, iconicDockingsCount)
               , (efitUndockingsCount,   efitDockingsCount)
               , (efitG5UndockingsCount, efitG5DockingsCount)
               )
              -> DockingEventsCount station variation
                 (EventsCountResult Boost  0 0) -- No boost bikes in Toronto.
                 (EventsCountResult Iconic (fromIntegral iconicUndockingsCount) (fromIntegral iconicDockingsCount))
                 (EventsCountResult EFit   (fromIntegral   efitUndockingsCount) (fromIntegral   efitDockingsCount))
                 (EventsCountResult EFitG5 (fromIntegral efitG5UndockingsCount) (fromIntegral efitG5DockingsCount))
             ) counts

-- | Query the number of dockings and undockings for a station (returning tuples of each count for each bike type).
queryDockingEventsCountExpr' :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                             => StatusVariationQuery -> m [(StationInformation, (Int32, Int32), (Int32, Int32), (Int32, Int32))]
queryDockingEventsCountExpr' variation = withPostgres $ runSelectReturningList $ selectWith $ do
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)) (orderPartitionBy_ ((asc_ . _statusLastReported . _statusCommon) row)) noBounds_)
                     (\row w -> ( row
                                , lagWithDefault_ (row ^. vehicleTypesAvailableIconic) (val_ 1) (row ^. vehicleTypesAvailableIconic) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableEfit  ) (val_ 1) (row ^. vehicleTypesAvailableEfit  ) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableEfitG5) (val_ 1) (row ^. vehicleTypesAvailableEfitG5) `over_` w
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
    do -- Pg.pgNubBy_ (_infoStationId . (^. _1 . _1)) $ lateral_ status $ \status' -> do
      statusSums <-
        aggregate_ (\(row, pIconic, pEFit, pEFitG5) ->
                      let
                          dIconic = row ^. vehicleTypesAvailableIconic - pIconic
                          dEFit   = row ^. vehicleTypesAvailableEfit   - pEFit
                          dEFitG5 = row ^. vehicleTypesAvailableEfitG5 - pEFitG5
                      in
                       ( group_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)
                       -- Undockings
                       , ( fromMaybe_ 0 $ sum_ dIconic `filterWhere_` (dIconic  <. 0)
                         , fromMaybe_ 0 $ sum_ dEFit   `filterWhere_` (dEFit    <. 0)
                         , fromMaybe_ 0 $ sum_ dEFitG5 `filterWhere_` (dEFitG5  <. 0)
                         )
                       -- Dockings
                       , ( fromMaybe_ 0 $ sum_ dIconic `filterWhere_` (dIconic  >. 0)
                         , fromMaybe_ 0 $ sum_ dEFit   `filterWhere_` (dEFit    >. 0)
                         , fromMaybe_ 0 $ sum_ dEFitG5 `filterWhere_` (dEFitG5  >. 0)
                         )
                       ))
        (reuse cte)

      rankedInfo <- filter_ (\inf -> inf ^. _2 ==. val_ 1) (reuse rankedInfo')

      guard_' ( _infoStationId (rankedInfo ^. _1) ==?. (statusSums ^. _1)
                &&?. (statusSums ^. _1)           ==?. _infoStationId (rankedInfo ^. _1)
              )

      pure ( rankedInfo ^. _1
           --      Undockings   |     Dockings
           , (statusSums ^. _2 . _1, statusSums ^. _3 . _1) -- Iconic
           , (statusSums ^. _2 . _2, statusSums ^. _3 . _2) -- E-Fit
           , (statusSums ^. _2 . _3, statusSums ^. _3 . _3) -- E-Fit G5
           )


---------------------------------

-- | Query the number of charging events for a station (returning status record and tuples of (dDisabled, dEfit, dEfitG5, sumDisabled, sumEfit, sumEfitG5).
queryChargingEventsCount :: (HasEnv env m, MonadIO m, MonadCatch m) => StatusVariationQuery -> m [(StationInformation, Int32, Int32, Int32)]
queryChargingEventsCount variation = withPostgres $ runSelectReturningList $ selectWith $ do
  stationInfo <- selecting $ all_ (bikeshareDb ^. bikeshareStationInformation)
  -- Lag expression
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)) (orderPartitionBy_ ((asc_ . _statusLastReported . _statusCommon) row)) noBounds_)
                     (\row w -> ( row
                                , lagWithDefault_ (row ^. statusNumBikesDisabled     ) (val_ 1) (row ^. statusNumBikesDisabled     ) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableEfit  ) (val_ 1) (row ^. vehicleTypesAvailableEfit  ) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableEfitG5) (val_ 1) (row ^. vehicleTypesAvailableEfitG5) `over_` w
                                ))
                     statusForStation

  pure $ do
    stationInfo' <- reuse stationInfo
    guard_ (_infoIsChargingStation stationInfo' ==. val_ True &&. _infoActive stationInfo' ==. val_ True)
    chargingsSum <-
      aggregate_ (\(row, pBikesDisabled, pEFit, pEFitG5) ->
                    let dBikesDisabled = row ^. statusNumBikesDisabled - pBikesDisabled
                        dEFit          = row ^. vehicleTypesAvailableEfit   - pEFit
                        dEFitG5        = row ^. vehicleTypesAvailableEfitG5 - pEFitG5
                    in
                     ( group_ ((_unInformationStationId . _statusInfoId . _statusCommon) row)
                     -- Sum of all instances where an e-bike was disabled, then enabled.
                     , fromMaybe_ 0 $ sum_ dBikesDisabled  `filterWhere_` (dBikesDisabled  <. 0 &&. (dEFit >. 0 ||. dEFitG5 >. 0))
                     -- Sum of all instances where an E-Fit bike was disabled, then enabled.
                     , fromMaybe_ 0 $ sum_ dEFit           `filterWhere_` (dBikesDisabled  <. 0 &&. dEFit >. 0)
                     -- Sum of all instances where an E-Fit G5 bike was disabled, then enabled.
                     , fromMaybe_ 0 $ sum_ dEFitG5         `filterWhere_` (dBikesDisabled  <. 0 &&. dEFitG5 >. 0)
                     ))
                  (reuse cte)

    guard_' ( (chargingsSum ^. _1) ==?. _infoStationId stationInfo'
           )

    pure ( stationInfo'
         , chargingsSum ^. _2 -- sum of charging events over queried range (negative; reflects change in disabled bikes)
         , chargingsSum ^. _3 -- sum of E-Fit charging events over queried range (positive; reflects change in available e-fit)
         , chargingsSum ^. _4 -- sum of E-Fit G5 charging events over queried range (positive; reflects change in available e-fit g5)
         )
