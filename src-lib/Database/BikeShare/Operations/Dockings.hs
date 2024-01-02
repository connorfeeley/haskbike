{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

-- Signatures of beam-related functions are incredibly verbose, so let's settle for partial type signatures.
-- Sometimes it is straight up impossible to write the types down because of ambiguous types.
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE PartialTypeSignatures     #-}

-- | This module contains operations to query the number of dockings and undockings for a station.

module Database.BikeShare.Operations.Dockings
     ( queryChargingEventsCount
     , queryDockingEventsCount
     ) where


import           API.VehicleType

import           AppEnv

import           Control.Lens                                 hiding ( reuse, (.=), (<.) )

import           Data.Int                                     ( Int32 )

import           Database.Beam
import qualified Database.Beam.Postgres                       as Pg
import           Database.BikeShare
import           Database.BikeShare.EventCounts
import           Database.BikeShare.StatusVariationQuery
import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationStatus


-- | Query the number of dockings and undockings for a station.
queryDockingEventsCount :: StatusVariationQuery -> AppM [DockingEventsCount]
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
queryDockingEventsCountExpr' :: StatusVariationQuery -> AppM [(StationInformation, (Int32, Int32), (Int32, Int32), (Int32, Int32))]
queryDockingEventsCountExpr' variation = withPostgres $ runSelectReturningList $ selectWith $ do
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId  . _statusInfoId) row)) (orderPartitionBy_ (asc_ (_statusLastReported row))) noBounds_)
                     (\row w -> ( row
                                , lagWithDefault_ (row ^. vehicleTypesAvailableIconic) (val_ 1) (row ^. vehicleTypesAvailableIconic) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableEfit  ) (val_ 1) (row ^. vehicleTypesAvailableEfit  ) `over_` w
                                , lagWithDefault_ (row ^. vehicleTypesAvailableEfitG5) (val_ 1) (row ^. vehicleTypesAvailableEfitG5) `over_` w
                                ))
                     statusForStation
  pure $ do
    -- Join the station information with the dockings and undockings.
    -- status <- reuse cte
    result <- do -- Pg.pgNubBy_ (_infoStationId . (^. _1 . _1)) $ lateral_ status $ \status' -> do
      statusSums <-
        aggregate_ (\(row, pIconic, pEFit, pEFitG5) ->
                      let
                          dIconic = row ^. vehicleTypesAvailableIconic - pIconic
                          dEFit   = row ^. vehicleTypesAvailableEfit   - pEFit
                          dEFitG5 = row ^. vehicleTypesAvailableEfitG5 - pEFitG5
                      in
                       ( group_ ((_unInformationStationId  . _statusInfoId) row)
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
      rankedInfo <- do
        Pg.pgNubBy_ (_infoStationId . (^. _1)) $
          withWindow_ (\row -> frame_ (partitionBy_ (_infoStationId row)) (orderPartitionBy_ (desc_ (_infoId row))) noBounds_)
                      (\row w -> ( row
                                 , rank_ `over_` w
                                 )
                      ) (all_ (bikeshareDb ^. bikeshareStationInformation))

      guard_' (      -- _statusInfoId      (status ^. _1) `references_'` (rankedInfo ^. _1)
                -- &&?. _statusStationId   (status ^. _1) ==?. (statusSums ^. _1)
                -- &&?. _infoStationId (rankedInfo ^. _1) ==?. _statusStationId (status ^. _1)
                -- &&?.
        _infoStationId (rankedInfo ^. _1) ==?. (statusSums ^. _1)
                -- &&?. (statusSums ^. _1)                ==?. _statusStationId (status ^. _1)
                &&?. (statusSums ^. _1)                ==?. _infoStationId (rankedInfo ^. _1)
                -- &&?.  rankedInfo ^. _2                 ==?. val_ 1
              )
      -- guard_' ((events' ^. _1) ==?. _infoStationId (rankedInfo ^. _1))
      pure ( rankedInfo ^. _1
           -- , (events' ^. _2 . _1, events' ^. _3 . _1) -- Boost
           --      Undockings   |     Dockings
           , (statusSums ^. _2 . _1, statusSums ^. _3 . _1) -- Iconic
           , (statusSums ^. _2 . _2, statusSums ^. _3 . _2) -- E-Fit
           , (statusSums ^. _2 . _3, statusSums ^. _3 . _3) -- E-Fit G5
           )
      -- pure (rankedInfo, statusSums)
    pure result

    -- guard_' (      _statusInfoId      (status ^. _1) `references_'` (info' ^. _1)
    --           &&?. _statusStationId   (status ^. _1) ==?. (events' ^. _1)
    --           &&?. _infoStationId (info' ^. _1)      ==?. (events' ^. _1)
    --           &&?. _infoStationId (info' ^. _1)      ==?. _statusStationId (status ^. _1)
    --           &&?. (events' ^. _1)                   ==?. _statusStationId (status ^. _1)
    --           &&?. (events' ^. _1)                   ==?. _infoStationId (info' ^. _1)
    --           &&?. info' ^. _2 ==?. val_ 1
    --         )

    -- -- Return tuples of station information and the dockings and undockings.
    -- pure ( result ^. _1 . _1
    --      -- , (events' ^. _2 . _1, events' ^. _3 . _1) -- Boost
    --      --      Undockings   |     Dockings
    --      , (result ^. _2 . _2 . _1, result ^. _2 . _3 . _1) -- Iconic
    --      , (result ^. _2 . _2 . _2, result ^. _2 . _3 . _2) -- E-Fit
    --      , (result ^. _2 . _2 . _3, result ^. _2 . _3 . _3) -- E-Fit G5
    --      )


---------------------------------

-- | Query the number of charging events for a station (returning status record and tuples of (dDisabled, dEfit, dEfitG5, sumDisabled, sumEfit, sumEfitG5).
queryChargingEventsCount :: StatusVariationQuery -> AppM [(StationInformation, Int32, Int32, Int32)]
queryChargingEventsCount variation = withPostgres $ runSelectReturningList $ selectWith $ do
  stationInfo <- selecting $ all_ (bikeshareDb ^. bikeshareStationInformation)
  -- Lag expression
  cte <- selecting $ do
    let statusForStation = filter_ (filterFor_ variation)
                                   (all_ (bikeshareDb ^. bikeshareStationStatus))
      in withWindow_ (\row -> frame_ (partitionBy_ ((_unInformationStationId  . _statusInfoId) row)) (orderPartitionBy_ (asc_ $ _statusLastReported row)) noBounds_)
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
                     ( group_ ((_unInformationStationId  . _statusInfoId) row)
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
