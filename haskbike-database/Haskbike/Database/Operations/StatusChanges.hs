-- | This module contains the operations for inserting station status changes into the database.

module Haskbike.Database.Operations.StatusChanges
     ( insertChangedStationStatus
     , insertChangedStationStatusGeneric
     , populateChangedStationStatusTable
     ) where

import           Control.Lens                                hiding ( reuse, (<.) )

import           Data.Int                                    ( Int32 )

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres

import           Haskbike.Database.BikeShare
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus


{- |
Insert only changed station statuses into the database.
-}
insertChangedStationStatus :: MonadBeamInsertReturning Postgres m => m [StationStatusT Identity]
insertChangedStationStatus = insertChangedStationStatusGeneric (Just 1)


{- |
Populate the changed station statuses table from the contents of the station status table.
-}
populateChangedStationStatusTable :: MonadBeamInsertReturning Postgres m => m [StationStatusT Identity]
populateChangedStationStatusTable = insertChangedStationStatusGeneric Nothing


{- |
Generic function to populate the changed station statuses table from the contents of the station status table.
'Maybe' argument is the maximum row number to consider, or 'Nothing' to consider all rows.
Use 'Just 1' to consider only the latest status row for each station.
-}
insertChangedStationStatusGeneric :: MonadBeamInsertReturning Postgres m => Maybe Int32 -> m [StationStatusT Identity]
insertChangedStationStatusGeneric maxRowNumber = do
  runInsertReturningList $
    insertOnConflict (bikeshareDb ^. bikeshareStationStatusChanges)
    (insertFrom $ do
        ( row, rowNum
          , ( pBikesAvail, pBikesDisab, pDocksAvail, pDocksDisab )
          , ( pIsChargingStation, pStatus, pIsInstalled, pIsRenting, pIsReturning )
          , ( pVehicleDocksAvailable, pIconic, pEfit, pEfitG5)
          ) <-
          withWindow_ (\row ->
                         frame_ (partitionBy_ ((_unInformationStationId . _statusInfoId . _statusCommon) row))
                         (orderPartitionBy_ ((desc_ . _statusLastReported . _statusCommon) row))
                         noBounds_)
          (\row w -> ( row
                     , rowNumber_ `over_` w
                     -- , lag_ (row ^. statusLastReported         ) (val_ 1) `over_` w
                     , ( lead_ (row ^. statusNumBikesAvailable    ) (val_ (1 :: Integer)) `over_` w
                       , lead_ (row ^. statusNumBikesDisabled     ) (val_ (1 :: Integer)) `over_` w
                       , lead_ (row ^. statusNumDocksAvailable    ) (val_ (1 :: Integer)) `over_` w
                       , lead_ (row ^. statusNumDocksDisabled     ) (val_ (1 :: Integer)) `over_` w
                       )
                     , ( lead_ (row ^. statusIsChargingStation    ) (val_ (1 :: Integer)) `over_` w
                       , lead_ (row ^. statusStatus               ) (val_ (1 :: Integer)) `over_` w
                       , lead_ (row ^. statusIsInstalled          ) (val_ (1 :: Integer)) `over_` w
                       , lead_ (row ^. statusIsRenting            ) (val_ (1 :: Integer)) `over_` w
                       , lead_ (row ^. statusIsReturning          ) (val_ (1 :: Integer)) `over_` w
                       -- Traffic is null - don't compare!
                     )
                     , ( lead_ (row ^. statusVehicleDocksAvailable) (val_ (1 :: Integer)) `over_` w
                       , lead_ (row ^. vehicleTypesAvailableIconic) (val_ (1 :: Integer)) `over_` w
                       , lead_ (row ^. vehicleTypesAvailableEfit  ) (val_ (1 :: Integer)) `over_` w
                       , lead_ (row ^. vehicleTypesAvailableEfitG5) (val_ (1 :: Integer)) `over_` w
                       )
                     )
          ) $
          all_ (bikeshareDb ^. bikeshareStationStatus)

        guard_' ( maxRowNumberCond rowNum maxRowNumber &&?.
                  (((row ^. statusNumBikesAvailable      ) /=?. pBikesAvail            ||?.
                    (row ^. statusNumBikesDisabled       ) /=?. pBikesDisab            ||?.
                    (row ^. statusNumDocksAvailable      ) /=?. pDocksAvail            ||?.
                    (row ^. statusNumDocksDisabled       ) /=?. pDocksDisab
                   ) ||?.
                    ((row ^. statusIsChargingStation     ) /=?. pIsChargingStation     ||?.
                     (row ^. statusStatus                ) /=?. pStatus                ||?.
                     (row ^. statusIsInstalled           ) /=?. pIsInstalled           ||?.
                     (row ^. statusIsRenting             ) /=?. pIsRenting             ||?.
                     (row ^. statusIsReturning           ) /=?. pIsReturning
                      -- Traffic is null - don't compare!
                    ) ||?.
                    ((row ^. statusVehicleDocksAvailable ) /=?. pVehicleDocksAvailable ||?.
                     (row ^. vehicleTypesAvailableIconic ) /=?. pIconic                ||?.
                     (row ^. vehicleTypesAvailableEfit   ) /=?. pEfit                  ||?.
                     (row ^. vehicleTypesAvailableEfitG5 ) /=?. pEfitG5
                    )
                  ))
        pure row
    ) (conflictingFields primaryKey) onConflictDoNothing
  where
    -- Optionally limit the number of rows to process.
    -- Use Nothing to process all rows (initialize status changes table from ALL status records).
    -- Use (Just 1) to process only the latest status row for each station.
    maxRowNumberCond rowNum (Just maxRow) = sqlBool_ (rowNum <=. val_ maxRow)
    maxRowNumberCond _      Nothing       = sqlBool_ (val_ True)
