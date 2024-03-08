{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |

module Database.BikeShare.Schema.V001.StationStatusChanges
     ( PrimaryKey (..)
     , StationStatusChanges
     , StationStatusChangesId
     , StationStatusChangesT (..)
     , VehicleTypeMixin (..)
     , createStationStatusChanges
     , stationStatusChgModification
     , statusChgCommon
     , statusChgInfoId
     , statusIsChargingStation
     , statusLastReported
     , statusNumBikesAvailable
     , statusNumBikesDisabled
     , statusNumDocksAvailable
     , statusNumDocksDisabled
     , statusStationId
     , vehicleTypesAvailableBoost
     , vehicleTypesAvailableEfit
     , vehicleTypesAvailableEfitG5
     , vehicleTypesAvailableIconic
     ) where

import           Control.Lens

import           Data.Int
import qualified Data.Text                                          as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend                              ( timestampType )
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.BikeShare.Schema.V001.StationStatusCommon
import           Database.BikeShare.Schema.V001.VehicleTypeMixin
import           Database.BikeShare.Tables.StationInformation

-- | Schema for 'StationStatusChanges': a stripped down 'StationStatus' table.
data StationStatusChangesT f where
  StationStatusChanges ::
    { _statusChgCommon                :: StationStatusCommonMixin f
    , _statusChgVehicleTypesAvailable :: VehicleTypeMixin f
    } -> StationStatusChangesT f
  deriving (Generic, Beamable)

-- | Synonym for the table type.
type StationStatusChanges = StationStatusChangesT Identity
type StationStatusChangesId = PrimaryKey StationStatusChangesT Identity
deriving instance Show StationStatusChangesId
deriving instance Show StationStatusChanges

-- | Inform Beam about the table.
instance Table StationStatusChangesT where
  data PrimaryKey StationStatusChangesT f =
    StationStatusChangesId { _unStatusStationId    :: PrimaryKey StationInformationT f
                         , _unStatusLastReported :: Columnar f UTCTime
                         }
    deriving (Generic, Beamable)
  primaryKey = StationStatusChangesId <$> (_statusInfoId . _statusChgCommon)  <*> (_statusLastReported . _statusChgCommon)

-- | StationStatusChanges Lenses
statusChgCommon             :: Getter (StationStatusChangesT f) (StationStatusCommonMixin f)
statusChgInfoId             :: Getter (StationStatusChangesT f) (PrimaryKey StationInformationT f)
statusStationId             :: Lens' (StationStatusChangesT f) (C f Int32)
statusLastReported          :: Lens' (StationStatusChangesT f) (C f UTCTime)
statusNumBikesAvailable     :: Lens' (StationStatusChangesT f) (C f Int32)
statusNumBikesDisabled      :: Lens' (StationStatusChangesT f) (C f Int32)
statusNumDocksAvailable     :: Lens' (StationStatusChangesT f) (C f Int32)
statusNumDocksDisabled      :: Lens' (StationStatusChangesT f) (C f Int32)
statusIsChargingStation     :: Lens' (StationStatusChangesT f) (C f Bool)
vehicleTypesAvailableBoost  :: Lens' (StationStatusChangesT f) (C f Int32)
vehicleTypesAvailableIconic :: Lens' (StationStatusChangesT f) (C f Int32)
vehicleTypesAvailableEfit   :: Lens' (StationStatusChangesT f) (C f Int32)
vehicleTypesAvailableEfitG5 :: Lens' (StationStatusChangesT f) (C f Int32)

statusChgCommon = to _statusChgCommon
statusChgInfoId = to (_statusInfoId . _statusChgCommon)
StationStatusChanges (StationStatusCommon _ (LensFor statusStationId)         _ _ _ _ _ _) _ = tableLenses
StationStatusChanges (StationStatusCommon _ _ (LensFor statusLastReported)      _ _ _ _ _) _ = tableLenses
StationStatusChanges (StationStatusCommon _ _ _ (LensFor statusNumBikesAvailable) _ _ _ _) _ = tableLenses
StationStatusChanges (StationStatusCommon _ _ _ _ (LensFor statusNumBikesDisabled)  _ _ _) _ = tableLenses
StationStatusChanges (StationStatusCommon _ _ _ _ _ (LensFor statusNumDocksAvailable) _ _) _ = tableLenses
StationStatusChanges (StationStatusCommon _ _ _ _ _ _ (LensFor statusNumDocksDisabled)  _) _ = tableLenses
StationStatusChanges (StationStatusCommon _ _ _ _ _ _ _ (LensFor statusIsChargingStation)) _ = tableLenses
StationStatusChanges _ (VehicleType (LensFor vehicleTypesAvailableBoost)   _ _ _)            = tableLenses
StationStatusChanges _ (VehicleType _ (LensFor vehicleTypesAvailableIconic)  _ _)            = tableLenses
StationStatusChanges _ (VehicleType _ _ (LensFor vehicleTypesAvailableEfit)    _)            = tableLenses
StationStatusChanges _ (VehicleType _ _ _ (LensFor vehicleTypesAvailableEfitG5) )            = tableLenses

-- * Table modifications and migrations.

-- | Table modifications for 'StationStatus' table.
stationStatusChgModification :: EntityModification (DatabaseEntity be db) be (TableEntity StationStatusChangesT)
stationStatusChgModification =
  setEntityName "station_status_status_changes" <> modifyTableFields tableModification
  { _statusChgCommon                = stationStatusCommonFields "" -- ^ No prefix, to stay backwards compatible with non-mixin schema.
  , _statusChgVehicleTypesAvailable = vehicleTypeFields "vehicle_types_available_"
  }

-- | Migration for the StationStatus table.
createStationStatusChanges :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity StationStatusChangesT))
createStationStatusChanges =
  createTable "station_status_status_changes" $ StationStatusChanges
  { _statusChgCommon                = StationStatusCommon (StationInformationId (field "info_station_id" int notNull)
                                                                            (field "info_reported" (DataType (timestampType Nothing True)) notNull))
                                                      (field "station_id"              int notNull)
                                                      (field "last_reported"           (DataType (timestampType Nothing True)) notNull)
                                                      (field "num_bikes_available"     int notNull)
                                                      (field "num_bikes_disabled"      int notNull)
                                                      (field "num_docks_available"     int notNull)
                                                      (field "num_docks_disabled"      int notNull)
                                                      (field "is_charging_station"     boolean notNull)
  , _statusChgVehicleTypesAvailable = VehicleType (field "vehicle_types_available_boost"   int)
                                              (field "vehicle_types_available_iconic"  int)
                                              (field "vehicle_types_available_efit"    int)
                                              (field "vehicle_types_available_efit_g5" int)
  }

referenceStatusTable :: BeamMigrateSqlBackend be => Constraint be
referenceStatusTable = Constraint $ referencesConstraintSyntax "station_status" ["station_id", "last_reported"]
                       Nothing
                       (Just referentialActionCascadeSyntax)
                       Nothing

referenceStatusTableField :: BeamMigrateSqlBackend be => T.Text -> Constraint be
referenceStatusTableField referenceField = Constraint $ referencesConstraintSyntax "station_status" [referenceField]
                       Nothing
                       (Just referentialActionCascadeSyntax)
                       Nothing
