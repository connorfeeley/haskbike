{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- |

module Database.BikeShare.Tables.StationStatusDelta
     ( PrimaryKey (..)
     , StationStatusDelta
     , StationStatusDeltaId
     , StationStatusDeltaT (..)
     , VehicleTypeMixin (..)
     , createStationStatusDelta
     , deltaBoost
     , deltaEfit
     , deltaEfitG5
     , deltaIconic
     , deltaId
     , deltaNumBikesAvailable
     , deltaNumBikesDisabled
     , deltaNumDocksAvailable
     , deltaNumDocksDisabled
     , deltaVehicleDocksAvailable
     , stationStatusDeltaModification
     ) where

import           Control.Lens

import           Data.Int
import           Data.String                             ( IsString, fromString )
import qualified Data.Text                               as T

import           Database.Beam
import           Database.Beam.Backend                   ( timestampType )
import           Database.Beam.Backend.SQL.Builder
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.BikeShare.Tables.StationStatus

data StationStatusDeltaT f where
  StationStatusDelta ::
    { _deltaId                    :: PrimaryKey StationStatusT f
    , _deltaNumBikesAvailable     :: Columnar f Int32
    , _deltaNumBikesDisabled      :: Columnar f Int32
    , _deltaNumDocksAvailable     :: Columnar f Int32
    , _deltaNumDocksDisabled      :: Columnar f Int32
    , _deltaVehicleDocksAvailable :: Columnar f Int32
    , _deltaVehicleTypesAvailable :: VehicleTypeMixin f
    } -> StationStatusDeltaT f
  deriving (Generic, Beamable)

-- | Synonym for the table type.
type StationStatusDelta = StationStatusDeltaT Identity
type StationStatusDeltaId = PrimaryKey StationStatusDeltaT Identity
deriving instance Show StationStatusDeltaId
deriving instance Show StationStatusDelta

-- | Inform Beam about the table.
instance Table StationStatusDeltaT where
  data PrimaryKey StationStatusDeltaT f =
    StationStatusDeltaId { _unDeltaId :: PrimaryKey StationStatusT f }
    deriving (Generic, Beamable)
  primaryKey = StationStatusDeltaId <$> _deltaId

-- | StationStatusDelta Lenses
deltaId             :: Getter (StationStatusDeltaT Identity) (PrimaryKey StationStatusT Identity)
deltaNumBikesAvailable     :: Lens' (StationStatusDeltaT f) (C f Int32)
deltaNumBikesDisabled      :: Lens' (StationStatusDeltaT f) (C f Int32)
deltaNumDocksAvailable     :: Lens' (StationStatusDeltaT f) (C f Int32)
deltaNumDocksDisabled      :: Lens' (StationStatusDeltaT f) (C f Int32)
deltaVehicleDocksAvailable :: Lens' (StationStatusDeltaT f) (C f Int32)
deltaBoost  :: Lens' (StationStatusDeltaT f) (C f Int32)
deltaIconic :: Lens' (StationStatusDeltaT f) (C f Int32)
deltaEfit   :: Lens' (StationStatusDeltaT f) (C f Int32)
deltaEfitG5 :: Lens' (StationStatusDeltaT f) (C f Int32)

deltaId                                                                   = to _deltaId
StationStatusDelta _ (LensFor deltaNumBikesAvailable)           _ _ _ _ _ = tableLenses
StationStatusDelta _ _ (LensFor deltaNumBikesDisabled)            _ _ _ _ = tableLenses
StationStatusDelta _ _ _ (LensFor deltaNumDocksAvailable)           _ _ _ = tableLenses
StationStatusDelta _ _ _ _ (LensFor deltaNumDocksDisabled)            _ _ = tableLenses
StationStatusDelta _ _ _ _ _ (LensFor deltaVehicleDocksAvailable)       _ = tableLenses
StationStatusDelta _ _ _ _ _ _ (VehicleType (LensFor deltaBoost) _ _ _)   = tableLenses
StationStatusDelta _ _ _ _ _ _ (VehicleType _ (LensFor deltaIconic) _ _)  = tableLenses
StationStatusDelta _ _ _ _ _ _ (VehicleType _ _ (LensFor deltaEfit)    _) = tableLenses
StationStatusDelta _ _ _ _ _ _ (VehicleType _ _ _ (LensFor deltaEfitG5))  = tableLenses

deltaVehicleTypeFields :: IsString (Columnar f Int32) => String -> VehicleTypeMixin f
deltaVehicleTypeFields b =
  VehicleType (fromString (b <> "_boost"))
              (fromString (b <> "_iconic"))
              (fromString (b <> "_efit"))
              (fromString (b <> "_efit_g5"))

-- * Table modifications and migrations.

-- | Table modifications for 'StationStatus' table.
stationStatusDeltaModification :: EntityModification (DatabaseEntity be db) be (TableEntity StationStatusDeltaT)
stationStatusDeltaModification =
  setEntityName "station_status_delta" <> modifyTableFields tableModification
  { _deltaId                    = StationStatusId (StationInformationId "id") "last_reported"
  , _deltaNumBikesAvailable     = "delta_bikes_available"
  , _deltaNumBikesDisabled      = "delta_bikes_disabled"
  , _deltaNumDocksAvailable     = "delta_docks_available"
  , _deltaNumDocksDisabled      = "delta_docks_disabled"
  , _deltaVehicleDocksAvailable = "vehicle_docks_available"
  , _deltaVehicleTypesAvailable = deltaVehicleTypeFields "delta"
  }

-- | Migration for the StationStatus table.
createStationStatusDelta :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity StationStatusDeltaT))
createStationStatusDelta =
  createTable "station_status_delta" $ StationStatusDelta
  { _deltaId                    = StationStatusId
                                  (StationInformationId (field "id" int notNull))
                                  (field "last_reported" (DataType (timestampType Nothing True)) notNull)
  , _deltaNumBikesAvailable     = field "num_bikes_available"     int notNull
  , _deltaNumBikesDisabled      = field "num_bikes_disabled"      int notNull
  , _deltaNumDocksAvailable     = field "num_docks_available"     int notNull
  , _deltaNumDocksDisabled      = field "num_docks_disabled"      int notNull
  , _deltaVehicleDocksAvailable = field "vehicle_docks_available" int notNull
  , _deltaVehicleTypesAvailable = VehicleType (field "vehicle_types_available_boost"   int)
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
