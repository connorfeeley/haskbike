{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Common Toronto vehicle type definition.

module Database.BikeShare.Schema.V001.VehicleTypeMixin
     ( VehicleTypeMixin (..)
     , availableBoost
     , availableEfit
     , availableEfitG5
     , availableIconic
     , vehicleTypeFields
     , vehicleTypesAvailable
     ) where

import           Control.Lens

import           Data.Int
import           Data.String                   ( IsString (fromString) )

import           Database.Beam
import           Database.Beam.Postgres        ( Postgres )
import           Database.Beam.Postgres.Syntax ( pgTextType )

-- | Vehicle type available columns.
data VehicleTypeMixin f where
  VehicleType :: { _availableBoost  :: Columnar f Int32
                 , _availableIconic :: Columnar f Int32
                 , _availableEfit   :: Columnar f Int32
                 , _availableEfitG5 :: Columnar f Int32
                 } -> VehicleTypeMixin f
  deriving (Generic, Beamable)
type VehicleType = VehicleTypeMixin Identity
deriving instance Show (VehicleTypeMixin Identity)
deriving instance Eq (VehicleTypeMixin Identity)

vehicleTypeFields :: IsString (Columnar f Int32) => String -> VehicleTypeMixin f
vehicleTypeFields b =
  VehicleType (fromString (b <> "boost"))
              (fromString (b <> "iconic"))
              (fromString (b <> "efit"))
              (fromString (b <> "efit_g5"))

vehicleTypesAvailable :: DataType Postgres VehicleType
vehicleTypesAvailable = DataType pgTextType

-- | VehicleType Lenses
availableBoost   :: Lens' VehicleType Int32
availableIconic  :: Lens' VehicleType Int32
availableEfit    :: Lens' VehicleType Int32
availableEfitG5  :: Lens' VehicleType Int32

VehicleType (LensFor availableBoost)  _ _ _ = tableLenses
VehicleType _ (LensFor availableIconic) _ _ = tableLenses
VehicleType _ _ (LensFor availableEfit)   _ = tableLenses
VehicleType _ _ _ (LensFor availableEfitG5) = tableLenses
