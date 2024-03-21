{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Mixin definition for common fields between 'StationStatus' and 'StationStatusDelta' tables.

module Haskbike.Database.Schema.V001.StationStatusCommon
     ( StationStatusCommon
     , StationStatusCommonMixin (..)
     , stationStatusCommonFields
     , statusCommonInfoId
     , statusCommonIsChargingStation
     , statusCommonLastReported
     , statusCommonNumBikesAvailable
     , statusCommonNumBikesDisabled
     , statusCommonNumDocksAvailable
     , statusCommonNumDocksDisabled
     , statusCommonStationId
     ) where

import           Control.Lens

import           Data.Int
import           Data.String                                       ( IsString (fromString) )
import           Data.Time

import           Database.Beam
import           Haskbike.Database.Schema.V001.StationInformation


-- | Common fields between 'StationStatus' and 'StationStatusDelta' tables.
data StationStatusCommonMixin f where
  StationStatusCommon :: { _statusInfoId                :: PrimaryKey StationInformationT f
                         , _statusStationId             :: Columnar f Int32
                         , _statusLastReported          :: Columnar f UTCTime
                         , _statusNumBikesAvailable     :: Columnar f Int32
                         , _statusNumBikesDisabled      :: Columnar f Int32
                         , _statusNumDocksAvailable     :: Columnar f Int32
                         , _statusNumDocksDisabled      :: Columnar f Int32
                         , _statusIsChargingStation     :: Columnar f Bool
                         } -> StationStatusCommonMixin f
  deriving (Generic, Beamable)

type StationStatusCommon = StationStatusCommonMixin Identity
deriving instance Show (StationStatusCommonMixin Identity)
deriving instance Eq (StationStatusCommonMixin Identity)

-- | Database field names.
stationStatusCommonFields :: (IsString (Columnar f Int32), IsString (Columnar f UTCTime), IsString (Columnar f Bool))
                          => String -- ^ Field name prefix.
                          -> StationStatusCommonMixin f
stationStatusCommonFields b =
  StationStatusCommon (StationInformationId (fromString (b <> "info_station_id"))
                                            (fromString (b <> "info_reported")))
                      (fromString (b <> "station_id"))
                      (fromString (b <> "last_reported"))
                      (fromString (b <> "num_bikes_available"))
                      (fromString (b <> "num_bikes_disabled"))
                      (fromString (b <> "num_docks_available"))
                      (fromString (b <> "num_docks_disabled"))
                      (fromString (b <> "is_charging_station"))


-- | StationStatusCommon Lenses
statusCommonInfoId            :: (Profunctor p, Contravariant f1) => Optic' p f1 (StationStatusCommonMixin f2) (PrimaryKey StationInformationT f2)
statusCommonStationId         :: Lens' StationStatusCommon Int32
statusCommonLastReported      :: Lens' StationStatusCommon UTCTime
statusCommonNumBikesAvailable :: Lens' StationStatusCommon Int32
statusCommonNumBikesDisabled  :: Lens' StationStatusCommon Int32
statusCommonNumDocksAvailable :: Lens' StationStatusCommon Int32
statusCommonNumDocksDisabled  :: Lens' StationStatusCommon Int32
statusCommonIsChargingStation :: Lens' StationStatusCommon Bool

statusCommonInfoId = to _statusInfoId
StationStatusCommon _ (LensFor statusCommonStationId)                     _ _ _ _ _ _  = tableLenses
StationStatusCommon _ _ (LensFor statusCommonLastReported)                  _ _ _ _ _  = tableLenses
StationStatusCommon _ _ _ (LensFor statusCommonNumBikesAvailable)             _ _ _ _  = tableLenses
StationStatusCommon _ _ _ _ (LensFor statusCommonNumBikesDisabled)              _ _ _  = tableLenses
StationStatusCommon _ _ _ _ _ (LensFor statusCommonNumDocksAvailable)             _ _  = tableLenses
StationStatusCommon _ _ _ _ _ _ (LensFor statusCommonNumDocksDisabled)              _  = tableLenses
StationStatusCommon _ _ _ _ _ _ _ (LensFor statusCommonIsChargingStation)              = tableLenses
