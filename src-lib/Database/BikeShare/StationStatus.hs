-- | Station infrormation table definition and functions.

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Database.BikeShare.StationStatus
     ( BeamStationStatusString (..)
     , PrimaryKey (StationStatusId)
     , ReportTime (..)
     , StationStatus
     , StationStatusId
     , StationStatusT (..)
     , VehicleTypeMixin (..)
     , available_boost
     , available_efit
     , available_efit_g5
     , available_iconic
     , d_status_active
     , d_status_id
     , d_status_info_id
     , d_status_is_charging_station
     , d_status_last_reported
     , d_status_num_bikes_available
     , d_status_num_bikes_disabled
     , d_status_num_docks_available
     , d_status_num_docks_disabled
     , d_status_station_id
     , fromBeamStationStatusToJSON
     , fromJSONToBeamStationStatus
     , reportTimeType
     , stationStatusType
     , vehicleTypeFields
     , vehicleTypesAvailable
     , vehicle_types_available_boost
     , vehicle_types_available_efit
     , vehicle_types_available_efit_g5
     , vehicle_types_available_iconic
     ) where

import qualified API.Types                                  as AT

import           Control.Lens

import qualified Data.ByteString.Char8                      as B
import           Data.Coerce                                ( coerce )
import           Data.Int
import           Data.List                                  ( find )
import           Data.String                                ( IsString (fromString) )
import qualified Data.Text                                  as Text

import           Database.Beam
import           Database.Beam.Backend                      ( BeamBackend, HasSqlValueSyntax (sqlValueSyntax),
                                                              SqlSerial (..) )
import           Database.Beam.Postgres                     ( Postgres )
import           Database.Beam.Postgres.Syntax              ( pgTextType )
import           Database.BikeShare.StationInformation
import           Database.PostgreSQL.Simple.FromField       ( Field (typeOid), FromField (..), ResultError (..),
                                                              returnError, typoid )
import           Database.PostgreSQL.Simple.ToField         ( ToField (..) )
import           Database.PostgreSQL.Simple.TypeInfo.Static ( text )

import           ReportTime


-- | Declare a (Beam) table for the 'StationStatus' type.
data StationStatusT f where
  StationStatus :: { _d_status_id                      :: Columnar f (SqlSerial Int32)
                   , _d_status_info_id                 :: PrimaryKey StationInformationT f
                   , _d_status_station_id              :: Columnar f Int32
                   , _d_status_num_bikes_available     :: Columnar f Int32
                   , _d_status_num_bikes_disabled      :: Columnar f Int32
                   , _d_status_num_docks_available     :: Columnar f Int32
                   , _d_status_num_docks_disabled      :: Columnar f Int32
                   , _d_status_last_reported           :: Columnar f (Maybe ReportTime) -- In UTC time
                   , _d_status_is_charging_station     :: Columnar f Bool
                   , _d_status_status                  :: Columnar f BeamStationStatusString
                   , _d_status_is_installed            :: Columnar f Bool
                   , _d_status_is_renting              :: Columnar f Bool
                   , _d_status_is_returning            :: Columnar f Bool
                   , _d_status_traffic                 :: Columnar f (Maybe Text.Text) -- PBSC doesn't seem to set this field
                   , _d_status_vehicle_docks_available :: Columnar f Int32
                   , _d_status_vehicle_types_available :: VehicleTypeMixin f
                   , _d_status_active                  :: Columnar f Bool -- Flag indicating if the record is active or not
                   } -> StationStatusT f
  deriving (Generic, Beamable)

-- | Synonym for the table type.
type StationStatus = StationStatusT Identity
type StationStatusId = PrimaryKey StationStatusT Identity
deriving instance Show StationStatusId
deriving instance Eq StationStatusId
deriving instance Show StationStatus
deriving instance Eq StationStatus

-- | Inform Beam about the table.
instance Table StationStatusT where
  data PrimaryKey StationStatusT f = StationStatusId (Columnar f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = StationStatusId . _d_status_id

data VehicleTypeMixin f =
  VehicleType { _available_boost   :: Columnar f Int32
              , _available_iconic  :: Columnar f Int32
              , _available_efit    :: Columnar f Int32
              , _available_efit_g5 :: Columnar f Int32
              } deriving (Generic, Beamable)
type VehicleType = VehicleTypeMixin Identity
deriving instance Show (VehicleTypeMixin Identity)
deriving instance Eq (VehicleTypeMixin Identity)

vehicleTypeFields :: IsString (Columnar f Int32) => String -> VehicleTypeMixin f
vehicleTypeFields b =
  VehicleType (fromString (b <> "_boost"))
              (fromString (b <> "_iconic"))
              (fromString (b <> "_efit"))
              (fromString (b <> "_efit_g5"))

vehicleTypesAvailable :: DataType Postgres VehicleType
vehicleTypesAvailable = DataType pgTextType

-- | VehicleType Lenses
available_boost   :: Lens' VehicleType Int32
available_iconic  :: Lens' VehicleType Int32
available_efit    :: Lens' VehicleType Int32
available_efit_g5 :: Lens' VehicleType Int32

VehicleType (LensFor available_boost) _ _ _   = tableLenses
VehicleType _ (LensFor available_iconic) _ _  = tableLenses
VehicleType _ _ (LensFor available_efit) _    = tableLenses
VehicleType _ _ _ (LensFor available_efit_g5) = tableLenses

-- | StationStatus Lenses
d_status_id                      :: Lens' (StationStatusT f) (C f (SqlSerial Int32))
d_status_info_id                 :: Lens' (StationStatusT f) (C f Int32)
d_status_station_id              :: Lens' (StationStatusT f) (C f Int32)
d_status_num_bikes_available     :: Lens' (StationStatusT f) (C f Int32)
d_status_num_bikes_disabled      :: Lens' (StationStatusT f) (C f Int32)
d_status_num_docks_available     :: Lens' (StationStatusT f) (C f Int32)
d_status_num_docks_disabled      :: Lens' (StationStatusT f) (C f Int32)
d_status_last_reported           :: Lens' (StationStatusT f) (C f (Maybe ReportTime))
d_status_is_charging_station     :: Lens' (StationStatusT f) (C f Bool)
d_status_status                  :: Lens' (StationStatusT f) (C f BeamStationStatusString)
d_status_is_installed            :: Lens' (StationStatusT f) (C f Bool)
d_status_is_renting              :: Lens' (StationStatusT f) (C f Bool)
d_status_is_returning            :: Lens' (StationStatusT f) (C f Bool)
d_status_traffic                 :: Lens' (StationStatusT f) (C f (Maybe Text.Text))
d_status_vehicle_docks_available :: Lens' (StationStatusT f) (C f Int32)
vehicle_types_available_boost    :: Lens' (StationStatusT f) (C f Int32)
vehicle_types_available_iconic   :: Lens' (StationStatusT f) (C f Int32)
vehicle_types_available_efit     :: Lens' (StationStatusT f) (C f Int32)
vehicle_types_available_efit_g5  :: Lens' (StationStatusT f) (C f Int32)
d_status_active                  :: Lens' (StationStatusT f) (C f Bool)

StationStatus (LensFor d_status_id)                                         _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ (StationInformationId (LensFor d_status_info_id))             _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ (LensFor d_status_station_id)                                 _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ (LensFor d_status_num_bikes_available)                        _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ (LensFor d_status_num_bikes_disabled)                         _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ (LensFor d_status_num_docks_available)                        _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ (LensFor d_status_num_docks_disabled)                         _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ (LensFor d_status_last_reported)                              _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ (LensFor d_status_is_charging_station)                        _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ (LensFor d_status_status)                                     _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ (LensFor d_status_is_installed)                               _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ (LensFor d_status_is_renting)                                 _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ (LensFor d_status_is_returning)                               _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor d_status_traffic)                                    _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor d_status_vehicle_docks_available)                    _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType (LensFor vehicle_types_available_boost) _ _ _)   _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ (LensFor vehicle_types_available_iconic) _ _)  _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ _ (LensFor vehicle_types_available_efit)    _) _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ _ _ (LensFor vehicle_types_available_efit_g5)) _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor d_status_active)                                     = tableLenses

-- | Newtype wrapper for StationStatusString to allow us to define a custom FromBackendRow instance.
-- Don't want to implement database-specific code for the underlying StationStatusString type.
newtype BeamStationStatusString where
  BeamStationStatusString :: AT.StationStatusString -> BeamStationStatusString
  deriving (Eq, Generic, Show, Read) via AT.StationStatusString

instance (BeamBackend be, FromBackendRow be Text.Text) => FromBackendRow be BeamStationStatusString where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: Text.Text of
      "IN_SERVICE"  -> pure $ BeamStationStatusString AT.InService
      "END_OF_LIFE" -> pure $ BeamStationStatusString AT.EndOfLife
      _             -> fail ("Invalid value for BeamStationStatusString: " ++ Text.unpack val)

instance (HasSqlValueSyntax be String, Show BeamStationStatusString) => HasSqlValueSyntax be BeamStationStatusString where
  sqlValueSyntax = sqlValueSyntax . show

instance FromField BeamStationStatusString where
   fromField f mdata = do
     if typeOid f /= typoid text -- TODO: any way to determine this automatically?
        then returnError Incompatible f ""
        else case B.unpack `fmap` mdata of
               Nothing  -> returnError UnexpectedNull f ""
               Just dat ->
                  case [ x | (x,t) <- reads dat, ("","") <- lex t ] of
                    [x] -> return x
                    _   -> returnError ConversionFailed f dat

instance ToField BeamStationStatusString where
  toField = toField . show

stationStatusType :: DataType Postgres BeamStationStatusString
stationStatusType = DataType pgTextType

-- | Convert from the JSON StationStatus to the Beam StationStatus type
fromJSONToBeamStationStatus status =
  StationStatus { _d_status_id                       = default_
                , _d_status_info_id                  = StationInformationId $ fromIntegral $ status ^. AT.status_station_id
                , _d_status_station_id               = fromIntegral $ status ^. AT.status_station_id
                , _d_status_num_bikes_available      = fromIntegral $ status ^. AT.status_num_bikes_available
                , _d_status_num_bikes_disabled       = fromIntegral $ status ^. AT.status_num_bikes_disabled
                , _d_status_num_docks_available      = fromIntegral $ status ^. AT.status_num_docks_available
                , _d_status_num_docks_disabled       = fromIntegral $ status ^. AT.status_num_docks_disabled
                , _d_status_last_reported            = val_ (coerce $ status ^. AT.status_last_reported)
                , _d_status_is_charging_station      = val_ $ status ^. AT.status_is_charging_station
                , _d_status_status                   = val_ (coerce $ status ^. AT.status_status :: BeamStationStatusString)
                , _d_status_is_installed             = val_ $ status ^. AT.status_is_installed
                , _d_status_is_renting               = val_ $ status ^. AT.status_is_renting
                , _d_status_is_returning             = val_ $ status ^. AT.status_is_returning
                , _d_status_traffic                  = val_ $ fmap Text.pack $ status ^. AT.status_traffic
                , _d_status_vehicle_docks_available  = fromIntegral $ AT.dock_count $ head $ status ^. AT.status_vehicle_docks_available
                , _d_status_vehicle_types_available  = val_ $ VehicleType num_boost num_iconic num_efit num_efit_g5
                , _d_status_active                   = val_ True
                }
  where
    -- | Find the vehicle type in the list of vehicle types available; default to 0 if not found.
    findByType' vehicle_type = find (\x -> AT.vehicle_type_id x == vehicle_type) $ status ^. AT.status_vehicle_types_available
    findByType  vehicle_type = fromIntegral $ maybe 0 AT.type_count (findByType' vehicle_type)
    num_boost   = findByType AT.Boost
    num_iconic  = findByType AT.Iconic
    num_efit    = findByType AT.EFit
    num_efit_g5 = findByType AT.EFitG5

-- | Convert from the Beam StationStatus type to the JSON StationStatus
fromBeamStationStatusToJSON :: StationStatus -> AT.StationStatus
fromBeamStationStatusToJSON status =
  AT.StationStatus { AT._status_station_id                  = fromIntegral $ status^.d_status_station_id
                      , AT._status_num_bikes_available      = fromIntegral $ status^.d_status_num_bikes_available
                      , AT._status_num_bikes_disabled       = fromIntegral $ status^.d_status_num_bikes_disabled
                      , AT._status_num_docks_available      = fromIntegral $ status^.d_status_num_docks_available
                      , AT._status_num_docks_disabled       = fromIntegral $ status^.d_status_num_docks_disabled
                      , AT._status_last_reported            = coerce $ status^.d_status_last_reported
                      , AT._status_is_charging_station      = status^.d_status_is_charging_station
                      , AT._status_status                   = coerce $ status^.d_status_status
                      , AT._status_is_installed             = status^.d_status_is_installed
                      , AT._status_is_renting               = status^.d_status_is_renting
                      , AT._status_is_returning             = status^.d_status_is_returning
                      , AT._status_traffic                  = fmap Text.unpack $ status^.d_status_traffic
                      , AT._status_vehicle_docks_available  = [ AT.VehicleDock ["FIXME"] (fromIntegral $ status^.d_status_vehicle_docks_available) ]
                      , AT._status_vehicle_types_available  = [ AT.VehicleType AT.Boost  (fromIntegral (status^.vehicle_types_available_boost))
                                                              , AT.VehicleType AT.Iconic (fromIntegral (status^.vehicle_types_available_iconic))
                                                              , AT.VehicleType AT.EFit   (fromIntegral (status^.vehicle_types_available_efit))
                                                              , AT.VehicleType AT.EFitG5 (fromIntegral (status^.vehicle_types_available_efit_g5))
                                                              ]
                      }
