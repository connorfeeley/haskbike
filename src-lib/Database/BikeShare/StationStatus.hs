-- | Station infrormation table definition and functions.

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Database.BikeShare.StationStatus
     ( BeamStationStatusString (..)
     , PrimaryKey (..)
     , ReportTime (..)
     , StationStatus
     , StationStatusId
     , StationStatusT (..)
     , VehicleTypeMixin (..)
     , available_boost
     , available_efit
     , available_efit_g5
     , available_iconic
     , fromBeamStationStatusToJSON
     , fromJSONToBeamStationStatus
     , reportTimeType
     , stationStatusType
     , statusIsChargingStation
     , statusLastReported
     , statusNumBikesAvailable
     , statusNumBikesDisabled
     , statusNumDocksAvailable
     , statusNumDocksDisabled
     , statusStationId
     , unStatusLastReported
     , unStatusStationId
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
import           Database.Beam.Backend                      ( BeamBackend, HasSqlValueSyntax (sqlValueSyntax) )
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
  StationStatus :: { _statusStationId             :: PrimaryKey StationInformationT f
                   , _statusLastReported          :: Columnar f ReportTime -- In UTC time
                   , _statusNumBikesAvailable     :: Columnar f Int32
                   , _statusNumBikesDisabled      :: Columnar f Int32
                   , _statusNumDocksAvailable     :: Columnar f Int32
                   , _statusNumDocksDisabled      :: Columnar f Int32
                   , _statusIsChargingStation     :: Columnar f Bool
                   , _statusStatus                :: Columnar f BeamStationStatusString
                   , _statusIsInstalled           :: Columnar f Bool
                   , _statusIsRenting             :: Columnar f Bool
                   , _statusIsReturning           :: Columnar f Bool
                   , _statusTraffic               :: Columnar f (Maybe Text.Text) -- PBSC doesn't seem to set this field
                   , _statusVehicleDocksAvailable :: Columnar f Int32
                   , _statusVehicleTypesAvailable :: VehicleTypeMixin f
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
  data PrimaryKey StationStatusT f = StationStatusId { _unStatusStationId    :: PrimaryKey StationInformationT f
                                                     , _unStatusLastReported :: Columnar f ReportTime
                                                     }
    deriving (Generic, Beamable)
  primaryKey = StationStatusId <$> _statusStationId  <*> _statusLastReported

-- | Lenses
unStatusLastReported :: Lens' (PrimaryKey StationStatusT f) (Columnar f ReportTime)
unStatusLastReported key (StationStatusId stationId lastReported) = fmap (StationStatusId stationId) (key lastReported)
{-# INLINE unStatusLastReported #-}

unStatusStationId :: Lens' (PrimaryKey StationStatusT f) (PrimaryKey StationInformationT f)
unStatusStationId key (StationStatusId stationId lastReported) = fmap (`StationStatusId` lastReported) (key stationId)
{-# INLINE unStatusStationId #-}

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
-- statusStationId                  :: (Columnar f Int32 ~ PrimaryKey StationInformationT f, Functor f')
--                                  => (PrimaryKey StationInformationT f -> f' (PrimaryKey StationInformationT f))
--                                  -> StationStatusT f
--                                  -> f' (StationStatusT f)
-- statusStationId                  :: forall {f  :: Type -> Type}. Lens' (PrimaryKey StationInformationT f) (StationInformationT f)
statusStationId                 :: Getter (StationStatusT Identity) (PrimaryKey StationInformationT Identity)
statusStationId = to _statusStationId
statusLastReported              :: Lens' (StationStatusT f) (C f ReportTime)
statusNumBikesAvailable         :: Lens' (StationStatusT f) (C f Int32)
statusNumBikesDisabled          :: Lens' (StationStatusT f) (C f Int32)
statusNumDocksAvailable         :: Lens' (StationStatusT f) (C f Int32)
statusNumDocksDisabled          :: Lens' (StationStatusT f) (C f Int32)
statusIsChargingStation         :: Lens' (StationStatusT f) (C f Bool)
statusStatus                    :: Lens' (StationStatusT f) (C f BeamStationStatusString)
statusIsInstalled               :: Lens' (StationStatusT f) (C f Bool)
statusIsRenting                 :: Lens' (StationStatusT f) (C f Bool)
statusIsReturning               :: Lens' (StationStatusT f) (C f Bool)
statusTraffic                   :: Lens' (StationStatusT f) (C f (Maybe Text.Text))
statusVehicleDocksAvailable     :: Lens' (StationStatusT f) (C f Int32)
vehicle_types_available_boost   :: Lens' (StationStatusT f) (C f Int32)
vehicle_types_available_iconic  :: Lens' (StationStatusT f) (C f Int32)
vehicle_types_available_efit    :: Lens' (StationStatusT f) (C f Int32)
vehicle_types_available_efit_g5 :: Lens' (StationStatusT f) (C f Int32)

-- StationStatus (StationInformationId (LensFor statusStationId))              _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ (LensFor statusLastReported)                                  _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ (LensFor statusNumBikesAvailable)                        _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ (LensFor statusNumBikesDisabled)                         _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ (LensFor statusNumDocksAvailable)                        _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ (LensFor statusNumDocksDisabled)                         _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ (LensFor statusIsChargingStation)                        _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ (LensFor statusStatus)                                     _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ (LensFor statusIsInstalled)                               _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ (LensFor statusIsRenting)                                 _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ (LensFor statusIsReturning)                               _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ (LensFor statusTraffic)                                    _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ (LensFor statusVehicleDocksAvailable)                    _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType (LensFor vehicle_types_available_boost) _ _ _)   = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ (LensFor vehicle_types_available_iconic) _ _)  = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ _ (LensFor vehicle_types_available_efit)    _) = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ _ _ (LensFor vehicle_types_available_efit_g5)) = tableLenses

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
fromJSONToBeamStationStatus :: AT.StationStatus -> Maybe (StationStatusT (QExpr Postgres s))
fromJSONToBeamStationStatus status
  | Just lastReported <- status ^. AT.status_last_reported = Just $
  StationStatus { _statusStationId             = StationInformationId (fromIntegral $ status ^. AT.status_station_id)
                , _statusLastReported          = val_ (coerce lastReported)
                , _statusNumBikesAvailable     = fromIntegral $ status ^. AT.status_num_bikes_available
                , _statusNumBikesDisabled      = fromIntegral $ status ^. AT.status_num_bikes_disabled
                , _statusNumDocksAvailable     = fromIntegral $ status ^. AT.status_num_docks_available
                , _statusNumDocksDisabled      = fromIntegral $ status ^. AT.status_num_docks_disabled
                , _statusIsChargingStation     = val_ $ status ^. AT.status_is_charging_station
                , _statusStatus                = val_ (coerce $ status ^. AT.status_status :: BeamStationStatusString) , _statusIsInstalled = val_ $ status ^. AT.status_is_installed
                , _statusIsRenting             = val_ $ status ^. AT.status_is_renting
                , _statusIsReturning           = val_ $ status ^. AT.status_is_returning
                , _statusTraffic               = val_ $ fmap Text.pack $ status ^. AT.status_traffic
                , _statusVehicleDocksAvailable = fromIntegral $ AT.dock_count $ head $ status ^. AT.status_vehicle_docks_available
                , _statusVehicleTypesAvailable = val_ $ VehicleType num_boost num_iconic num_efit num_efit_g5
                }
  | otherwise = Nothing
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
  AT.StationStatus { AT._status_station_id              = fromIntegral sid
                   , AT._status_num_bikes_available     = fromIntegral $ status ^. statusNumBikesAvailable
                   , AT._status_num_bikes_disabled      = fromIntegral $ status ^. statusNumBikesDisabled
                   , AT._status_num_docks_available     = fromIntegral $ status ^. statusNumDocksAvailable
                   , AT._status_num_docks_disabled      = fromIntegral $ status ^. statusNumDocksDisabled
                   , AT._status_last_reported           = coerce (Just (status ^. statusLastReported))
                   , AT._status_is_charging_station     = status ^. statusIsChargingStation
                   , AT._status_status                  = coerce (status ^. statusStatus)
                   , AT._status_is_installed            = status ^. statusIsInstalled
                   , AT._status_is_renting              = status ^. statusIsRenting
                   , AT._status_is_returning            = status ^. statusIsReturning
                   , AT._status_traffic                 = fmap Text.unpack $ status ^. statusTraffic
                   , AT._status_vehicle_docks_available = [ AT.VehicleDock (map show [AT.Boost, AT.Iconic, AT.EFit, AT.EFitG5]) (fromIntegral $ status ^. statusVehicleDocksAvailable) ]
                   , AT._status_vehicle_types_available = [ AT.VehicleType AT.Boost  (fromIntegral (status ^. vehicle_types_available_boost))
                                                          , AT.VehicleType AT.Iconic (fromIntegral (status ^. vehicle_types_available_iconic))
                                                          , AT.VehicleType AT.EFit   (fromIntegral (status ^. vehicle_types_available_efit))
                                                          , AT.VehicleType AT.EFitG5 (fromIntegral (status ^. vehicle_types_available_efit_g5))
                                                          ]
                   }
  where
    StationInformationId sid = _statusStationId status
