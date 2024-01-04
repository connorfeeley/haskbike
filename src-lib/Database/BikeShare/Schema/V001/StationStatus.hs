{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Station infrormation table definition and functions.

module Database.BikeShare.Schema.V001.StationStatus
     ( BeamStationStatusString (..)
     , PrimaryKey (..)
     , StationStatus
     , StationStatusId
     , StationStatusT (..)
     , VehicleTypeMixin (..)
     , availableBoost
     , availableEfit
     , availableEfitG5
     , availableIconic
     , createStationStatus
     , fromBeamStationStatusToJSON
     , fromJSONToBeamStationStatus
     , stationStatusModification
     , stationStatusType
     , statusInfoId
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
     , vehicleTypesAvailableBoost
     , vehicleTypesAvailableEfit
     , vehicleTypesAvailableEfitG5
     , vehicleTypesAvailableIconic
     ) where

import qualified API.StationStatus                                 as AT
import qualified API.VehicleType                                   as AT

import           Control.Lens

import qualified Data.ByteString.Char8                             as B
import           Data.Coerce                                       ( coerce )
import           Data.Int
import qualified Data.Map                                          as Map
import           Data.Maybe                                        ( listToMaybe )
import           Data.String                                       ( IsString (fromString) )
import qualified Data.Text                                         as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend                             ( BeamBackend, HasSqlValueSyntax (sqlValueSyntax),
                                                                     IsSql92DataTypeSyntax (..) )
import           Database.Beam.Migrate
import           Database.Beam.Postgres                            ( Postgres )
import qualified Database.Beam.Postgres                            as Pg
import           Database.Beam.Postgres.Syntax                     ( pgTextType )
import           Database.BikeShare.Schema.V001.StationInformation
import           Database.PostgreSQL.Simple.FromField              ( Field (typeOid), FromField (..), ResultError (..),
                                                                     returnError, typoid )
import           Database.PostgreSQL.Simple.ToField                ( ToField (..) )
import           Database.PostgreSQL.Simple.TypeInfo.Static        ( text )


-- | Declare a (Beam) table for the 'StationStatus' type.
data StationStatusT f where
  StationStatus :: { _statusInfoId                :: PrimaryKey StationInformationT f
                   , _statusStationId             :: Columnar f Int32
                   , _statusLastReported          :: Columnar f UTCTime
                   , _statusNumBikesAvailable     :: Columnar f Int32
                   , _statusNumBikesDisabled      :: Columnar f Int32
                   , _statusNumDocksAvailable     :: Columnar f Int32
                   , _statusNumDocksDisabled      :: Columnar f Int32
                   , _statusIsChargingStation     :: Columnar f Bool
                   , _statusStatus                :: Columnar f BeamStationStatusString
                   , _statusIsInstalled           :: Columnar f Bool
                   , _statusIsRenting             :: Columnar f Bool
                   , _statusIsReturning           :: Columnar f Bool
                   , _statusTraffic               :: Columnar f (Maybe T.Text) -- PBSC doesn't seem to set this field
                   , _statusVehicleDocksAvailable :: Columnar f Int32
                   , _statusVehicleTypesAvailable :: VehicleTypeMixin f
                   } -> StationStatusT f
  deriving (Generic, Beamable)

-- | Synonym for the table type.
type StationStatus   = StationStatusT Identity
type StationStatusId = PrimaryKey StationStatusT Identity
deriving instance Show StationStatusId
deriving instance Eq   StationStatusId
deriving instance Show StationStatus
deriving instance Eq   StationStatus

-- | Inform Beam about the table.
instance Table StationStatusT where
  data PrimaryKey StationStatusT f =
    StationStatusId { _unStatusStationId    :: PrimaryKey StationInformationT f
                    , _unStatusLastReported :: Columnar f UTCTime
                    }
    deriving (Generic, Beamable)
  primaryKey = StationStatusId <$> _statusInfoId  <*> _statusLastReported

-- | Lenses
unStatusLastReported :: Lens' (PrimaryKey StationStatusT f) (Columnar f UTCTime)
unStatusLastReported key (StationStatusId stationId lastReported) = fmap (StationStatusId stationId) (key lastReported)
{-# INLINE unStatusLastReported #-}

unStatusStationId :: Lens' (PrimaryKey StationStatusT f) (PrimaryKey StationInformationT f)
unStatusStationId key (StationStatusId stationId lastReported) = fmap (`StationStatusId` lastReported) (key stationId)
{-# INLINE unStatusStationId #-}

data VehicleTypeMixin f =
  VehicleType { _availableBoost  :: Columnar f Int32
              , _availableIconic :: Columnar f Int32
              , _availableEfit   :: Columnar f Int32
              , _availableEfitG5 :: Columnar f Int32
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
availableBoost   :: Lens' VehicleType Int32
availableIconic  :: Lens' VehicleType Int32
availableEfit    :: Lens' VehicleType Int32
availableEfitG5  :: Lens' VehicleType Int32

VehicleType (LensFor availableBoost)  _ _ _ = tableLenses
VehicleType _ (LensFor availableIconic) _ _ = tableLenses
VehicleType _ _ (LensFor availableEfit)   _ = tableLenses
VehicleType _ _ _ (LensFor availableEfitG5) = tableLenses

-- | StationStatus Lenses
statusInfoId                :: Getter (StationStatusT Identity) (PrimaryKey StationInformationT Identity)
statusStationId             :: Lens' (StationStatusT f) (C f Int32)
statusLastReported          :: Lens' (StationStatusT f) (C f UTCTime)
statusNumBikesAvailable     :: Lens' (StationStatusT f) (C f Int32)
statusNumBikesDisabled      :: Lens' (StationStatusT f) (C f Int32)
statusNumDocksAvailable     :: Lens' (StationStatusT f) (C f Int32)
statusNumDocksDisabled      :: Lens' (StationStatusT f) (C f Int32)
statusIsChargingStation     :: Lens' (StationStatusT f) (C f Bool)
statusStatus                :: Lens' (StationStatusT f) (C f BeamStationStatusString)
statusIsInstalled           :: Lens' (StationStatusT f) (C f Bool)
statusIsRenting             :: Lens' (StationStatusT f) (C f Bool)
statusIsReturning           :: Lens' (StationStatusT f) (C f Bool)
statusTraffic               :: Lens' (StationStatusT f) (C f (Maybe T.Text))
statusVehicleDocksAvailable :: Lens' (StationStatusT f) (C f Int32)
vehicleTypesAvailableBoost  :: Lens' (StationStatusT f) (C f Int32)
vehicleTypesAvailableIconic :: Lens' (StationStatusT f) (C f Int32)
vehicleTypesAvailableEfit   :: Lens' (StationStatusT f) (C f Int32)
vehicleTypesAvailableEfitG5 :: Lens' (StationStatusT f) (C f Int32)

statusInfoId = to _statusInfoId
StationStatus _ (LensFor statusStationId)                                  _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ (LensFor statusLastReported)                               _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ (LensFor statusNumBikesAvailable)                          _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ (LensFor statusNumBikesDisabled)                           _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ (LensFor statusNumDocksAvailable)                          _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ (LensFor statusNumDocksDisabled)                           _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ (LensFor statusIsChargingStation)                          _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ (LensFor statusStatus)                                     _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ (LensFor statusIsInstalled)                                _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ (LensFor statusIsRenting)                                  _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ (LensFor statusIsReturning)                                _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ (LensFor statusTraffic)                                    _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor statusVehicleDocksAvailable)                      _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType (LensFor vehicleTypesAvailableBoost) _ _ _)   = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ (LensFor vehicleTypesAvailableIconic) _ _)  = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ _ (LensFor vehicleTypesAvailableEfit)    _) = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ _ _ (LensFor vehicleTypesAvailableEfitG5))  = tableLenses

-- | Newtype wrapper for StationStatusString to allow us to define a custom FromBackendRow instance.
-- Don't want to implement database-specific code for the underlying StationStatusString type.
newtype BeamStationStatusString where
  BeamStationStatusString :: AT.StationStatusString -> BeamStationStatusString
  deriving (Eq, Generic, Show, Read) via AT.StationStatusString

instance (BeamBackend be, FromBackendRow be T.Text) => FromBackendRow be BeamStationStatusString where
  fromBackendRow = do
    val <- fromBackendRow
    -- TODO: tie this in with 'AT.StationStatusString' so that they can't get out of sync.
    case val :: T.Text of
      "IN_SERVICE"  -> pure $ BeamStationStatusString AT.InService
      "MAINTENANCE" -> pure $ BeamStationStatusString AT.Maintenance
      "PLANNED"     -> pure $ BeamStationStatusString AT.Planned
      "END_OF_LIFE" -> pure $ BeamStationStatusString AT.EndOfLife
      _             -> fail ("Invalid value for BeamStationStatusString: " ++ T.unpack val)

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
fromJSONToBeamStationStatus :: StationInformationId -> AT.StationStatus -> Maybe (StationStatusT (QExpr Postgres s))
fromJSONToBeamStationStatus infId status
  | Just lastReported <- status ^. AT.statusLastReported = Just $
  StationStatus { _statusInfoId                = val_ infId
                , _statusStationId             = val_ (fromIntegral $ status ^. AT.statusStationId)
                , _statusLastReported          = val_ (coerce lastReported)
                , _statusNumBikesAvailable     = fromIntegral $ status ^. AT.statusNumBikesAvailable
                , _statusNumBikesDisabled      = fromIntegral $ status ^. AT.statusNumBikesDisabled
                , _statusNumDocksAvailable     = fromIntegral $ status ^. AT.statusNumDocksAvailable
                , _statusNumDocksDisabled      = fromIntegral $ status ^. AT.statusNumDocksDisabled
                , _statusIsChargingStation     = val_ $ status ^. AT.statusIsChargingStation
                , _statusStatus                = val_ (coerce $ status ^. AT.statusStatus :: BeamStationStatusString)
                , _statusIsInstalled           = val_ $ status ^. AT.statusIsInstalled
                , _statusIsRenting             = val_ $ status ^. AT.statusIsRenting
                , _statusIsReturning           = val_ $ status ^. AT.statusIsReturning
                , _statusTraffic               = val_ $ fmap T.pack $ status ^. AT.statusTraffic
                , _statusVehicleDocksAvailable = maybe 0 (fromIntegral . AT.dock_count) $ listToMaybe $ status ^. AT.statusVehicleDocksAvailable
                , _statusVehicleTypesAvailable = val_ $ VehicleType num_boost num_iconic num_efit num_efit_g5
                }
  | otherwise = Nothing
  where
    -- | Find the vehicle type in the list of vehicle types available; default to 0 if not found.
    vta = AT._statusVehicleTypesAvailable status
    findByType vehicle_type = (maybe 0 fromIntegral . Map.lookup vehicle_type) vta
    num_boost   = findByType AT.Boost
    num_iconic  = findByType AT.Iconic
    num_efit    = findByType AT.EFit
    num_efit_g5 = findByType AT.EFitG5

-- | Convert from the Beam StationStatus type to the JSON StationStatus
fromBeamStationStatusToJSON :: StationStatus -> AT.StationStatus
fromBeamStationStatusToJSON status =
  AT.StationStatus { AT._statusStationId             = fromIntegral $ status ^. statusInfoId . unInformationStationId
                   , AT._statusNumBikesAvailable     = fromIntegral $ status ^. statusNumBikesAvailable
                   , AT._statusNumBikesDisabled      = fromIntegral $ status ^. statusNumBikesDisabled
                   , AT._statusNumDocksAvailable     = fromIntegral $ status ^. statusNumDocksAvailable
                   , AT._statusNumDocksDisabled      = fromIntegral $ status ^. statusNumDocksDisabled
                   , AT._statusLastReported          = coerce (Just  (status ^. statusLastReported))
                   , AT._statusIsChargingStation     = status ^. statusIsChargingStation
                   , AT._statusStatus                = coerce (status ^. statusStatus)
                   , AT._statusIsInstalled           = status ^. statusIsInstalled
                   , AT._statusIsRenting             = status ^. statusIsRenting
                   , AT._statusIsReturning           = status ^. statusIsReturning
                   , AT._statusTraffic               = fmap T.unpack $ status ^. statusTraffic
                   , AT._statusVehicleDocksAvailable = [ AT.VehicleDock (map show [AT.Boost, AT.Iconic, AT.EFit, AT.EFitG5]) (fromIntegral $ status ^. statusVehicleDocksAvailable) ]
                   , AT._statusVehicleTypesAvailable =
                     AT.listToMap [ AT.VehicleType AT.Boost  (fromIntegral (status ^. vehicleTypesAvailableBoost))
                                  , AT.VehicleType AT.Iconic (fromIntegral (status ^. vehicleTypesAvailableIconic))
                                  , AT.VehicleType AT.EFit   (fromIntegral (status ^. vehicleTypesAvailableEfit))
                                  , AT.VehicleType AT.EFitG5 (fromIntegral (status ^. vehicleTypesAvailableEfitG5))
                                  ]
                   }

-- * Table modifications and migrations.

-- | Table modifications for 'StationStatus' table.
stationStatusModification :: EntityModification (DatabaseEntity be db) be (TableEntity StationStatusT)
stationStatusModification =
  setEntityName "station_status" <> modifyTableFields tableModification
  { _statusInfoId                = StationInformationId "info_station_id" "info_reported"
  , _statusStationId             = "station_id"
  , _statusLastReported          = "last_reported"
  , _statusNumBikesAvailable     = "num_bikes_available"
  , _statusNumBikesDisabled      = "num_bikes_disabled"
  , _statusNumDocksAvailable     = "num_docks_available"
  , _statusNumDocksDisabled      = "num_docks_disabled"
  , _statusIsChargingStation     = "is_charging_station"
  , _statusStatus                = "status"
  , _statusIsInstalled           = "is_installed"
  , _statusIsRenting             = "is_renting"
  , _statusIsReturning           = "is_returning"
  , _statusTraffic               = "traffic"
  , _statusVehicleDocksAvailable = "vehicle_docks_available"
  , _statusVehicleTypesAvailable = vehicleTypeFields "vehicle_types_available"
  }

-- | Migration for the StationStatus table.
createStationStatus :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity StationStatusT))
createStationStatus =
  createTable "station_status" $ StationStatus
  { _statusInfoId                = StationInformationId
                                  (field "info_station_id" int notNull)
                                  (field "info_reported" (DataType (timestampType Nothing True)) notNull)
  , _statusStationId             = field "station_id"              int notNull
  , _statusLastReported          = field "last_reported"           (DataType (timestampType Nothing True)) notNull
  , _statusNumBikesAvailable     = field "num_bikes_available"     int notNull
  , _statusNumBikesDisabled      = field "num_bikes_disabled"      int notNull
  , _statusNumDocksAvailable     = field "num_docks_available"     int notNull
  , _statusNumDocksDisabled      = field "num_docks_disabled"      int notNull
  , _statusIsChargingStation     = field "is_charging_station"     boolean notNull
  , _statusStatus                = field "status"                  stationStatusType notNull
  , _statusIsInstalled           = field "is_installed"            boolean notNull
  , _statusIsRenting             = field "is_renting"              boolean notNull
  , _statusIsReturning           = field "is_returning"            boolean notNull
  , _statusTraffic               = field "traffic"                 (maybeType Pg.text)
  , _statusVehicleDocksAvailable = field "vehicle_docks_available" int notNull
  , _statusVehicleTypesAvailable = VehicleType (field "vehicle_types_available_boost"   int notNull)
                                               (field "vehicle_types_available_iconic"  int notNull)
                                               (field "vehicle_types_available_efit"    int notNull)
                                               (field "vehicle_types_available_efit_g5" int notNull)
  }

extraStatusMigrations :: IsString a => [a]
extraStatusMigrations = ["CREATE INDEX IF NOT EXISTS station_status_info_station_id_last_reported_idx ON station_status (info_station_id, last_reported);"]
