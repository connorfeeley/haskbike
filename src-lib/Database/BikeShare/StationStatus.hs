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
     , StationStatus
     , StationStatusId
     , StationStatusT (..)
     , VehicleTypeMixin (..)
     , availableBoost
     , availableEfit
     , availableEfitG5
     , availableIconic
     , fromBeamStationStatusToJSON
     , fromJSONToBeamStationStatus
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
     , vehicleTypesAvailableBoost
     , vehicleTypesAvailableEfit
     , vehicleTypesAvailableEfitG5
     , vehicleTypesAvailableIconic
     ) where

import qualified API.Types                                  as AT

import           Control.Lens

import qualified Data.ByteString.Char8                      as B
import           Data.Coerce                                ( coerce )
import           Data.Int
import           Data.List                                  ( find )
import           Data.Maybe                                 ( listToMaybe )
import           Data.String                                ( IsString (fromString) )
import qualified Data.Text                                  as Text
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend                      ( BeamBackend, HasSqlValueSyntax (sqlValueSyntax) )
import           Database.Beam.Postgres                     ( Postgres )
import           Database.Beam.Postgres.Syntax              ( pgTextType )
import           Database.BikeShare.StationInformation
import           Database.PostgreSQL.Simple.FromField       ( Field (typeOid), FromField (..), ResultError (..),
                                                              returnError, typoid )
import           Database.PostgreSQL.Simple.ToField         ( ToField (..) )
import           Database.PostgreSQL.Simple.TypeInfo.Static ( text )


-- | Declare a (Beam) table for the 'StationStatus' type.
data StationStatusT f where
  StationStatus :: { _statusStationId             :: PrimaryKey StationInformationT f
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
                   , _statusTraffic               :: Columnar f (Maybe Text.Text) -- PBSC doesn't seem to set this field
                   , _statusVehicleDocksAvailable :: Columnar f Int32
                   , _statusVehicleTypesAvailable :: VehicleTypeMixin f
                   } -> StationStatusT f
  deriving (Generic, Beamable)

-- | Synonym for the table type.
type StationStatus = StationStatusT Identity
type StationStatusId = PrimaryKey StationStatusT Identity
deriving instance Show StationStatusId
deriving instance Show StationStatus

-- | Inform Beam about the table.
instance Table StationStatusT where
  data PrimaryKey StationStatusT f =
    StationStatusId { _unStatusStationId    :: PrimaryKey StationInformationT f
                    , _unStatusLastReported :: Columnar f UTCTime
                    }
    deriving (Generic, Beamable)
  primaryKey = StationStatusId <$> _statusStationId  <*> _statusLastReported

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
statusStationId             :: Getter (StationStatusT Identity) (PrimaryKey StationInformationT Identity)
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
statusTraffic               :: Lens' (StationStatusT f) (C f (Maybe Text.Text))
statusVehicleDocksAvailable :: Lens' (StationStatusT f) (C f Int32)
vehicleTypesAvailableBoost  :: Lens' (StationStatusT f) (C f Int32)
vehicleTypesAvailableIconic :: Lens' (StationStatusT f) (C f Int32)
vehicleTypesAvailableEfit   :: Lens' (StationStatusT f) (C f Int32)
vehicleTypesAvailableEfitG5 :: Lens' (StationStatusT f) (C f Int32)

statusStationId = to _statusStationId
StationStatus _ (LensFor statusLastReported)                               _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ (LensFor statusNumBikesAvailable)                          _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ (LensFor statusNumBikesDisabled)                           _ _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ (LensFor statusNumDocksAvailable)                          _ _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ (LensFor statusNumDocksDisabled)                           _ _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ (LensFor statusIsChargingStation)                          _ _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ (LensFor statusStatus)                                     _ _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ (LensFor statusIsInstalled)                                _ _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ (LensFor statusIsRenting)                                  _ _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ (LensFor statusIsReturning)                                _ _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ (LensFor statusTraffic)                                    _ _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ (LensFor statusVehicleDocksAvailable)                      _ = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType (LensFor vehicleTypesAvailableBoost) _ _ _)   = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ (LensFor vehicleTypesAvailableIconic) _ _)  = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ _ (LensFor vehicleTypesAvailableEfit)    _) = tableLenses
StationStatus _ _ _ _ _ _ _ _ _ _ _ _ _ (VehicleType _ _ _ (LensFor vehicleTypesAvailableEfitG5))  = tableLenses

-- | Newtype wrapper for StationStatusString to allow us to define a custom FromBackendRow instance.
-- Don't want to implement database-specific code for the underlying StationStatusString type.
newtype BeamStationStatusString where
  BeamStationStatusString :: AT.StationStatusString -> BeamStationStatusString
  deriving (Eq, Generic, Show, Read) via AT.StationStatusString

instance (BeamBackend be, FromBackendRow be Text.Text) => FromBackendRow be BeamStationStatusString where
  fromBackendRow = do
    val <- fromBackendRow
    -- TODO: tie this in with 'AT.StationStatusString' so that they can't get out of sync.
    case val :: Text.Text of
      "IN_SERVICE"  -> pure $ BeamStationStatusString AT.InService
      "MAINTENANCE" -> pure $ BeamStationStatusString AT.Maintenance
      "PLANNED"     -> pure $ BeamStationStatusString AT.Planned
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
  | Just lastReported <- status ^. AT.statusLastReported = Just $
  StationStatus { _statusStationId             = StationInformationId (fromIntegral $ status ^. AT.statusStationId)
                , _statusLastReported          = val_ (coerce lastReported)
                , _statusNumBikesAvailable     = fromIntegral $ status ^. AT.statusNumBikesAvailable
                , _statusNumBikesDisabled      = fromIntegral $ status ^. AT.statusNumBikesDisabled
                , _statusNumDocksAvailable     = fromIntegral $ status ^. AT.statusNumDocksAvailable
                , _statusNumDocksDisabled      = fromIntegral $ status ^. AT.statusNumDocksDisabled
                , _statusIsChargingStation     = val_ $ status ^. AT.statusIsChargingStation
                , _statusStatus                = val_ (coerce $ status ^. AT.statusStatus :: BeamStationStatusString) , _statusIsInstalled = val_ $ status ^. AT.statusIsInstalled
                , _statusIsRenting             = val_ $ status ^. AT.statusIsRenting
                , _statusIsReturning           = val_ $ status ^. AT.statusIsReturning
                , _statusTraffic               = val_ $ fmap Text.pack $ status ^. AT.statusTraffic
                , _statusVehicleDocksAvailable = maybe 0 (fromIntegral . AT.dock_count) $ listToMaybe $ status ^. AT.statusVehicleDocksAvailable
                , _statusVehicleTypesAvailable = val_ $ VehicleType num_boost num_iconic num_efit num_efit_g5
                }
  | otherwise = Nothing
  where
    -- | Find the vehicle type in the list of vehicle types available; default to 0 if not found.
    findByType' vehicle_type = find (\x -> AT.vehicle_type_id x == vehicle_type) $ status ^. AT.statusVehicleTypesAvailable
    findByType  vehicle_type = fromIntegral $ maybe 0 AT.type_count (findByType' vehicle_type)
    num_boost   = findByType AT.Boost
    num_iconic  = findByType AT.Iconic
    num_efit    = findByType AT.EFit
    num_efit_g5 = findByType AT.EFitG5

-- | Convert from the Beam StationStatus type to the JSON StationStatus
fromBeamStationStatusToJSON :: StationStatus -> AT.StationStatus
fromBeamStationStatusToJSON status =
  AT.StationStatus { AT._statusStationId             = fromIntegral sid
                   , AT._statusNumBikesAvailable     = fromIntegral $ status ^. statusNumBikesAvailable
                   , AT._statusNumBikesDisabled      = fromIntegral $ status ^. statusNumBikesDisabled
                   , AT._statusNumDocksAvailable     = fromIntegral $ status ^. statusNumDocksAvailable
                   , AT._statusNumDocksDisabled      = fromIntegral $ status ^. statusNumDocksDisabled
                   , AT._statusLastReported          = coerce (Just (status ^. statusLastReported))
                   , AT._statusIsChargingStation     = status ^. statusIsChargingStation
                   , AT._statusStatus                = coerce (status ^. statusStatus)
                   , AT._statusIsInstalled           = status ^. statusIsInstalled
                   , AT._statusIsRenting             = status ^. statusIsRenting
                   , AT._statusIsReturning           = status ^. statusIsReturning
                   , AT._statusTraffic               = fmap Text.unpack $ status ^. statusTraffic
                   , AT._statusVehicleDocksAvailable = [ AT.VehicleDock (map show [AT.Boost, AT.Iconic, AT.EFit, AT.EFitG5]) (fromIntegral $ status ^. statusVehicleDocksAvailable) ]
                   , AT._statusVehicleTypesAvailable = [ AT.VehicleType AT.Boost  (fromIntegral (status ^. vehicleTypesAvailableBoost))
                                                       , AT.VehicleType AT.Iconic (fromIntegral (status ^. vehicleTypesAvailableIconic))
                                                       , AT.VehicleType AT.EFit   (fromIntegral (status ^. vehicleTypesAvailableEfit))
                                                       , AT.VehicleType AT.EFitG5 (fromIntegral (status ^. vehicleTypesAvailableEfitG5))
                                                       ]
                   }
  where
    StationInformationId sid = _statusStationId status
