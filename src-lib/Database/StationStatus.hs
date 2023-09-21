-- | Station infrormation table definition and functions.

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Database.StationStatus
        ( StationStatusT(..)
        , StationStatus
        , StationStatusId
        , PrimaryKey(StationStatusId)
        , BeamStationStatusString(..)
        , VehicleTypeMixin(..)
        , BeamReportTime(..)
        , reportTimeType
        , d_status_id
        , d_status_info_id
        , d_status_station_id
        , d_status_last_reported
        , d_status_num_bikes_available
        , d_status_num_bikes_disabled
        , d_status_num_docks_available
        , d_status_num_docks_disabled
        , d_status_active
        , available_boost
        , available_iconic
        , available_efit
        , available_efit_g5
        , stationStatusType
        , vehicleTypeFields
        , vehicleTypesAvailable
        , fromJSONToBeamStationStatus
        , fromBeamStationStatusToJSON
        ) where

import qualified API.Types                                  as API.T
import           Common
import           Database.StationInformation

import           Control.Lens
import qualified Data.ByteString.Char8                      as B
import           Data.Coerce                                (coerce)
import           Data.Int
import           Data.List                                  (find)
import           Data.String                                (IsString (fromString))
import qualified Data.Text                                  as Text
import           Data.Time
import           Data.Time.Clock.POSIX

import           Database.Beam
import           Database.Beam.Backend                      (BeamBackend,
                                                             HasSqlValueSyntax (sqlValueSyntax),
                                                             IsSql92DataTypeSyntax (timestampType),
                                                             SqlNull (SqlNull),
                                                             SqlSerial (..))
import           Database.Beam.Migrate                      (HasDefaultSqlDataType)
import           Database.Beam.Postgres                     (Postgres)
import           Database.Beam.Postgres.Syntax              (PgValueSyntax,
                                                             pgTextType)
import           Database.PostgreSQL.Simple.FromField       (Field (typeOid),
                                                             FromField (..),
                                                             ResultError (..),
                                                             returnError,
                                                             typoid)
import           Database.PostgreSQL.Simple.ToField         (ToField (..))
import           Database.PostgreSQL.Simple.TypeInfo.Static (text)


-- | Declare a (Beam) table for the 'StationStatus' type.
data StationStatusT f where
  StationStatus :: { _d_status_id                      :: Columnar f (SqlSerial Int32)
                   , _d_status_info_id                 :: PrimaryKey StationInformationT f
                   , _d_status_station_id              :: Columnar f Int32
                   , _d_status_num_bikes_available     :: Columnar f Int32
                   , _d_status_num_bikes_disabled      :: Columnar f Int32
                   , _d_status_num_docks_available     :: Columnar f Int32
                   , _d_status_num_docks_disabled      :: Columnar f Int32
                   , _d_status_last_reported           :: Columnar f (Maybe BeamReportTime)
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

VehicleType
  (LensFor available_boost)
  (LensFor available_iconic)
  (LensFor available_efit)
  (LensFor available_efit_g5)
  = tableLenses

-- | Lenses
StationStatus
  (LensFor d_status_id)
  (StationInformationId (LensFor d_status_info_id))
  (LensFor d_status_station_id)
  (LensFor d_status_num_bikes_available)
  (LensFor d_status_num_bikes_disabled)
  (LensFor d_status_num_docks_available)
  (LensFor d_status_num_docks_disabled)
  (LensFor d_status_last_reported)
  (LensFor d_status_is_charging_station)
  (LensFor d_status_status)
  (LensFor d_status_is_installed)
  (LensFor d_status_is_renting)
  (LensFor d_status_is_returning)
  (LensFor d_status_traffic)
  (LensFor d_status_vehicle_docks_available)
  (VehicleType
    (LensFor vehicle_types_available_boost)
    (LensFor vehicle_types_available_iconic)
    (LensFor vehicle_types_available_efit)
    (LensFor vehicle_types_available_efit_g5))
  (LensFor d_status_active)
  = tableLenses

-- | Newtype wrapper for StationStatusString to allow us to define a custom FromBackendRow instance.
-- Don't want to implement database-specific code for the underlying StationStatusString type.
newtype BeamStationStatusString where
  BeamStationStatusString :: API.T.StationStatusString -> BeamStationStatusString
  deriving (Eq, Generic, Show, Read) via API.T.StationStatusString

instance (BeamBackend be, FromBackendRow be Text.Text) => FromBackendRow be BeamStationStatusString where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: Text.Text of
      "IN_SERVICE"  -> pure $ BeamStationStatusString API.T.InService
      "END_OF_LIFE" -> pure $ BeamStationStatusString API.T.EndOfLife
      _ -> fail ("Invalid value for BeamStationStatusString: " ++ Text.unpack val)

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
fromJSONToBeamStationStatus (API.T.StationStatus
                             station_id
                             num_bikes_available
                             num_bikes_disabled
                             num_docks_available
                             num_docks_disabled
                             last_reported
                             is_charging_station
                             status
                             is_installed
                             is_renting
                             is_returning
                             traffic
                             vehicle_docks_available
                             vehicle_types_available
                            ) =
  StationStatus { _d_status_id                       = default_
                , _d_status_info_id                  = StationInformationId $ fromIntegral station_id
                , _d_status_station_id               = fromIntegral station_id
                , _d_status_num_bikes_available      = fromIntegral num_bikes_available
                , _d_status_num_bikes_disabled       = fromIntegral num_bikes_disabled
                , _d_status_num_docks_available      = fromIntegral num_docks_available
                , _d_status_num_docks_disabled       = fromIntegral num_docks_disabled
                , _d_status_last_reported            = val_ (coerce last_reported)
                , _d_status_is_charging_station      = val_ is_charging_station
                , _d_status_status                   = val_ (coerce status :: BeamStationStatusString)
                , _d_status_is_installed             = val_ is_installed
                , _d_status_is_renting               = val_ is_renting
                , _d_status_is_returning             = val_ is_returning
                , _d_status_traffic                  = val_ $ fmap Text.pack traffic
                , _d_status_vehicle_docks_available  = fromIntegral $ API.T.dock_count $ head vehicle_docks_available
                , _d_status_vehicle_types_available  = val_ $ VehicleType num_boost num_iconic num_efit num_efit_g5
                , _d_status_active                   = val_ True
                }
  where
    -- | Find the vehicle type in the list of vehicle types available; default to 0 if not found.
    findByType' vehicle_type = find (\x -> API.T.vehicle_type_id x == vehicle_type) vehicle_types_available
    findByType  vehicle_type = fromIntegral $ maybe 0 API.T.type_count (findByType' vehicle_type)
    num_boost   = findByType "BOOST"
    num_iconic  = findByType "ICONIC"
    num_efit    = findByType "EFIT"
    num_efit_g5 = findByType "EFIT G5"

-- | Convert from the Beam StationStatus type to the JSON StationStatus
fromBeamStationStatusToJSON :: StationInformation -> StationStatus -> API.T.StationStatus
fromBeamStationStatusToJSON _info status =
  API.T.StationStatus { API.T._status_station_id               = fromIntegral $ status^.d_status_station_id
                      , API.T._status_num_bikes_available      = fromIntegral $ status^.d_status_num_bikes_available
                      , API.T._status_num_bikes_disabled       = fromIntegral $ status^.d_status_num_bikes_disabled
                      , API.T._status_num_docks_available      = fromIntegral $ status^.d_status_num_docks_available
                      , API.T._status_num_docks_disabled       = fromIntegral $ status^.d_status_num_docks_disabled
                      , API.T._status_last_reported            = coerce $ status^.d_status_last_reported
                      , API.T._status_is_charging_station      = status^.d_status_is_charging_station
                      , API.T._status_status                   = coerce $ status^.d_status_status
                      , API.T._status_is_installed             = status^.d_status_is_installed
                      , API.T._status_is_renting               = status^.d_status_is_renting
                      , API.T._status_is_returning             = status^.d_status_is_returning
                      , API.T._status_traffic                  = fmap Text.unpack $ status^.d_status_traffic
                      , API.T._status_vehicle_docks_available  = [ API.T.VehicleDock ["FIXME"] (fromIntegral $ status^.d_status_vehicle_docks_available) ]
                      , API.T._status_vehicle_types_available  = [ API.T.VehicleType "" (fromIntegral (status^.vehicle_types_available_boost))
                                                                 , API.T.VehicleType "" (fromIntegral (status^.vehicle_types_available_iconic))
                                                                 , API.T.VehicleType "" (fromIntegral (status^.vehicle_types_available_efit))
                                                                 , API.T.VehicleType "" (fromIntegral (status^.vehicle_types_available_efit_g5))
                                                                 ]
                      }

-- | Newtype wrapper for the last_reported field, which is a POSIX timestamp in the JSON API.
newtype BeamReportTime where
  BeamReportTime :: LocalTime -> BeamReportTime
  deriving (Eq, Ord, Show, Read, FromField, ToField) via LocalTime
  deriving (HasSqlValueSyntax PgValueSyntax) via LocalTime
  deriving (FromBackendRow Postgres) via LocalTime
  deriving (HasSqlEqualityCheck Postgres) via LocalTime
  deriving (HasDefaultSqlDataType Postgres) via LocalTime

instance Num BeamReportTime where
    fromInteger i = BeamReportTime $ utcToLocalTime reportTimeZone . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral $ i
    abs           = error "BeamReportTime: abs not implemented"
    signum        = error "BeamReportTime: signum not implemented"
    negate        = error "BeamReportTime: negate not implemented"
    (+)           = error "BeamReportTime: (+) not implemented"
    (-)           = error "BeamReportTime: (-) not implemented"
    (*)           = error "BeamReportTime: (*) not implemented"

instance Real BeamReportTime where
  toRational (BeamReportTime t) = toRational . localToPosix $ t

instance Integral BeamReportTime => Enum BeamReportTime where
  toEnum = fromIntegral :: Int -> BeamReportTime
  fromEnum a = fromIntegral a :: Int

instance Enum BeamReportTime => Integral BeamReportTime where
  toInteger (BeamReportTime t) = fromIntegral $ localToPosix t
  quotRem a _ = (a, a)

instance (HasSqlValueSyntax BeamReportTime x, HasSqlValueSyntax BeamReportTime SqlNull) => HasSqlValueSyntax BeamReportTime (Maybe x) where
  sqlValueSyntax (Just x) = sqlValueSyntax x
  sqlValueSyntax Nothing  = sqlValueSyntax SqlNull

-- | Beam (migrate) datatype for the last_reported field in the StationStatus table.
reportTimeType :: DataType Postgres BeamReportTime
reportTimeType = DataType (timestampType Nothing True)
