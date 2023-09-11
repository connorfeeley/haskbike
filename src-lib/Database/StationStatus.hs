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
        , stationStatus
        , vehicleTypeFields
        , vehicleTypesAvailable
        , fromJSONToBeamStationStatus
        , fromBeamStationStatusToJSON
        ) where

import           Control.Lens
import qualified StationStatus                              as SS

import qualified Data.ByteString.Char8                      as B
import           Data.Coerce                                (coerce)
import           Data.Int
import           Data.List                                  (find)
import           Data.String                                (IsString (fromString))
import qualified Data.Text                                  as Text

import           Database.Beam
import           Database.Beam.Backend                      (BeamBackend,
                                                             HasSqlValueSyntax (sqlValueSyntax),
                                                             SqlSerial)
import           Database.Beam.Postgres                     (Postgres)
import           Database.Beam.Postgres.Syntax              (pgTextType)
import           Database.PostgreSQL.Simple.FromField       (Field (typeOid),
                                                             FromField (..),
                                                             ResultError (..),
                                                             returnError,
                                                             typoid)
import           Database.PostgreSQL.Simple.ToField         (ToField (..))
import           Database.PostgreSQL.Simple.TypeInfo.Static (text)


-- | Declare a (Beam) table for the 'StationStatus' type.
data StationStatusT f where
  StationStatus :: { _status_id                               :: Columnar f (SqlSerial Int32)
                   , _status_station_id                       :: Columnar f Int32
                   , _status_num_bikes_available              :: Columnar f Int32
                   , _status_num_bikes_disabled               :: Columnar f Int32
                   , _status_num_docks_available              :: Columnar f Int32
                   , _status_num_docks_disabled               :: Columnar f Int32
                   , _status_last_reported                    :: Columnar f (Maybe Int32)
                   , _status_is_charging_station              :: Columnar f Bool
                   , _status_status                           :: Columnar f BeamStationStatusString
                   , _status_is_installed                     :: Columnar f Bool
                   , _status_is_renting                       :: Columnar f Bool
                   , _status_is_returning                     :: Columnar f Bool
                   , _status_traffic                          :: Columnar f (Maybe Text.Text) -- PBSC doesn't seem to set this field
                   , _status_vehicle_docks_available          :: Columnar f Int32
                   , _status_vehicle_types_available          :: VehicleTypeMixin f
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
  primaryKey = StationStatusId . _status_id

data VehicleTypeMixin f =
  VehicleType { _vehicle_types_available_boost   :: Columnar f Int32
              , _vehicle_types_available_iconic  :: Columnar f Int32
              , _vehicle_types_available_efit    :: Columnar f Int32
              , _vehicle_types_available_efit_g5 :: Columnar f Int32
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
  (LensFor vehicle_types_available_boost)
  (LensFor vehicle_types_available_iconic)
  (LensFor vehicle_types_available_efit)
  (LensFor vehicle_types_available_efit_g5)
  = tableLenses

-- | Lenses
StationStatus
  (LensFor status_id)
  (LensFor status_station_id)
  (LensFor status_num_bikes_available)
  (LensFor status_num_bikes_disabled)
  (LensFor status_num_docks_available)
  (LensFor status_num_docks_disabled)
  (LensFor status_last_reported)
  (LensFor status_is_charging_station)
  (LensFor status_status)
  (LensFor status_is_installed)
  (LensFor status_is_renting)
  (LensFor status_is_returning)
  (LensFor status_traffic)
  (LensFor status_vehicle_docks_available)
  (VehicleType
    (LensFor status_vehicle_types_available_boost)
    (LensFor status_vehicle_types_available_iconic)
    (LensFor status_vehicle_types_available_efit)
    (LensFor status_vehicle_types_available_efit_g5)
  ) = tableLenses

-- | Newtype wrapper for StationStatusString to allow us to define a custom FromBackendRow instance.
-- Don't want to implement database-specific code for the underlying StationStatusString type.
newtype BeamStationStatusString where
  BeamStationStatusString :: SS.StationStatusString -> BeamStationStatusString
  deriving (Eq, Generic, Show, Read) via SS.StationStatusString

instance (BeamBackend be, FromBackendRow be Text.Text) => FromBackendRow be BeamStationStatusString where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: Text.Text of
      "IN_SERVICE"  -> pure $ BeamStationStatusString SS.InService
      "END_OF_LIFE" -> pure $ BeamStationStatusString SS.EndOfLife
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

stationStatus :: DataType Postgres BeamStationStatusString
stationStatus = DataType pgTextType

-- | Convert from the JSON StationStatus to the Beam StationStatus type
fromJSONToBeamStationStatus (SS.StationStatus
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
  StationStatus { _status_id                        = default_
                , _status_station_id                = fromIntegral station_id
                , _status_num_bikes_available       = fromIntegral num_bikes_available
                , _status_num_bikes_disabled        = fromIntegral num_bikes_disabled
                , _status_num_docks_available       = fromIntegral num_docks_available
                , _status_num_docks_disabled        = fromIntegral num_docks_disabled
                , _status_last_reported             = val_ $ fmap fromIntegral last_reported
                , _status_is_charging_station       = val_ is_charging_station
                , _status_status                    = val_ (coerce status :: BeamStationStatusString)
                , _status_is_installed              = val_ is_installed
                , _status_is_renting                = val_ is_renting
                , _status_is_returning              = val_ is_returning
                , _status_traffic                   = val_ $ fmap Text.pack traffic
                , _status_vehicle_docks_available   = fromIntegral $ SS.dock_count $ head vehicle_docks_available
                , _status_vehicle_types_available   = val_ $ VehicleType num_boost num_iconic num_efit num_efit_g5
                }
  where
    -- | Find the vehicle type in the list of vehicle types available; default to 0 if not found.
    findByType' vehicle_type = find (\x -> SS.type_vehicle_type_id x == vehicle_type) vehicle_types_available
    findByType  vehicle_type = fromIntegral $ maybe 0 SS.type_count (findByType' vehicle_type)
    num_boost   = findByType "BOOST"
    num_iconic  = findByType "ICONIC"
    num_efit    = findByType "EFIT"
    num_efit_g5 = findByType "EFIT G5"

-- | Convert from the Beam StationStatus type to the JSON StationStatus
fromBeamStationStatusToJSON :: StationStatus -> SS.StationStatus
fromBeamStationStatusToJSON (StationStatus
                            _record_id
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
  SS.StationStatus { SS.status_station_id                 = fromIntegral station_id
                    , SS.status_num_bikes_available       = fromIntegral num_bikes_available
                    , SS.status_num_bikes_disabled        = fromIntegral num_bikes_disabled
                    , SS.status_num_docks_available       = fromIntegral num_docks_available
                    , SS.status_num_docks_disabled        = fromIntegral num_docks_disabled
                    , SS.status_last_reported             = fmap fromIntegral last_reported
                    , SS.status_is_charging_station       = is_charging_station
                    , SS.status_status                    = coerce status
                    , SS.status_is_installed              = is_installed
                    , SS.status_is_renting                = is_renting
                    , SS.status_is_returning              = is_returning
                    , SS.status_traffic                   = fmap Text.unpack traffic
                    , SS.status_vehicle_docks_available   = [SS.VehicleDock ["FIXME"] (fromIntegral vehicle_docks_available)]
                    , SS.status_vehicle_types_available   = [ SS.VehicleType "" (fromIntegral (vehicle_types_available ^. vehicle_types_available_boost))
                                                            , SS.VehicleType "" (fromIntegral (vehicle_types_available ^. vehicle_types_available_iconic))
                                                            , SS.VehicleType "" (fromIntegral (vehicle_types_available ^. vehicle_types_available_efit))
                                                            , SS.VehicleType "" (fromIntegral (vehicle_types_available ^. vehicle_types_available_efit_g5))
                                                            ]
                      }
