-- | Station infrormation table definition and functions.

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}


module Database.BikeShare.StationInformation
     ( BeamPhysicalConfiguration (..)
     , BeamRentalMethod (..)
     , PrimaryKey (..)
     , StationInformation
     , StationInformationId
     , StationInformationT (..)
     , fromBeamStationInformationToJSON
     , fromJSONToBeamStationInformation
     , physicalConfiguration
     , rentalMethod
       -- Lenses
     , createStationInformation
     , infoActive
     , infoAddress
     , infoAltitude
     , infoBluetoothId
     , infoCapacity
     , infoGroups
     , infoIsChargingStation
     , infoIsValetStation
     , infoIsVirtualStation
     , infoLat
     , infoLon
     , infoName
     , infoNearbyDistance
     , infoObcn
     , infoPhysicalConfiguration
     , infoRentalMethods
     , infoRentalUris
     , infoRideCodeSupport
     , infoStationId
     , info_id
     , unInformationStationId
     ) where

import qualified API.Types                                  as AT

import           Control.Lens

import qualified Data.ByteString.Char8                      as B
import           Data.Coerce                                ( coerce )
import           Data.Int
import           Data.Maybe                                 ( fromMaybe )
import qualified Data.Text                                  as Text
import           Data.Vector                                ( fromList, toList )
import qualified Data.Vector                                as Vector

import           Database.Beam
import           Database.Beam.Backend                      ( BeamBackend, HasSqlValueSyntax (sqlValueSyntax),
                                                              SqlSerial )
import           Database.Beam.Migrate
import           Database.Beam.Postgres                     ( Postgres )
import qualified Database.Beam.Postgres                     as Pg
import           Database.Beam.Postgres.Syntax              ( pgTextType )
import           Database.PostgreSQL.Simple.FromField       ( Field (typeOid), FromField (..), ResultError (..),
                                                              returnError, typoid )
import           Database.PostgreSQL.Simple.ToField         ( ToField (..) )
import           Database.PostgreSQL.Simple.TypeInfo.Static ( text )


-- | Declare a (Beam) table for the 'StationInformation' type.
data StationInformationT f where
  StationInformation :: { _infoId                    :: Columnar f (SqlSerial Int32)
                        , _infoStationId             :: Columnar f Int32
                        , _infoName                  :: Columnar f Text.Text
                        , _infoPhysicalConfiguration :: Columnar f BeamPhysicalConfiguration
                        , _infoLat                   :: Columnar f Double
                        , _infoLon                   :: Columnar f Double
                        , _infoAltitude              :: Columnar f (Maybe Double)
                        , _infoAddress               :: Columnar f (Maybe Text.Text)
                        , _infoCapacity              :: Columnar f Int32
                        , _infoIsChargingStation     :: Columnar f Bool
                        , _infoRentalMethods         :: Columnar f (Vector.Vector BeamRentalMethod)
                        , _infoIsValetStation        :: Columnar f Bool
                        , _infoIsVirtualStation      :: Columnar f Bool
                        , _infoGroups                :: Columnar f (Vector.Vector Text.Text)
                        , _infoObcn                  :: Columnar f Text.Text
                        , _infoNearbyDistance        :: Columnar f Double
                        , _infoBluetoothId           :: Columnar f Text.Text
                        , _infoRideCodeSupport       :: Columnar f Bool
                        , _infoRentalUris            :: Columnar f (Vector.Vector Text.Text)
                        , _infoActive                :: Columnar f Bool
                        } -> StationInformationT f
  deriving (Generic, Beamable)

-- | Synonym for the table type.
type StationInformation = StationInformationT Identity
type StationInformationId = PrimaryKey StationInformationT Identity
deriving instance Show StationInformationId
deriving instance Eq StationInformationId
deriving instance Ord StationInformationId
deriving instance Show StationInformation
deriving instance Eq StationInformation

-- | Inform Beam about the table.
instance Table StationInformationT where
  data PrimaryKey StationInformationT f = StationInformationId { _unInformationStationId :: C f Int32 }
    deriving (Generic, Beamable)
  primaryKey = StationInformationId <$> _infoStationId

-- | Lenses (technically, Iso)
-- Lens' always works with part of a data structure (can set or view), while an Iso can swap between two different types bi-directionally.
unInformationStationId :: Iso (PrimaryKey StationInformationT f1) (PrimaryKey StationInformationT f2) (C f1 Int32) (C f2 Int32)
unInformationStationId = iso (\ (StationInformationId key) -> key) StationInformationId
{-# INLINE unInformationStationId #-}


-- | StationInformation Lenses
info_id                     :: Lens' (StationInformationT f) (C f (SqlSerial Int32))
infoStationId               :: Lens' (StationInformationT f) (C f Int32)
infoName                    :: Lens' (StationInformationT f) (C f Text.Text)
infoPhysicalConfiguration   :: Lens' (StationInformationT f) (C f BeamPhysicalConfiguration)
infoLat                     :: Lens' (StationInformationT f) (C f Double)
infoLon                     :: Lens' (StationInformationT f) (C f Double)
infoAltitude                :: Lens' (StationInformationT f) (C f (Maybe Double))
infoAddress                 :: Lens' (StationInformationT f) (C f (Maybe Text.Text))
infoCapacity                :: Lens' (StationInformationT f) (C f Int32)
infoIsChargingStation       :: Lens' (StationInformationT f) (C f Bool)
infoRentalMethods           :: Lens' (StationInformationT f) (C f (Vector.Vector BeamRentalMethod))
infoIsValetStation          :: Lens' (StationInformationT f) (C f Bool)
infoIsVirtualStation        :: Lens' (StationInformationT f) (C f Bool)
infoGroups                  :: Lens' (StationInformationT f) (C f (Vector.Vector Text.Text))
infoObcn                    :: Lens' (StationInformationT f) (C f Text.Text)
infoNearbyDistance          :: Lens' (StationInformationT f) (C f Double)
infoBluetoothId             :: Lens' (StationInformationT f) (C f Text.Text)
infoRideCodeSupport         :: Lens' (StationInformationT f) (C f Bool)
infoRentalUris              :: Lens' (StationInformationT f) (C f (Vector.Vector Text.Text))
infoActive                  :: Lens' (StationInformationT f) (C f Bool)

StationInformation (LensFor info_id)                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ (LensFor infoStationId)               _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ (LensFor infoName)                    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ (LensFor infoPhysicalConfiguration)   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ (LensFor infoLat)                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ (LensFor infoLon)                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ (LensFor infoAltitude)                _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ (LensFor infoAddress)                 _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ (LensFor infoCapacity)                _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ (LensFor infoIsChargingStation)       _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ (LensFor infoRentalMethods)           _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ (LensFor infoIsValetStation)          _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoIsVirtualStation)        _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoGroups)                  _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoObcn)                    _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoNearbyDistance)          _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoBluetoothId)             _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoRideCodeSupport)         _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoRentalUris)              _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoActive)                  = tableLenses

-- | Newtype wrapper for RentalMethod to allow us to define a custom FromBackendRow instance.
-- Don't want to implement database-specific code for the underlying RentalMethod type.
newtype BeamRentalMethod where
  BeamRentalMethod :: AT.RentalMethod -> BeamRentalMethod
  deriving (Eq, Generic, Show, Read) via AT.RentalMethod

instance (BeamBackend be, FromBackendRow be Text.Text) => FromBackendRow be BeamRentalMethod where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: Text.Text of
      "KEY"         -> pure $ BeamRentalMethod AT.Key
      "TRANATTCARD" -> pure $ BeamRentalMethod AT.TransitCard
      "CREDITCARD"  -> pure $ BeamRentalMethod AT.CreditCard
      "PHONE"       -> pure $ BeamRentalMethod AT.Phone
      _             -> fail ("Invalid value for BeamRentalMethod: " ++ Text.unpack val)

instance (HasSqlValueSyntax be String, Show BeamRentalMethod) => HasSqlValueSyntax be BeamRentalMethod where
  sqlValueSyntax = sqlValueSyntax . show

instance FromField BeamRentalMethod where
   fromField f mdata = if typeOid f /= typoid text -- TODO: any way to determine this automatically?
   then returnError Incompatible f ""
   else case B.unpack `fmap` mdata of
          Nothing  -> returnError UnexpectedNull f ""
          Just dat ->
             case [ x | (x,t) <- reads dat, ("","") <- lex t ] of
               [x] -> return x
               _   -> returnError ConversionFailed f dat

instance ToField BeamRentalMethod where
  toField = toField . show

rentalMethod :: DataType Pg.Postgres BeamRentalMethod
rentalMethod = DataType pgTextType

-- | Newtype wrapper for PhysicalConfiguration to allow us to define a custom FromBackendRow instance.
-- Don't want to implement database-specific code for the underlying PhysicalConfiguration type.
newtype BeamPhysicalConfiguration where
  BeamPhysicalConfiguration :: AT.PhysicalConfiguration -> BeamPhysicalConfiguration
  deriving (Eq, Generic, Show, Read) via AT.PhysicalConfiguration

instance (BeamBackend be, FromBackendRow be Text.Text) => FromBackendRow be BeamPhysicalConfiguration where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: Text.Text of
      "ELECTRICBIKESTATION" -> pure $ BeamPhysicalConfiguration AT.ElectricBikeStation
      "REGULAR"             -> pure $ BeamPhysicalConfiguration AT.Regular
      "REGULARLITMAPFRAME"  -> pure $ BeamPhysicalConfiguration AT.RegularLitMapFrame
      "SMARTLITMAPFRAME"    -> pure $ BeamPhysicalConfiguration AT.SmartLitMapFrame
      "SMARTMAPFRAME"       -> pure $ BeamPhysicalConfiguration AT.SmartMapFrame
      "VAULT"               -> pure $ BeamPhysicalConfiguration AT.Vault
      _                     -> fail ("Invalid value for BeamPhysicalConfiguration: " ++ Text.unpack val)

instance (HasSqlValueSyntax be String, Show BeamPhysicalConfiguration) => HasSqlValueSyntax be BeamPhysicalConfiguration where
  sqlValueSyntax = sqlValueSyntax . show

instance FromField BeamPhysicalConfiguration where
   fromField f mdata = if typeOid f /= typoid text -- TODO: any way to determine this automatically?
   then returnError Incompatible f ""
   else case B.unpack `fmap` mdata of
          Nothing  -> returnError UnexpectedNull f ""
          Just dat ->
             case [ x | (x,t) <- reads dat, ("","") <- lex t ] of
               [x] -> return x
               _   -> returnError ConversionFailed f dat

instance ToField BeamPhysicalConfiguration where
  toField = toField . show

physicalConfiguration :: DataType Postgres BeamPhysicalConfiguration
physicalConfiguration = DataType pgTextType


-- | Migration for the StationInformation table.
createStationInformation :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity StationInformationT))
createStationInformation =
  createTable "station_information" $ StationInformation
  { _infoId                    = field "id"                     Pg.serial notNull unique
  , _infoStationId             = field "station_id"             int notNull unique
  , _infoName                  = field "name"                   (varchar (Just 100)) notNull
  , _infoPhysicalConfiguration = field "physical_configuration" physicalConfiguration
  , _infoLat                   = field "lat"                    double notNull
  , _infoLon                   = field "lon"                    double notNull
  , _infoAltitude              = field "altitude"               (maybeType double)
  , _infoAddress               = field "address"                (maybeType (varchar (Just 100)))
  , _infoCapacity              = field "capacity"               int notNull
  , _infoIsChargingStation     = field "is_charging_station"    boolean notNull
  , _infoRentalMethods         = field "rental_methods"         (Pg.unboundedArray rentalMethod)
  , _infoIsValetStation        = field "is_valet_station"       boolean notNull
  , _infoIsVirtualStation      = field "is_virtual_station"     boolean notNull
  , _infoGroups                = field "groups"                 (Pg.unboundedArray (varchar (Just 100)))
  , _infoObcn                  = field "obcn"                   (varchar (Just 100)) notNull
  , _infoNearbyDistance        = field "nearby_distance"        double notNull
  , _infoBluetoothId           = field "bluetooth_id"           (varchar (Just 100)) notNull
  , _infoRideCodeSupport       = field "ride_code_support"      boolean notNull
  , _infoRentalUris            = field "rental_uris"            (Pg.unboundedArray (varchar (Just 100)))
  , _infoActive                = field "active"                 boolean notNull
  }


-- | Convert from the JSON StationInformation to the Beam StationInformation type
fromJSONToBeamStationInformation :: AT.StationInformation -> StationInformationT (QExpr Postgres s)
fromJSONToBeamStationInformation (AT.StationInformation
                                  station_id
                                  name
                                  physical_configuration
                                  lat
                                  lon
                                  altitude
                                  address
                                  capacity
                                  is_charging_station
                                  rental_methods
                                  is_valet_station
                                  is_virtual_station
                                  groups
                                  obcn
                                  nearby_distance
                                  bluetooth_id
                                  ride_code_support
                                  rental_uris
                                 ) =
  StationInformation { _infoId                    = default_
                     , _infoStationId             = fromIntegral station_id
                     , _infoName                  = val_ $ Text.pack name
                     , _infoPhysicalConfiguration = val_ (coerce physical_configuration :: BeamPhysicalConfiguration)
                     , _infoLat                   = val_ lat
                     , _infoLon                   = val_ lon
                     , _infoAltitude              = val_ altitude
                     , _infoAddress               = val_ $ Text.pack <$> address
                     , _infoCapacity              = fromIntegral capacity
                     , _infoIsChargingStation     = val_ is_charging_station
                     , _infoRentalMethods         = val_ $ fromList (coerce rental_methods :: [BeamRentalMethod])
                     , _infoIsValetStation        = val_ $ fromMaybe False is_valet_station
                     , _infoIsVirtualStation      = val_ is_virtual_station
                     , _infoGroups                = val_ $ fromList $ fmap Text.pack groups
                     , _infoObcn                  = val_ $ Text.pack obcn
                     , _infoNearbyDistance        = val_ nearby_distance
                     , _infoBluetoothId           = val_ $ Text.pack bluetooth_id
                     , _infoRideCodeSupport       = val_ ride_code_support
                     , _infoActive                = val_ True
                     , _infoRentalUris            = val_ $ fromList [uriAndroid, uriIos, uriWeb]
                     }
  where
    uriAndroid = Text.pack (AT.rentalUrisAndroid rental_uris)
    uriIos     = Text.pack (AT.rentalUrisIos rental_uris)
    uriWeb     = Text.pack (AT.rentalUrisWeb rental_uris)

-- | Convert from the Beam StationInformation type to the JSON StationInformation
fromBeamStationInformationToJSON :: StationInformation -> AT.StationInformation
fromBeamStationInformationToJSON (StationInformation
                                  _id
                                  stationId
                                  name
                                  physicalConfiguration'
                                  lat
                                  lon
                                  altitude
                                  address
                                  capacity
                                  isChargingStation
                                  rentalMethods
                                  isValetStation
                                  isVirtualStation
                                  groups
                                  obcn
                                  nearbyDistance
                                  bluetoothId
                                  rideCodeSupport
                                  rentalUris
                                  _active
                                 ) =
  AT.StationInformation { AT.infoStationId               = fromIntegral stationId
                        , AT.infoName                    = show name
                        , AT.infoPhysicalConfiguration   = coerce physicalConfiguration' :: AT.PhysicalConfiguration
                        , AT.infoLat                     = lat
                        , AT.infoLon                     = lon
                        , AT.infoAltitude                = altitude
                        , AT.infoAddress                 = Text.unpack <$> address
                        , AT.infoCapacity                = fromIntegral capacity
                        , AT.infoIsChargingStation       = isChargingStation
                        , AT.infoRentalMethods           = coerce (toList rentalMethods) :: [AT.RentalMethod]
                        , AT.infoIsValetStation          = Just isValetStation
                        , AT.infoIsVirtualStation        = isVirtualStation
                        , AT.infoGroups                  = Text.unpack <$> toList groups
                        , AT.infoObcn                    = Text.unpack obcn
                        , AT.infoNearbyDistance          = nearbyDistance
                        , AT.infoBluetoothId             = Text.unpack bluetoothId
                        , AT.infoRideCodeSupport         = rideCodeSupport
                        , AT.infoRentalUris              = AT.RentalURIs { AT.rentalUrisAndroid = maybe "" Text.unpack (rentalUrisList ^? element 1)
                                                                         , AT.rentalUrisIos     = maybe "" Text.unpack (rentalUrisList ^? element 2)
                                                                         , AT.rentalUrisWeb     = maybe "" Text.unpack (rentalUrisList ^? element 3)
                                                                         }
                        }
  where
    rentalUrisList = toList rentalUris
