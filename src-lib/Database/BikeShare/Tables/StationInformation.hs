-- | Station infrormation table definition and functions.

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}


module Database.BikeShare.Tables.StationInformation
     ( BeamPhysicalConfiguration (..)
     , BeamRentalMethod (..)
     , PrimaryKey (..)
     , StationInformation
     , StationInformationId
     , StationInformationT (..)
     , createStationInformation
     , fromBeamStationInformationToJSON
     , fromJSONToBeamStationInformation
     , physicalConfiguration
     , stationInformationModification
       -- Lenses
     , infoActive
     , infoAddress
     , infoAltitude
     , infoBluetoothId
     , infoCapacity
     , infoGroups
     , infoId
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
     , infoReported
     , infoRideCodeSupport
     , infoStationId
     , unInformationId
     ) where

import qualified API.StationInformation                     as AT

import           Control.Lens

import qualified Data.ByteString.Char8                      as B
import           Data.Coerce                                ( coerce )
import           Data.Int
import qualified Data.Text                                  as T
import qualified Data.Text                                  as Text
import           Data.Time
import           Data.Vector                                ( fromList, toList )
import qualified Data.Vector                                as Vector

import           Database.Beam
import           Database.Beam.Backend                      ( BeamBackend, HasSqlValueSyntax (sqlValueSyntax),
                                                              SqlSerial, timestampType )
import           Database.Beam.Migrate
import           Database.Beam.Postgres                     ( Postgres )
import qualified Database.Beam.Postgres                     as Pg
import           Database.Beam.Postgres.Syntax              ( PgExpressionSyntax (PgExpressionSyntax), emit,
                                                              pgTextType )
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
                        , _infoObcn                  :: Columnar f (Maybe Text.Text)
                        , _infoNearbyDistance        :: Columnar f Double
                        , _infoBluetoothId           :: Columnar f (Maybe Text.Text)
                        , _infoRideCodeSupport       :: Columnar f Bool
                        , _infoRentalUris            :: Columnar f (Vector.Vector Text.Text)
                        , _infoActive                :: Columnar f Bool
                        , _infoReported              :: Columnar f UTCTime
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
  data PrimaryKey StationInformationT f = StationInformationId { _unInformationId :: C f (SqlSerial Int32) }
    deriving (Generic, Beamable)
  primaryKey = StationInformationId <$> _infoId

-- | Lenses (technically, Iso)
-- Lens' always works with part of a data structure (can set or view), while an Iso can swap between two different types bi-directionally.
unInformationId :: Iso (PrimaryKey StationInformationT f1) (PrimaryKey StationInformationT f2) (C f1 (SqlSerial Int32)) (C f2 (SqlSerial Int32))
unInformationId = iso (\ (StationInformationId key) -> key) StationInformationId
{-# INLINE unInformationId #-}


-- | StationInformation Lenses
infoId                      :: Lens' (StationInformationT f) (C f (SqlSerial Int32))
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
infoObcn                    :: Lens' (StationInformationT f) (C f (Maybe Text.Text))
infoNearbyDistance          :: Lens' (StationInformationT f) (C f Double)
infoBluetoothId             :: Lens' (StationInformationT f) (C f (Maybe Text.Text))
infoRideCodeSupport         :: Lens' (StationInformationT f) (C f Bool)
infoRentalUris              :: Lens' (StationInformationT f) (C f (Vector.Vector Text.Text))
infoActive                  :: Lens' (StationInformationT f) (C f Bool)
infoReported                :: Lens' (StationInformationT f) (C f UTCTime)

StationInformation (LensFor infoId)                      _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ (LensFor infoStationId)               _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ (LensFor infoName)                    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ (LensFor infoPhysicalConfiguration)   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ (LensFor infoLat)                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ (LensFor infoLon)                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ (LensFor infoAltitude)                _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ (LensFor infoAddress)                 _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ (LensFor infoCapacity)                _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ (LensFor infoIsChargingStation)       _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ (LensFor infoRentalMethods)           _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ (LensFor infoIsValetStation)          _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoIsVirtualStation)        _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoGroups)                  _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoObcn)                    _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoNearbyDistance)          _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoBluetoothId)             _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoRideCodeSupport)         _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoRentalUris)              _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoActive)                  _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor infoReported)                = tableLenses

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
   fromField f mdata = do
     if typeOid f /= typoid text -- TODO: any way to determine this automatically?
        then returnError Incompatible f ""
        else case B.unpack `fmap` mdata of
          Nothing  -> returnError UnexpectedNull f ""
          Just dat ->
             case [ x | (x,t) <- reads dat, ("","") <- lex t ] of
               [x] -> return x
               _   -> returnError ConversionFailed f dat

instance ToField BeamRentalMethod where
  toField = toField . show

rentalMethodType :: DataType Postgres BeamRentalMethod
rentalMethodType = DataType pgTextType

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

-- | Convert from the JSON StationInformation to the Beam StationInformation type
fromJSONToBeamStationInformation :: UTCTime -> AT.StationInformation -> StationInformationT (QExpr Postgres s)
fromJSONToBeamStationInformation
  reported
  (AT.StationInformation
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
                     , _infoIsValetStation        = val_ is_valet_station
                     , _infoIsVirtualStation      = val_ is_virtual_station
                     , _infoGroups                = val_ $ fromList $ fmap Text.pack groups
                     , _infoObcn                  = val_ . Just . Text.pack $ obcn
                     , _infoNearbyDistance        = val_ nearby_distance
                     , _infoBluetoothId           = val_ bluetoothId
                     , _infoRideCodeSupport       = val_ ride_code_support
                     , _infoRentalUris            = val_ $ fromList [uriAndroid, uriIos, uriWeb]
                     , _infoActive                = val_ True
                     , _infoReported              = val_ reported
                     }
  where
    uriAndroid = Text.pack (AT.rentalUrisAndroid rental_uris)
    uriIos     = Text.pack (AT.rentalUrisIos rental_uris)
    uriWeb     = Text.pack (AT.rentalUrisWeb rental_uris)
    bluetoothId = if not (null bluetooth_id) then Just . Text.pack $ bluetooth_id else Nothing

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
                                  _reported
                                 ) =
  AT.StationInformation { AT.infoStationId               = fromIntegral stationId
                        , AT.infoName                    = T.unpack name
                        , AT.infoPhysicalConfiguration   = coerce physicalConfiguration' :: AT.PhysicalConfiguration
                        , AT.infoLat                     = lat
                        , AT.infoLon                     = lon
                        , AT.infoAltitude                = altitude
                        , AT.infoAddress                 = Text.unpack <$> address
                        , AT.infoCapacity                = fromIntegral capacity
                        , AT.infoIsChargingStation       = isChargingStation
                        , AT.infoRentalMethods           = coerce (toList rentalMethods) :: [AT.RentalMethod]
                        , AT.infoIsValetStation          = isValetStation
                        , AT.infoIsVirtualStation        = isVirtualStation
                        , AT.infoGroups                  = Text.unpack <$> toList groups
                        , AT.infoObcn                    = maybe "" Text.unpack obcn
                        , AT.infoNearbyDistance          = nearbyDistance
                        , AT.infoBluetoothId             = maybe "" Text.unpack bluetoothId
                        , AT.infoRideCodeSupport         = rideCodeSupport
                        , AT.infoRentalUris              = AT.RentalURIs { AT.rentalUrisAndroid = maybe "" Text.unpack (rentalUrisList ^? element 1)
                                                                         , AT.rentalUrisIos     = maybe "" Text.unpack (rentalUrisList ^? element 2)
                                                                         , AT.rentalUrisWeb     = maybe "" Text.unpack (rentalUrisList ^? element 3)
                                                                         }
                        }
  where
    rentalUrisList = toList rentalUris

-- * Table modifications and migrations.

-- | Table modifications for the 'StationInformation' table.
stationInformationModification :: EntityModification (DatabaseEntity be db) be (TableEntity StationInformationT)
stationInformationModification =
  setEntityName "station_information" <> modifyTableFields tableModification
  { _infoId                    = "id"
  , _infoStationId             = "station_id"
  , _infoName                  = "name"
  , _infoPhysicalConfiguration = "physical_configuration"
  , _infoLat                   = "lat"
  , _infoLon                   = "lon"
  , _infoAltitude              = "altitude"
  , _infoAddress               = "address"
  , _infoCapacity              = "capacity"
  , _infoIsChargingStation     = "is_charging_station"
  , _infoRentalMethods         = "rental_methods"
  , _infoIsValetStation        = "is_valet_station"
  , _infoIsVirtualStation      = "is_virtual_station"
  , _infoGroups                = "groups"
  , _infoObcn                  = "obcn"
  , _infoNearbyDistance        = "nearby_distance"
  , _infoBluetoothId           = "bluetooth_id"
  , _infoRideCodeSupport       = "ride_code_support"
  , _infoRentalUris            = "rental_uris"
  , _infoActive                = "active"
  }


-- | Migration for the StationInformation table.
createStationInformation :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity StationInformationT))
createStationInformation =
  createTable "station_information" $ StationInformation
  { _infoId                    = field "id"                     Pg.serial notNull unique
  , _infoStationId             = field "station_id"             int notNull
  , _infoName                  = field "name"                   Pg.text notNull
  , _infoPhysicalConfiguration = field "physical_configuration" physicalConfiguration notNull
  , _infoLat                   = field "lat"                    double notNull
  , _infoLon                   = field "lon"                    double notNull
  , _infoAltitude              = field "altitude"               (maybeType double)
  , _infoAddress               = field "address"                (maybeType Pg.text)
  , _infoCapacity              = field "capacity"               int notNull
  , _infoIsChargingStation     = field "is_charging_station"    boolean notNull
  , _infoRentalMethods         = field "rental_methods"         (Pg.unboundedArray rentalMethodType) notNull
  , _infoIsValetStation        = field "is_valet_station"       boolean notNull
  , _infoIsVirtualStation      = field "is_virtual_station"     boolean notNull
  , _infoGroups                = field "groups"                 (Pg.unboundedArray Pg.text) notNull
  , _infoObcn                  = field "obcn"                   (maybeType Pg.text)
  , _infoNearbyDistance        = field "nearby_distance"        double notNull
  , _infoBluetoothId           = field "bluetooth_id"           (maybeType Pg.text)
  , _infoRideCodeSupport       = field "ride_code_support"      boolean notNull
  , _infoRentalUris            = field "rental_uris"            (Pg.unboundedArray Pg.text) notNull
  , _infoActive                = field "active"                 boolean notNull
  , _infoReported              = field "reported"               (DataType (timestampType Nothing True)) (defaultTo_ currentTimestampUtc_) notNull
  }

-- | Postgres @NOW()@ function. Returns the server's timestamp in UTC.
nowUtc_ :: QExpr Postgres s UTCTime
nowUtc_ = QExpr (\_ -> PgExpressionSyntax (emit "NOW() at time zone 'UTC'"))

currentTimestampUtc_ :: QExpr Postgres s UTCTime
currentTimestampUtc_ = QExpr (\_ -> PgExpressionSyntax (emit "CURRENT_TIMESTAMP"))
