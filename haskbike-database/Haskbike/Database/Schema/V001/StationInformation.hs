-- | Station infrormation table definition and functions.

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}


module Haskbike.Database.Schema.V001.StationInformation
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
     , unInformationReported
       -- Lenses
     , extraInfoMigrations
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
     , physicalConfigurationLabel
     , unInformationStationId
     ) where

import           Control.Lens                               hiding ( (.=) )

import           Data.Aeson
import qualified Data.ByteString.Char8                      as B
import           Data.Coerce                                ( coerce )
import           Data.Int
import           Data.Maybe
import           Data.String                                ( IsString )
import qualified Data.Text                                  as T
import qualified Data.Text.Lazy                             as TL
import           Data.Time
import           Data.Vector                                ( fromList, toList )
import qualified Data.Vector                                as Vector

import           Database.Beam
import           Database.Beam.Backend                      ( BeamBackend, HasSqlValueSyntax (sqlValueSyntax),
                                                              SqlSerial, timestampType )
import           Database.Beam.Migrate
import           Database.Beam.Postgres                     ( Postgres )
import qualified Database.Beam.Postgres                     as Pg
import           Database.Beam.Postgres.Syntax              ( pgTextType )
import           Database.PostgreSQL.Simple.FromField       ( Field (typeOid), FromField (..), ResultError (..),
                                                              returnError, typoid )
import           Database.PostgreSQL.Simple.ToField         ( ToField (..) )
import           Database.PostgreSQL.Simple.TypeInfo.Static ( text )

import qualified Haskbike.API.StationInformation            as AT
import           Haskbike.Database.CustomExpressions


-- | Declare a (Beam) table for the 'StationInformation' type.
data StationInformationT f where
  StationInformation :: { _infoId                    :: Columnar f (SqlSerial Int32)
                        , _infoStationId             :: Columnar f Int32
                        , _infoName                  :: Columnar f T.Text
                        , _infoPhysicalConfiguration :: Columnar f (Maybe BeamPhysicalConfiguration)
                        , _infoLat                   :: Columnar f Double
                        , _infoLon                   :: Columnar f Double
                        , _infoAltitude              :: Columnar f (Maybe Double)
                        , _infoAddress               :: Columnar f (Maybe T.Text)
                        , _infoCapacity              :: Columnar f Int32
                        , _infoIsChargingStation     :: Columnar f Bool
                        , _infoRentalMethods         :: Columnar f (Vector.Vector BeamRentalMethod)
                        , _infoIsValetStation        :: Columnar f Bool
                        , _infoIsVirtualStation      :: Columnar f Bool
                        , _infoGroups                :: Columnar f (Vector.Vector T.Text)
                        , _infoObcn                  :: Columnar f (Maybe T.Text)
                        , _infoNearbyDistance        :: Columnar f Double
                        , _infoBluetoothId           :: Columnar f (Maybe T.Text)
                        , _infoRideCodeSupport       :: Columnar f Bool
                        , _infoRentalUris            :: Columnar f (Vector.Vector T.Text)
                        , _infoActive                :: Columnar f Bool
                        , _infoReported              :: Columnar f UTCTime
                        } -> StationInformationT f
  deriving (Generic, Beamable)

instance ToJSON StationInformation where
  toJSON station =
    object [ "info_id"                .= _infoId                    station
           , "station_id"             .= _infoStationId             station
           , "name"                   .= _infoName                  station
           , "physical_configuration" .= _infoPhysicalConfiguration station
           , "lat"                    .= _infoLat                   station
           , "lon"                    .= _infoLon                   station
           , "altitude"               .= _infoAltitude              station
           , "address"                .= _infoAddress               station
           , "capacity"               .= _infoCapacity              station
           , "is_charging_station"    .= _infoIsChargingStation     station
           , "rental_methods"         .= _infoRentalMethods         station
           , "is_valet_station"       .= _infoIsValetStation        station
           , "is_virtual_station"     .= _infoIsVirtualStation      station
           , "groups"                 .= _infoGroups                station
           , "obcn"                   .= _infoObcn                  station
           , "nearby_distance"        .= _infoNearbyDistance        station
           , "_bluetooth_id"          .= _infoBluetoothId           station
           , "_ride_code_support"     .= _infoRideCodeSupport       station
           , "rental_uris"            .= _infoRentalUris            station
           , "active"                 .= _infoActive                station
           , "reported"               .= _infoReported              station
           ]

instance FromJSON StationInformation where
  parseJSON = withObject "StationInformation" $ \v -> StationInformation
    <$> v .: "info_id"
    <*> v .: "station_id"
    <*> v .:  "name"
    <*> v .:  "physical_configuration"
    <*> v .:  "lat"
    <*> v .:  "lon"
    <*> v .:  "altitude"
    <*> v .:? "address"
    <*> v .:  "capacity"
    <*> v .:  "is_charging_station"
    <*> v .:  "rental_methods"
    <*> v .:  "is_valet_station"
    <*> v .:  "is_virtual_station"
    <*> v .:  "groups"
    <*> v .:  "obcn"
    <*> v .:  "nearby_distance"
    <*> v .:  "_bluetooth_id"
    <*> v .:  "_ride_code_support"
    <*> v .:  "rental_uris"
    <*> v .:  "active"
    <*> v .:  "reported"


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
  data PrimaryKey StationInformationT f = StationInformationId { _unInformationStationId :: C f Int32
                                                               , _unInformationReported  :: C f UTCTime
                                                               }
    deriving (Generic, Beamable)
  primaryKey = StationInformationId <$> _infoStationId <*> _infoReported

-- | Lenses
unInformationReported :: Lens' (PrimaryKey StationInformationT f) (Columnar f UTCTime)
unInformationReported key (StationInformationId stationId lastReported) = fmap (StationInformationId stationId) (key lastReported)
{-# INLINE unInformationReported #-}

unInformationStationId :: Lens' (PrimaryKey StationInformationT f) (Columnar f Int32)
unInformationStationId key (StationInformationId stationId lastReported) = fmap (`StationInformationId` lastReported) (key stationId)
{-# INLINE unInformationStationId #-}


-- | StationInformation Lenses
infoId                      :: Lens' (StationInformationT f) (C f (SqlSerial Int32))
infoStationId               :: Lens' (StationInformationT f) (C f Int32)
infoName                    :: Lens' (StationInformationT f) (C f T.Text)
infoPhysicalConfiguration   :: Lens' (StationInformationT f) (C f (Maybe BeamPhysicalConfiguration))
infoLat                     :: Lens' (StationInformationT f) (C f Double)
infoLon                     :: Lens' (StationInformationT f) (C f Double)
infoAltitude                :: Lens' (StationInformationT f) (C f (Maybe Double))
infoAddress                 :: Lens' (StationInformationT f) (C f (Maybe T.Text))
infoCapacity                :: Lens' (StationInformationT f) (C f Int32)
infoIsChargingStation       :: Lens' (StationInformationT f) (C f Bool)
infoRentalMethods           :: Lens' (StationInformationT f) (C f (Vector.Vector BeamRentalMethod))
infoIsValetStation          :: Lens' (StationInformationT f) (C f Bool)
infoIsVirtualStation        :: Lens' (StationInformationT f) (C f Bool)
infoGroups                  :: Lens' (StationInformationT f) (C f (Vector.Vector T.Text))
infoObcn                    :: Lens' (StationInformationT f) (C f (Maybe T.Text))
infoNearbyDistance          :: Lens' (StationInformationT f) (C f Double)
infoBluetoothId             :: Lens' (StationInformationT f) (C f (Maybe T.Text))
infoRideCodeSupport         :: Lens' (StationInformationT f) (C f Bool)
infoRentalUris              :: Lens' (StationInformationT f) (C f (Vector.Vector T.Text))
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
  deriving (Eq, Generic, Show, Read, FromJSON, ToJSON) via AT.RentalMethod


instance (BeamBackend be, FromBackendRow be T.Text) => FromBackendRow be BeamRentalMethod where
  fromBackendRow = do
    val <- fromBackendRow
    case TL.fromStrict val of
      "KEY"         -> pure $ BeamRentalMethod AT.Key
      "TRANATTCARD" -> pure $ BeamRentalMethod AT.TransitCard
      "CREDITCARD"  -> pure $ BeamRentalMethod AT.CreditCard
      "PHONE"       -> pure $ BeamRentalMethod AT.Phone
      _             -> fail ("Invalid value for BeamRentalMethod: " ++ T.unpack val)

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
  deriving (Eq, Generic, Show, Read, Ord, FromJSON, ToJSON) via AT.PhysicalConfiguration


physicalConfigurationLabel :: Maybe BeamPhysicalConfiguration -> T.Text
physicalConfigurationLabel (Just (BeamPhysicalConfiguration AT.ElectricBikeStation)) = "Electric Bike Station"
physicalConfigurationLabel (Just (BeamPhysicalConfiguration AT.Regular))             = "Regular"
physicalConfigurationLabel (Just (BeamPhysicalConfiguration AT.RegularLitMapFrame))  = "Regular Lit Map Frame"
physicalConfigurationLabel (Just (BeamPhysicalConfiguration AT.SmartLitMapFrame))    = "Smart Lit Map Frame"
physicalConfigurationLabel (Just (BeamPhysicalConfiguration AT.SmartMapFrame))       = "Smart Map Frame"
physicalConfigurationLabel (Just (BeamPhysicalConfiguration AT.Vault))               = "Vault"
physicalConfigurationLabel Nothing                                                   = "None"

instance (BeamBackend be, FromBackendRow be T.Text) => FromBackendRow be BeamPhysicalConfiguration where
  fromBackendRow = do
    val <- {-# SCC "BeamPhysicalConfiguration-fromBackendRow" #-} fromBackendRow
    {-# SCC "BeamPhysicalConfiguration-case" #-} case TL.fromStrict val of
      "ELECTRICBIKESTATION" -> pure $ BeamPhysicalConfiguration AT.ElectricBikeStation
      "REGULAR"             -> pure $ BeamPhysicalConfiguration AT.Regular
      "REGULARLITMAPFRAME"  -> pure $ BeamPhysicalConfiguration AT.RegularLitMapFrame
      "SMARTLITMAPFRAME"    -> pure $ BeamPhysicalConfiguration AT.SmartLitMapFrame
      "SMARTMAPFRAME"       -> pure $ BeamPhysicalConfiguration AT.SmartMapFrame
      "VAULT"               -> pure $ BeamPhysicalConfiguration AT.Vault
      _                     -> fail ("Invalid value for BeamPhysicalConfiguration: " ++ T.unpack val)

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
                     , _infoName                  = val_ name
                     , _infoPhysicalConfiguration = val_ (coerce physical_configuration :: Maybe BeamPhysicalConfiguration)
                     , _infoLat                   = val_ lat
                     , _infoLon                   = val_ lon
                     , _infoAltitude              = val_ altitude
                     , _infoAddress               = val_ address
                     , _infoCapacity              = fromIntegral capacity
                     , _infoIsChargingStation     = val_ is_charging_station
                     , _infoRentalMethods         = val_ $ fromList $ fromMaybe [] (coerce rental_methods :: Maybe [BeamRentalMethod])
                     , _infoIsValetStation        = val_ is_valet_station
                     , _infoIsVirtualStation      = val_ is_virtual_station
                     , _infoGroups                = val_ $ fromList groups
                     , _infoObcn                  = val_ . Just $ obcn
                     , _infoNearbyDistance        = val_ nearby_distance
                     , _infoBluetoothId           = val_ bluetoothId
                     , _infoRideCodeSupport       = val_ ride_code_support
                     , _infoRentalUris            = val_ $ fromList [uriAndroid, uriIos, uriWeb]
                     , _infoActive                = val_ True
                     , _infoReported              = val_ reported
                     }
  where
    uriAndroid = AT.rentalUrisAndroid rental_uris
    uriIos     = AT.rentalUrisIos rental_uris
    uriWeb     = AT.rentalUrisWeb rental_uris
    bluetoothId = if not (T.null bluetooth_id)
                  then Just bluetooth_id
                  else Nothing

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
                        , AT.infoName                    = name
                        , AT.infoPhysicalConfiguration   = coerce physicalConfiguration' :: Maybe AT.PhysicalConfiguration
                        , AT.infoLat                     = lat
                        , AT.infoLon                     = lon
                        , AT.infoAltitude                = altitude
                        , AT.infoAddress                 = address
                        , AT.infoCapacity                = fromIntegral capacity
                        , AT.infoIsChargingStation       = isChargingStation
                        , AT.infoRentalMethods           = emptyListToNothing (coerce (toList rentalMethods) :: [AT.RentalMethod])
                        , AT.infoIsValetStation          = isValetStation
                        , AT.infoIsVirtualStation        = isVirtualStation
                        , AT.infoGroups                  = toList groups
                        , AT.infoObcn                    = fromMaybe "" obcn
                        , AT.infoNearbyDistance          = nearbyDistance
                        , AT.infoBluetoothId             = fromMaybe "" bluetoothId
                        , AT.infoRideCodeSupport         = rideCodeSupport
                        , AT.infoRentalUris              = AT.RentalURIs { AT.rentalUrisAndroid = fromMaybe "" (rentalUrisList ^? element 1)
                                                                         , AT.rentalUrisIos     = fromMaybe "" (rentalUrisList ^? element 2)
                                                                         , AT.rentalUrisWeb     = fromMaybe "" (rentalUrisList ^? element 3)
                                                                         }
                        }
  where
    rentalUrisList = toList rentalUris
    -- Convert an empty list to Nothing, otherwise Just the list.
    emptyListToNothing [] = Nothing
    emptyListToNothing xs = Just xs

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
  , _infoPhysicalConfiguration = field "physical_configuration" (maybeType physicalConfiguration)
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

extraInfoMigrations :: IsString a => [a]
extraInfoMigrations = ["ALTER TABLE public.station_status ADD CONSTRAINT fk_station_information FOREIGN KEY (info_station_id, info_reported) REFERENCES public.station_information (station_id, reported) ON UPDATE CASCADE"]
