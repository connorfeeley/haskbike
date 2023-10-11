-- | Station infrormation table definition and functions.

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{-# LANGUAGE AllowAmbiguousTypes       #-}
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


module Database.BikeShare.StationInformation
     ( BeamPhysicalConfiguration (..)
     , BeamRentalMethod (..)
     , PrimaryKey (StationInformationId)
     , StationInformation
     , StationInformationId
     , StationInformationT (..)
     , fromBeamStationInformationToJSON
     , fromJSONToBeamStationInformation
     , physicalConfiguration
     , rentalMethod
       -- Lenses
     , info_active
     , info_address
     , info_altitude
     , info_bluetooth_id
     , info_capacity
     , info_groups
     , info_id
     , info_is_charging_station
     , info_is_valet_station
     , info_is_virtual_station
     , info_lat
     , info_lon
     , info_name
     , info_nearby_distance
     , info_obcn
     , info_physical_configuration
     , info_rental_methods
     , info_rental_uris
     , info_ride_code_support
     , info_station_id
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
import           Database.Beam.Postgres                     ( Postgres )
import           Database.Beam.Postgres.Syntax              ( pgTextType )
import           Database.PostgreSQL.Simple.FromField       ( Field (typeOid), FromField (..), ResultError (..),
                                                              returnError, typoid )
import           Database.PostgreSQL.Simple.ToField         ( ToField (..) )
import           Database.PostgreSQL.Simple.TypeInfo.Static ( text )


-- | Declare a (Beam) table for the 'StationInformation' type.
data StationInformationT f where
  StationInformation :: { _info_id                        :: Columnar f (SqlSerial Int32)
                        , _info_station_id                :: Columnar f Int32
                        , _info_name                      :: Columnar f Text.Text
                        , _info_physical_configuration    :: Columnar f BeamPhysicalConfiguration
                        , _info_lat                       :: Columnar f Double
                        , _info_lon                       :: Columnar f Double
                        , _info_altitude                  :: Columnar f (Maybe Double)
                        , _info_address                   :: Columnar f Text.Text
                        , _info_capacity                  :: Columnar f Int32
                        , _info_is_charging_station       :: Columnar f Bool
                        , _info_rental_methods            :: Columnar f (Vector.Vector BeamRentalMethod)
                        , _info_is_valet_station          :: Columnar f Bool
                        , _info_is_virtual_station        :: Columnar f Bool
                        , _info_groups                    :: Columnar f (Vector.Vector Text.Text)
                        , _info_obcn                      :: Columnar f Text.Text
                        , _info_nearby_distance           :: Columnar f Double
                        , _info_bluetooth_id              :: Columnar f Text.Text
                        , _info_ride_code_support         :: Columnar f Bool
                        , _info_rental_uris               :: Columnar f (Vector.Vector Text.Text)
                        , _info_active                    :: Columnar f Bool
                        } -> StationInformationT f
  deriving (Generic, Beamable)

-- | Synonym for the table type.
type StationInformation = StationInformationT Identity
type StationInformationId = PrimaryKey StationInformationT Identity
deriving instance Show StationInformationId
deriving instance Eq StationInformationId
deriving instance Show StationInformation
deriving instance Eq StationInformation

-- | Inform Beam about the table.
instance Table StationInformationT where
  data PrimaryKey StationInformationT f = StationInformationId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = StationInformationId . _info_station_id

-- | StationInformation Lenses
info_id                     :: Lens' (StationInformationT f) (C f (SqlSerial Int32))
info_station_id             :: Lens' (StationInformationT f) (C f Int32)
info_name                   :: Lens' (StationInformationT f) (C f Text.Text)
info_physical_configuration :: Lens' (StationInformationT f) (C f BeamPhysicalConfiguration)
info_lat                    :: Lens' (StationInformationT f) (C f Double)
info_lon                    :: Lens' (StationInformationT f) (C f Double)
info_altitude               :: Lens' (StationInformationT f) (C f (Maybe Double))
info_address                :: Lens' (StationInformationT f) (C f Text.Text)
info_capacity               :: Lens' (StationInformationT f) (C f Int32)
info_is_charging_station    :: Lens' (StationInformationT f) (C f Bool)
info_rental_methods         :: Lens' (StationInformationT f) (C f (Vector.Vector BeamRentalMethod))
info_is_valet_station       :: Lens' (StationInformationT f) (C f Bool)
info_is_virtual_station     :: Lens' (StationInformationT f) (C f Bool)
info_groups                 :: Lens' (StationInformationT f) (C f (Vector.Vector Text.Text))
info_obcn                   :: Lens' (StationInformationT f) (C f Text.Text)
info_nearby_distance        :: Lens' (StationInformationT f) (C f Double)
info_bluetooth_id           :: Lens' (StationInformationT f) (C f Text.Text)
info_ride_code_support      :: Lens' (StationInformationT f) (C f Bool)
info_rental_uris            :: Lens' (StationInformationT f) (C f (Vector.Vector Text.Text))
info_active                 :: Lens' (StationInformationT f) (C f Bool)

StationInformation (LensFor info_id)                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ (LensFor info_station_id)             _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ (LensFor info_name)                   _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ (LensFor info_physical_configuration) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ (LensFor info_lat)                    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ (LensFor info_lon)                    _ _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ (LensFor info_altitude)               _ _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ (LensFor info_address)                _ _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ (LensFor info_capacity)               _ _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ (LensFor info_is_charging_station)    _ _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ (LensFor info_rental_methods)         _ _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ (LensFor info_is_valet_station)       _ _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ (LensFor info_is_virtual_station)     _ _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor info_groups)                 _ _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor info_obcn)                   _ _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor info_nearby_distance)        _ _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor info_bluetooth_id)           _ _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor info_ride_code_support)      _ _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor info_rental_uris)            _ = tableLenses
StationInformation _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (LensFor info_active)                 = tableLenses

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

rentalMethod :: DataType Postgres BeamRentalMethod
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
  StationInformation { _info_id                        = default_
                     , _info_station_id                = fromIntegral station_id
                     , _info_name                      = val_ $ Text.pack name
                     , _info_physical_configuration    = val_ (coerce physical_configuration :: BeamPhysicalConfiguration)
                     , _info_lat                       = val_ lat
                     , _info_lon                       = val_ lon
                     , _info_altitude                  = val_ altitude
                     , _info_address                   = val_ $ Text.pack address
                     , _info_capacity                  = fromIntegral capacity
                     , _info_is_charging_station       = val_ is_charging_station
                     , _info_rental_methods            = val_ $ fromList (coerce rental_methods :: [BeamRentalMethod])
                     , _info_is_valet_station          = val_ $ fromMaybe False is_valet_station
                     , _info_is_virtual_station        = val_ is_virtual_station
                     , _info_groups                    = val_ $ fromList $ fmap Text.pack groups
                     , _info_obcn                      = val_ $ Text.pack obcn
                     , _info_nearby_distance           = val_ nearby_distance
                     , _info_bluetooth_id              = val_ $ Text.pack bluetooth_id
                     , _info_ride_code_support         = val_ ride_code_support
                     , _info_active                    = val_ True
                     , _info_rental_uris               = val_ $ fromList [uriAndroid, uriIos, uriWeb]
                     }
  where
    uriAndroid = Text.pack $ AT.rental_uris_android rental_uris
    uriIos     = Text.pack $ AT.rental_uris_ios rental_uris
    uriWeb     = Text.pack $ AT.rental_uris_web rental_uris

-- | Convert from the Beam StationInformation type to the JSON StationInformation
fromBeamStationInformationToJSON :: StationInformation -> AT.StationInformation
fromBeamStationInformationToJSON (StationInformation
                                  _id
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
                                  _active
                                 ) =
  AT.StationInformation { AT.info_station_id                = fromIntegral station_id
                        , AT.info_name                      = show name
                        , AT.info_physical_configuration    = coerce physical_configuration :: AT.PhysicalConfiguration
                        , AT.info_lat                       = lat
                        , AT.info_lon                       = lon
                        , AT.info_altitude                  = altitude
                        , AT.info_address                   = Text.unpack address
                        , AT.info_capacity                  = fromIntegral capacity
                        , AT.info_is_charging_station       = is_charging_station
                        , AT.info_rental_methods            = coerce (toList rental_methods) :: [AT.RentalMethod]
                        , AT.info_is_valet_station          = Just is_valet_station
                        , AT.info_is_virtual_station        = is_virtual_station
                        , AT.info_groups                    = Text.unpack <$> toList groups
                        , AT.info_obcn                      = Text.unpack obcn
                        , AT.info_nearby_distance           = nearby_distance
                        , AT.info_bluetooth_id              = Text.unpack bluetooth_id
                        , AT.info_ride_code_support         = ride_code_support
                        , AT.info_rental_uris               = AT.RentalURIs { AT.rental_uris_android = maybe "" Text.unpack (rentalUris ^? element 1)
                                                                            , AT.rental_uris_ios     = maybe "" Text.unpack (rentalUris ^? element 2)
                                                                            , AT.rental_uris_web     = maybe "" Text.unpack (rentalUris ^? element 3)
                                                                            }
                        }
  where
    rentalUris = toList rental_uris
