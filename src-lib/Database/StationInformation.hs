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

module Database.StationInformation
        ( StationInformationT(..)
        , StationInformation
        , StationInformationId
        , PrimaryKey(StationInformationId)
        , BeamRentalMethod(..)
        , rentalMethod
        , station_id
        , name
        , BeamPhysicalConfiguration(..)
        , physicalConfiguration
        , fromJSONToBeamStationInformation
        , fromBeamStationInformationToJSON
        ) where

import           Control.Lens
import qualified StationInformation                         as SI

import           Database.Beam

import           Data.Int
import qualified Data.Text                                  as Text
import           Data.Vector                                (fromList, toList)
import qualified Data.Vector                                as Vector

import qualified Data.ByteString.Char8                      as B
import           Data.Coerce                                (coerce)
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


-- | Declare a (Beam) table for the 'StationInformation' type.
data StationInformationT f where
  StationInformation :: { _id                        :: Columnar f (SqlSerial Int32)
                        , _station_id                :: Columnar f Int32
                        , _name                      :: Columnar f Text.Text
                        , _physical_configuration    :: Columnar f BeamPhysicalConfiguration
                        , _lat                       :: Columnar f Double
                        , _lon                       :: Columnar f Double
                        , _altitude                  :: Columnar f Double
                        , _address                   :: Columnar f Text.Text
                        , _capacity                  :: Columnar f Int32
                        , _is_charging_station       :: Columnar f Bool
                        , _rental_methods            :: Columnar f (Vector.Vector BeamRentalMethod)
                        , _is_virtual_station        :: Columnar f Bool
                        , _groups                    :: Columnar f (Vector.Vector Text.Text)
                        , _obcn                      :: Columnar f Text.Text
                        , _nearby_distance           :: Columnar f Double
                        , _bluetooth_id              :: Columnar f Text.Text
                        , _ride_code_support         :: Columnar f Bool
                        -- , _rental_uris               :: Columnar f SI.RentalURIs
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
  primaryKey = StationInformationId . _station_id

-- | Lenses
StationInformation
  (LensFor id)
  (LensFor station_id)
  (LensFor name)
  (LensFor physical_configuration)
  (LensFor lat)
  (LensFor lon)
  (LensFor altitude)
  (LensFor address)
  (LensFor capacity)
  (LensFor is_charging_station)
  (LensFor rental_methods)
  (LensFor is_virtual_station)
  (LensFor groups)
  (LensFor obcn)
  (LensFor nearby_distance)
  (LensFor bluetooth_id)
  (LensFor ride_code_support)
  -- (LensFor rental_uris)
  = tableLenses

-- | Newtype wrapper for RentalMethod to allow us to define a custom FromBackendRow instance.
-- Don't want to implement database-specific code for the underlying RentalMethod type.
newtype BeamRentalMethod where
  BeamRentalMethod :: SI.RentalMethod -> BeamRentalMethod
  deriving (Eq, Generic, Show, Read) via SI.RentalMethod

instance (BeamBackend be, FromBackendRow be Text.Text) => FromBackendRow be BeamRentalMethod where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: Text.Text of
      "KEY"           -> pure $ BeamRentalMethod SI.Key
      "TRANSITCARD"   -> pure $ BeamRentalMethod SI.TransitCard
      "CREDITCARD"    -> pure $ BeamRentalMethod SI.CreditCard
      "PHONE"         -> pure $ BeamRentalMethod SI.Phone
      _ -> fail ("Invalid value for BeamRentalMethod: " ++ Text.unpack val)

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

rentalMethod :: DataType Postgres BeamRentalMethod
rentalMethod = DataType pgTextType

-- | Newtype wrapper for PhysicalConfiguration to allow us to define a custom FromBackendRow instance.
-- Don't want to implement database-specific code for the underlying PhysicalConfiguration type.
newtype BeamPhysicalConfiguration where
  BeamPhysicalConfiguration :: SI.PhysicalConfiguration -> BeamPhysicalConfiguration
  deriving (Eq, Generic, Show, Read) via SI.PhysicalConfiguration

instance (BeamBackend be, FromBackendRow be Text.Text) => FromBackendRow be BeamPhysicalConfiguration where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: Text.Text of
      "ELECTRICBIKESTATION" -> pure $ BeamPhysicalConfiguration SI.ElectricBikeStation
      "REGULAR"             -> pure $ BeamPhysicalConfiguration SI.Regular
      "REGULARLITMAPFRAME"  -> pure $ BeamPhysicalConfiguration SI.RegularLitMapFrame
      "SMARTLITMAPFRAME"    -> pure $ BeamPhysicalConfiguration SI.SmartLitMapFrame
      "SMARTMAPFRAME"       -> pure $ BeamPhysicalConfiguration SI.SmartMapFrame
      "VAULT"               -> pure $ BeamPhysicalConfiguration SI.Vault
      _ -> fail ("Invalid value for BeamPhysicalConfiguration: " ++ Text.unpack val)

instance (HasSqlValueSyntax be String, Show BeamPhysicalConfiguration) => HasSqlValueSyntax be BeamPhysicalConfiguration where
  sqlValueSyntax = sqlValueSyntax . show

instance FromField BeamPhysicalConfiguration where
   fromField f mdata = do
     if typeOid f /= typoid text -- TODO: any way to determine this automatically?
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
fromJSONToBeamStationInformation (SI.StationInformation
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
                                  is_virtual_station
                                  groups
                                  obcn
                                  nearby_distance
                                  bluetooth_id
                                  ride_code_support
                                  -- rental_uris
                                 ) =
  StationInformation { _id                        = default_
                     , _station_id                = fromIntegral station_id
                     , _name                      = val_ $ Text.pack name
                     , _physical_configuration    = val_ (coerce physical_configuration :: BeamPhysicalConfiguration)
                     , _lat                       = val_ lat
                     , _lon                       = val_ lon
                     , _altitude                  = val_ altitude
                     , _address                   = val_ $ Text.pack address
                     , _capacity                  = fromIntegral capacity
                     , _is_charging_station       = val_ is_charging_station
                     , _rental_methods            = val_ $ fromList (coerce rental_methods :: [BeamRentalMethod])
                     , _is_virtual_station        = val_ is_virtual_station
                     , _groups                    = val_ $ fromList $ fmap Text.pack groups
                     , _obcn                      = val_ $ Text.pack obcn
                     , _nearby_distance           = val_ nearby_distance
                     , _bluetooth_id              = val_ $ Text.pack bluetooth_id
                     , _ride_code_support         = val_ ride_code_support
                     -- , _rental_uris               = val_ ""
                     }

-- | Convert from the Beam StationInformation type to the JSON StationInformation
fromBeamStationInformationToJSON :: StationInformation -> SI.StationInformation
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
                                  is_virtual_station
                                  groups
                                  obcn
                                  nearby_distance
                                  bluetooth_id
                                  ride_code_support
                                  -- rental_uris
                                 ) =
  SI.StationInformation { SI.station_id                = fromIntegral station_id
                        , SI.name                      = show name
                        , SI.physical_configuration    = coerce physical_configuration :: SI.PhysicalConfiguration
                        , SI.lat                       = lat
                        , SI.lon                       = lon
                        , SI.altitude                  = altitude
                        , SI.address                   = Text.unpack address
                        , SI.capacity                  = fromIntegral capacity
                        , SI.is_charging_station       = is_charging_station
                        , SI.rental_methods            = coerce (toList rental_methods) :: [SI.RentalMethod]
                        , SI.is_virtual_station        = is_virtual_station
                        , SI.groups                    = Text.unpack <$> toList groups
                        , SI.obcn                      = Text.unpack obcn
                        , SI.nearby_distance           = nearby_distance
                        , SI.bluetooth_id              = Text.unpack bluetooth_id
                        , SI.ride_code_support         = ride_code_support
                        -- , SI.rental_uris               = SI.RentalURIs { SI.rental_uris_android = "", SI.rental_uris_ios = "", SI.rental_uris_web = "" }
                        }
