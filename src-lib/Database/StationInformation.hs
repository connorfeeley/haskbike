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
                                                             SqlSerial,
                                                             autoSqlValueSyntax)
import           Database.Beam.Postgres                     (Postgres)
import           Database.Beam.Postgres.Syntax              (pgTextType)
import           Database.PostgreSQL.Simple.FromField       (Field (typeOid),
                                                             FromField (..),
                                                             ResultError (..),
                                                             returnError,
                                                             typename, typoid)
import           Database.PostgreSQL.Simple.ToField         (ToField (..))
import           Database.PostgreSQL.Simple.TypeInfo.Static (text)


-- | Declare a (Beam) table for the 'StationInformation' type.
data StationInformationT f where
  StationInformation :: { _information_id                        :: Columnar f (SqlSerial Int32)
                        , _information_station_id                :: Columnar f Int32
                        , _information_name                      :: Columnar f Text.Text
                        , _information_physical_configuration    :: Columnar f BeamPhysicalConfiguration
                        , _information_lat                       :: Columnar f Double
                        , _information_lon                       :: Columnar f Double
                        , _information_altitude                  :: Columnar f Double
                        , _information_address                   :: Columnar f Text.Text
                        , _information_capacity                  :: Columnar f Int32
                        , _information_is_charging_station       :: Columnar f Bool
                        , _information_rental_methods            :: Columnar f (Vector.Vector BeamRentalMethod)
                        , _information_is_virtual_station        :: Columnar f Bool
                        , _information_groups                    :: Columnar f (Vector.Vector Text.Text)
                        , _information_obcn                      :: Columnar f Text.Text
                        , _information_nearby_distance           :: Columnar f Double
                        , _information_bluetooth_id              :: Columnar f Text.Text
                        , _information_ride_code_support         :: Columnar f Bool
                        -- , _information_rental_uris               :: Columnar f SI.RentalURIs
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
  data PrimaryKey StationInformationT f = StationInformationId (Columnar f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = StationInformationId . _information_id

-- | Lenses
StationInformation
  (LensFor information_id)
  (LensFor information_station_id)
  (LensFor information_name)
  (LensFor information_physical_configuration)
  (LensFor information_lat)
  (LensFor information_lon)
  (LensFor information_altitude)
  (LensFor information_address)
  (LensFor information_capacity)
  (LensFor information_is_charging_station)
  (LensFor information_rental_methods)
  (LensFor information_is_virtual_station)
  (LensFor information_groups)
  (LensFor information_obcn)
  (LensFor information_nearby_distance)
  (LensFor information_bluetooth_id)
  (LensFor information_ride_code_support)
  -- (LensFor information_rental_uris)
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
  StationInformation { _information_id                        = default_
                     , _information_station_id                = fromIntegral station_id
                     , _information_name                      = val_ $ Text.pack name
                     , _information_physical_configuration    = val_ (coerce physical_configuration :: BeamPhysicalConfiguration)
                     , _information_lat                       = val_ lat
                     , _information_lon                       = val_ lon
                     , _information_altitude                  = val_ altitude
                     , _information_address                   = val_ $ Text.pack address
                     , _information_capacity                  = fromIntegral capacity
                     , _information_is_charging_station       = val_ is_charging_station
                     , _information_rental_methods            = val_ $ fromList (coerce rental_methods :: [BeamRentalMethod])
                     , _information_is_virtual_station        = val_ is_virtual_station
                     , _information_groups                    = val_ $ fromList $ fmap Text.pack groups
                     , _information_obcn                      = val_ $ Text.pack obcn
                     , _information_nearby_distance           = val_ nearby_distance
                     , _information_bluetooth_id              = val_ $ Text.pack bluetooth_id
                     , _information_ride_code_support         = val_ ride_code_support
                     -- , _information_rental_uris               = val_ ""
                     }

-- | Convert from the Beam StationInformation type to the JSON StationInformation
fromBeamStationInformationToJSON :: StationInformation -> SI.StationInformation
fromBeamStationInformationToJSON (StationInformation
                                  _
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
  SI.StationInformation { SI.information_station_id                = fromIntegral station_id
                        , SI.information_name                      = show name
                        , SI.information_physical_configuration    = coerce physical_configuration :: SI.PhysicalConfiguration
                        , SI.information_lat                       = lat
                        , SI.information_lon                       = lon
                        , SI.information_altitude                  = altitude
                        , SI.information_address                   = Text.unpack address
                        , SI.information_capacity                  = fromIntegral capacity
                        , SI.information_is_charging_station       = is_charging_station
                        , SI.information_rental_methods            = coerce (toList rental_methods) :: [SI.RentalMethod]
                        , SI.information_is_virtual_station        = is_virtual_station
                        , SI.information_groups                    = Text.unpack <$> toList groups
                        , SI.information_obcn                      = Text.unpack obcn
                        , SI.information_nearby_distance           = nearby_distance
                        , SI.information_bluetooth_id              = Text.unpack bluetooth_id
                        , SI.information_ride_code_support         = ride_code_support
                        -- , SI.information_rental_uris               = SI.RentalURIs { SI.rental_uris_android = "", SI.rental_uris_ios = "", SI.rental_uris_web = "" }
                        }
