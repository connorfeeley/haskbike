-- | Station infrormation table definition and functions.

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
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
        , fromJSONToBeamStationInformation
        , fromBeamStationInformationToJSON
        ) where

import           Control.Lens

import           Database.Beam

import           Data.Int
import qualified Data.Text          as Text
import qualified Data.Vector        as Vector
import           Data.Vector        (toList)

import qualified StationInformation as SI
import Database.PostgreSQL.Simple.FromField (FromField (..), typename, ResultError (..), returnError, Field (typeOid), typoid)
import qualified Data.ByteString.Char8 as B


-- | Declare a (Beam) table for the 'StationInformation' type.
data StationInformationT f where
  StationInformation :: { _information_id                        :: Columnar f Int32
                        , _information_station_id                :: Columnar f Int32
                        , _information_name                      :: Columnar f Text.Text
                        , _information_physical_configuration    :: Columnar f Text.Text
                        , _information_lat                       :: Columnar f Double
                        , _information_lon                       :: Columnar f Double
                        , _information_altitude                  :: Columnar f Double
                        , _information_address                   :: Columnar f Text.Text
                        , _information_capacity                  :: Columnar f Int32
                        , _information_is_charging_station       :: Columnar f Bool
                        , _information_rental_methods            :: Columnar f (Vector.Vector SI.RentalMethod)
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
  data PrimaryKey StationInformationT f = StationInformationId (Columnar f Int32)
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

instance FromField SI.RentalMethod where
   fromField f mdata = do
     typ <- typename f
     if typ /= "RentalMethod"
        then returnError Incompatible f ""
        else case B.unpack `fmap` mdata of
               Nothing  -> returnError UnexpectedNull f ""
               Just dat ->
                  case [ x | (x,t) <- reads dat, ("","") <- lex t ] of
                    [x] -> return x
                    _   -> returnError ConversionFailed f dat

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
                     , _information_physical_configuration    = val_ $ Text.pack ""
                     , _information_lat                       = lat
                     , _information_lon                       = lon
                     , _information_altitude                  = altitude
                     , _information_address                   = val_ $ Text.pack address
                     , _information_capacity                  = fromIntegral capacity
                     , _information_is_charging_station       = val_ is_charging_station
                     , _information_rental_methods            = val_ $ Text.pack $ show rental_methods
                     , _information_is_virtual_station        = val_ is_virtual_station
                     , _information_groups                    = val_ $ Text.pack $ show groups
                     , _information_obcn                      = val_ $ Text.pack obcn
                     , _information_nearby_distance           = nearby_distance
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
                        , SI.information_physical_configuration    = SI.Regular
                        , SI.information_lat                       = lat
                        , SI.information_lon                       = lon
                        , SI.information_altitude                  = altitude
                        , SI.information_address                   = ""
                        , SI.information_capacity                  = fromIntegral capacity
                        , SI.information_is_charging_station       = is_charging_station
                        , SI.information_rental_methods            = toList rental_methods
                        , SI.information_is_virtual_station        = is_virtual_station
                        , SI.information_groups                    = Text.unpack <$> toList groups
                        , SI.information_obcn                      = Text.unpack obcn
                        , SI.information_nearby_distance           = nearby_distance
                        , SI.information_bluetooth_id              = Text.unpack bluetooth_id
                        , SI.information_ride_code_support         = ride_code_support
                        -- , SI.information_rental_uris               = SI.RentalURIs { SI.rental_uris_android = "", SI.rental_uris_ios = "", SI.rental_uris_web = "" }
                        }
