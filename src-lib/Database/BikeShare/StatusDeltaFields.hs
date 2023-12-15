-- |

module Database.BikeShare.StatusDeltaFields
     ( StatusDeltaFields (..)
     , calculateDelta
     ) where

import           API.StationStatus

import           Control.Lens

import           Data.List         ( find )
import           Data.Maybe        ( listToMaybe )
import           Data.Time


-- * Types for creating a delta.

data StatusDeltaFields where
  StatusDeltaFields ::
    { statusDeltaStationId                   :: Int
    , statusDeltaLastReported                :: Maybe UTCTime
    , statusDeltaNumBikesAvailable           :: Int
    , statusDeltaNumBikesDisabled            :: Int
    , statusDeltaNumDocksAvailable           :: Int
    , statusDeltaNumDocksDisabled            :: Int
    , statusDeltaVehicleDocksAvailable       :: Int
    , statusDeltaVehicleTypesAvailableBoost  :: Int
    , statusDeltaVehicleTypesAvailableIconic :: Int
    , statusDeltaVehicleTypesAvailableEfit   :: Int
    , statusDeltaVehicleTypesAvailableEfitG5 :: Int
    } -> StatusDeltaFields
    deriving (Eq, Show)

calculateDelta :: StationStatus -> StationStatus -> StatusDeltaFields
calculateDelta a b = StatusDeltaFields
  { statusDeltaStationId                   = _statusStationId     b
  , statusDeltaLastReported                = _statusLastReported  b
  , statusDeltaNumBikesAvailable           = _statusNumBikesAvailable b - _statusNumBikesAvailable a
  , statusDeltaNumBikesDisabled            = _statusNumBikesDisabled  b - _statusNumBikesDisabled  a
  , statusDeltaNumDocksAvailable           = _statusNumDocksAvailable b - _statusNumDocksAvailable a
  , statusDeltaNumDocksDisabled            = _statusNumDocksDisabled  b - _statusNumDocksDisabled  a
  , statusDeltaVehicleDocksAvailable       = vehicleDocks  b - vehicleDocks  a
  , statusDeltaVehicleTypesAvailableBoost  = num_boost     b - num_boost     a
  , statusDeltaVehicleTypesAvailableIconic = num_iconic    b - num_iconic    a
  , statusDeltaVehicleTypesAvailableEfit   = num_efit      b - num_efit      a
  , statusDeltaVehicleTypesAvailableEfitG5 = num_efit_g5   b - num_efit_g5   a
  }
  where
    -- | Find the vehicle type in the list of vehicle types available; default to 0 if not found.
    findByType' status vehicle_type = find (\x -> vehicle_type_id x == vehicle_type) $ status ^. statusVehicleTypesAvailable
    findByType  status vehicle_type = fromIntegral $ maybe 0 type_count (findByType' status vehicle_type)
    num_boost   status = findByType status Boost
    num_iconic  status = findByType status Iconic
    num_efit    status = findByType status EFit
    num_efit_g5 status = findByType status EFitG5
    vehicleDocks status = maybe 0 (fromIntegral . dock_count) $ listToMaybe $ status ^. statusVehicleDocksAvailable
