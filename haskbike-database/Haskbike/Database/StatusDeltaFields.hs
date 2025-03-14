-- |

module Haskbike.Database.StatusDeltaFields
     ( StatusDeltaFields (..)
     , calculateDelta
     ) where

import qualified Data.Map                   as Map
import           Data.Maybe                 ( listToMaybe )
import           Data.Time

import           Haskbike.API.StationStatus
import           Haskbike.API.VehicleType


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
  , statusDeltaVehicleTypesAvailableBoost  = (lookupCount Boost  . vta) b - (lookupCount Boost  . vta) a
  , statusDeltaVehicleTypesAvailableIconic = (lookupCount Iconic . vta) b - (lookupCount Iconic . vta) a
  , statusDeltaVehicleTypesAvailableEfit   = (lookupCount EFit   . vta) b - (lookupCount EFit   . vta) a
  , statusDeltaVehicleTypesAvailableEfitG5 = (lookupCount EFitG5 . vta) b - (lookupCount EFitG5 . vta) a
  }
  where
    vta = _statusVehicleTypesAvailable


lookupCount :: Ord k => k -> Map.Map k VehicleType -> Int
lookupCount tvt vt = maybe 0 vehicleTypeCnt (Map.lookup tvt vt)

vehicleDocks :: Num b => StationStatus -> b
vehicleDocks status = maybe 0 (fromIntegral . dock_count) (listToMaybe (_statusVehicleDocksAvailable status))


-- type StatusMap = Map.Map Int StationStatus

-- processNewStatus :: StatusMap -> StatusMap -> StatusMap
-- processNewStatus old new = undefined
--   where newDeltas = calculateNewDeltas old new

-- calculateNewDeltas :: (k ~ Int, Ord k) => Map.Map k StationStatus -> Map.Map k StationStatus -> [StatusDeltaFields]
-- calculateNewDeltas old new = Map.elems (Map.intersectionWith calculateDelta old new)
