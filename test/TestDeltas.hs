{-# LANGUAGE PartialTypeSignatures #-}
-- |

module TestDeltas
     ( unit_testStatusDeltas
     ) where

import qualified API.StationStatus                    as AT
import           API.Utils

import           Data.Function                        ( (&) )
import qualified Data.Map                             as Map
import           Data.Time

import           Database.BikeShare.StatusDeltaFields

import           Test.Tasty.HUnit


setStationId :: Int -> AT.StationStatus -> AT.StationStatus
setStationId stationId status = status { AT._statusStationId = stationId }


addBikesAvailable, addBikesDisabled, addDocksAvailable, addDocksDisabled :: Int -> AT.StationStatus -> AT.StationStatus
addBikesAvailable inc status = status { AT._statusNumBikesAvailable = AT._statusNumBikesAvailable status + inc }
addBikesDisabled  inc status = status { AT._statusNumBikesDisabled  = AT._statusNumBikesDisabled  status + inc }
addDocksAvailable inc status = status { AT._statusNumDocksAvailable = AT._statusNumDocksAvailable status + inc }
addDocksDisabled  inc status = status { AT._statusNumDocksDisabled  = AT._statusNumDocksDisabled  status + inc }

baseStatus7001, baseStatus7002 :: AT.StationStatus
baseStatus7001 = setStationId 7001 baseStatus
baseStatus7002 = setStationId 7002 baseStatus

-- "Original" map - current API data.
mapOrig :: Map.Map Int AT.StationStatus
mapOrig = Map.fromList
          [ (7001, baseStatus7001)
          , (7002, baseStatus7002)
          ]

-- "New" map - data fetched from API but not yet inserted.
mapNew :: Map.Map Int AT.StationStatus
mapNew = Map.fromList
         [ (7001, newBaseStatus baseStatus7001)
         , (7002, newBaseStatus baseStatus7002)
         ]
  where
    newBaseStatus status = status
                         & addTimeToStatus   15
                         & addBikesAvailable  1
                         & addBikesDisabled   1

addTimeToStatus :: NominalDiffTime -> AT.StationStatus -> AT.StationStatus
addTimeToStatus diffTime status =
  status { AT._statusLastReported = addTime diffTime status
         }

addTime :: NominalDiffTime -> AT.StationStatus -> Maybe UTCTime
addTime diffTime  status = maybe (Just defaultLastReported) (Just . addUTCTime diffTime) (AT._statusLastReported status)

-- Our operation to combine two Ints and produce a Float
_f :: AT.StationStatus -> AT.StationStatus -> StatusDeltaFields
_f = calculateDelta

-- Step 1: Intersection
intersection :: Map.Map Int StatusDeltaFields
intersection = Map.intersectionWith calculateDelta mapOrig mapNew

defaultLastReported :: UTCTime
defaultLastReported = UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime midnight)

unit_testStatusDeltas :: IO ()
unit_testStatusDeltas = do
  assertEqual "Intersection" intersection (Map.fromList
                                           [ (7001, expectedDeltaFields { statusDeltaStationId = 7001 })
                                           , (7002, expectedDeltaFields { statusDeltaStationId = 7002 })
                                           ])
  where
    expectedDeltaFields =
      StatusDeltaFields
      { statusDeltaStationId                   = 7001
      , statusDeltaLastReported                = (Just . addUTCTime 15) defaultLastReported
      , statusDeltaNumBikesAvailable           = 1
      , statusDeltaNumBikesDisabled            = 1
      , statusDeltaNumDocksAvailable           = 0
      , statusDeltaNumDocksDisabled            = 0
      , statusDeltaVehicleDocksAvailable       = 0
      , statusDeltaVehicleTypesAvailableBoost  = 0
      , statusDeltaVehicleTypesAvailableIconic = 0
      , statusDeltaVehicleTypesAvailableEfit   = 0
      , statusDeltaVehicleTypesAvailableEfitG5 = 0
      }
