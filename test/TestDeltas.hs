{-# LANGUAGE PartialTypeSignatures #-}
-- |

module TestDeltas
     ( unit_testStatusDeltas
     ) where

import           API.StationStatus
import           API.Utils

import qualified Data.Map                             as Map
import           Data.Time

import           Database.BikeShare.StatusDeltaFields

import           Test.Tasty.HUnit


-- * Test data.

-- | Base status data.
baseStatus7001, baseStatus7002 :: StationStatus
baseStatus7001 = setStationId 7001 baseStatus
baseStatus7002 = setStationId 7002 baseStatus

-- "Original" map - current API data.
mapOrig :: Map.Map Int StationStatus
mapOrig = Map.fromList
          [ (7001, baseStatus7001)
          ]

statusFn :: [StationStatus -> StationStatus]
statusFn =
  map (addTimeToStatus 1 .)
  [ addBikesAvailable 1 . addIconic 1 . addBikesDisabled 1
  -- ^ Deltas: +1 available, +1 iconic, +1 disabled

  , addBikesAvailable 3 . addIconic 1 . addEfitG5 2
  -- ^ Deltas: +3 available, +1 iconic, +1 efit g5

  , addBikesAvailable (-2) . addIconic (-1) . addEfitG5 (-1) . addBikesDisabled 1
  -- ^ Deltas: -2 available, -1 iconic, -1 efit g5, +1 disabled

  , addBikesDisabled 10
  -- ^ Deltas: -2 available, -1 iconic, -1 efit g5, +1 disabled

  , addBikesAvailable  4 . addEfit 3 . addEfitG5 1
  . addBikesDisabled   1

  , addBikesAvailable  1 . addEfitG5 1
  . addBikesDisabled   (-1)

  , addBikesAvailable  2 . addEfitG5 2
  . addBikesDisabled   (-2)

  , addBikesAvailable  1 . addEfitG5 1
  . addBikesDisabled   (-1)

  , addBikesAvailable  2 . addEfitG5 2
  . addBikesDisabled   (-2)

  , addBikesAvailable  1 . addEfitG5 1
  . addBikesDisabled   (-1)

  , addBikesAvailable  1 . addEfitG5 1
  . addBikesDisabled   (-1)
  ]

foldFunctions :: StationStatus -> [StationStatus -> StationStatus] -> StationStatus
foldFunctions = foldl (flip ($))

addTimeToStatus :: NominalDiffTime -> StationStatus -> StationStatus
addTimeToStatus diffTime status =
  status { _statusLastReported = addTime diffTime status
         }

addTime :: NominalDiffTime -> StationStatus -> Maybe UTCTime
addTime diffTime  status = maybe (Just defaultLastReported) (Just . addUTCTime diffTime) (_statusLastReported status)


setStationId :: Int -> StationStatus -> StationStatus
setStationId stationId status = status { _statusStationId = stationId }


-- Our operation to combine two Ints and produce a Float
_f :: StationStatus -> StationStatus -> StatusDeltaFields
_f = calculateDelta

-- | Get expected test data per station ID and element number.
expectedDeltaFields :: Int -> Int -> StatusDeltaFields
expectedDeltaFields sid n =
  [ baseStatusDeltas { statusDeltaStationId = sid
                     , statusDeltaLastReported = (Just . addUTCTime 15) defaultLastReported
                     }
  ] !! n

-- | Base 'StatusDeltaFields' to build test data.
baseStatusDeltas :: StatusDeltaFields
baseStatusDeltas =
  StatusDeltaFields
  { statusDeltaStationId                   = 0
  , statusDeltaLastReported                = Just defaultLastReported
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

defaultLastReported :: UTCTime
defaultLastReported = UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime midnight)

-- | Calculate nth expected test value.
nthTestData :: StationStatus -> Int -> StationStatus
nthTestData base n = foldFunctions base (take n statusFn)

-- * Test cases.

-- Step 1: Intersection
-- intersection :: Map.Map Int StatusDeltaFields
-- intersection = Map.intersectionWith calculateDelta mapOrig mapNew

unit_testStatusDeltas :: IO ()
unit_testStatusDeltas = do
  pure ()
  -- TODO
  -- assertEqual "Intersection"
  --   (Map.intersectionWith calculateDelta mapOrig (statusMapAt 0))
  --   (Map.fromList [ (7001, expectedDeltaFields 7001 0)
  --                 , (7002, expectedDeltaFields 7002 0)
  --                 ])
  -- assertEqual "End" (expectedResult 1) (expectedResult 1)
  where
    expectedResult = nthTestData baseStatus7001

statusMapAt :: (Ord k, Num k) => Int -> Map.Map k StationStatus
statusMapAt n = statusAt
  where statusAt = Map.fromList [ (7001, nthTestData baseStatus7001 n)
                                , (7002, nthTestData baseStatus7002 n)
                                ]
