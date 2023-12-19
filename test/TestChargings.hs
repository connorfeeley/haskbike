{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Test the database's charging count calculations and docking/undocking counts.

module TestChargings
     ( unit_queryChargings
     , unit_queryChargings'
     , unit_queryChargingsManual
     , unit_queryDockingsManual
     , unit_querySystemStatus
     ) where

import           AppEnv

import           Data.Functor                            ( void )
import           Data.Time

import           Database.BikeShare.EventCounts
import           Database.BikeShare.Operations
import           Database.BikeShare.StatusVariationQuery
import           Database.BikeShare.Utils

import           Test.Tasty.HUnit

import           TestDatabase

import           Utils


-- * Test units.

-- | HUnit test to query all charging events.
unit_queryChargings :: IO ()
unit_queryChargings = do
  runWithAppMSuppressLog dbnameTest setupTestDatabase
  initDBWithAllTestData

  chargings <- runWithAppM dbnameTest (queryChargingEventsCount variation)
  assertEqual "Expected number of chargings for entire system" (-1, 0, 1) (sumTuples chargings)
  where
    -- Query for all stations, for all data in the test dataset.
    variation = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (read "2023-01-01") (timeOfDayToTime midnight))
      , LatestTime   (UTCTime (read "2024-01-01") (timeOfDayToTime midnight))
      ]

sumTuples :: Num a => [(b, a, a, a)] -> (a, a, a)
sumTuples = foldr (\(_, a1, b1, c1) (a2, b2, c2) -> (a1 + a2, b1 + b2, c1 + c2)) (0, 0, 0)

-- | HUnit test to query all charging events (using exported database dump).
unit_queryChargings' :: IO ()
unit_queryChargings' = do
  runWithAppMSuppressLog dbnameTest setupTestDatabase
  initDBWithExportedData

  chargings <- runWithAppM dbnameTest $ queryChargingEventsCount variation
  assertEqual "Expected number of charging for entire system" (-110, 29, 81) (sumTuples chargings)
  where
    -- Query for all stations, for all data in the test dataset.
    variation = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (read "2023-01-01") (timeOfDayToTime midnight))
      , LatestTime   (UTCTime (read "2024-01-01") (timeOfDayToTime midnight))
      ]

-- | HUnit test to query all charging events (using manually constructed test data).
unit_queryChargingsManual :: IO ()
unit_queryChargingsManual = do
  runWithAppMSuppressLog dbnameTest setupTestDatabase

  ct <- getCurrentTime
  runWithAppM dbnameTest $ do
    -- Insert the single manually constructed station information.
    void $ insertStationInformation ct [manualStationInformation]
    -- Insert manually constructed station status.
    void $ insertStationStatus manualStatus

  -- Check charging events.
  void $ do
    -- First E-Fit charged.
    assertEqualBetweenMinute (-1, 1, 0) 0 5

    -- -- Second two bikes (E-Fit and E-Fit G5) charged.
    assertEqualBetweenMinute (-3, 2, 1) 0 8

    -- -- Genuinely broken bike is docked.
    assertEqualBetweenMinute (-3, 2, 1) 0 9

    assertEqualBetweenMinute (-3, 2, 1) 0 10

    -- -- Dock a dead E-Fit G5, dock a charged E-Fit, undock the charged E-Fit, charge the initally docked E-Fit G5.
    assertEqualBetweenMinute (-4, 2, 2) 0 14

    -- Dock an Iconic.
    assertEqualBetweenMinute (-4, 2, 2) 0 15
  where
    -- | Get charging events between two timestamps (with varying minute values).
    assertEqualBetweenMinute expected startMinute endMinute = do
      chargings <- runWithAppM dbnameTest $ queryChargingEventsCount (variation startMinute endMinute)
      assertEqual "Expected number of chargings for entire system" expected (sumTuples chargings)

    -- Query charging events between two minutes, for all stations.
    variation startMinute endMinute = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 startMinute 0)))
      , LatestTime   (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 endMinute   0)))
      ]

-- | HUnit test to query all charging events (using manually constructed test data).
unit_queryDockingsManual :: IO ()
unit_queryDockingsManual = do
  runWithAppMSuppressLog dbnameTest setupTestDatabase

  ct <- getCurrentTime

  runWithAppM dbnameTest $ do
    -- Insert the single manually constructed station information.
    void $ insertStationInformation ct [manualStationInformation]
    -- Insert manually constructed station status.
    void $ insertStationStatus manualStatus

  -- Check docking events.
  void $ do
    -- First E-Fit charged.
    assertBetween   (2, 0, 2, 0) (1, 0, 1, 0) (0, 5)

    -- Second two bikes (E-Fit and E-Fit G5) charged.
    assertBetween   (4, 0, 3, 1) (2, 0, 2, 0) (0, 8)

    -- Genuinely broken bike is docked.
    assertBetween   (4, 0, 3, 1) (4, 0, 3, 1) (0, 9)

    assertBetween   (4, 0, 3, 1) (4, 0, 3, 1) (0, 10)

    -- Dock a dead E-Fit G5, dock a charged E-Fit, undock the charged E-Fit, charge the initally docked E-Fit G5.
    assertBetween   (6, 0, 4, 2) (5, 0, 4, 1) (0, 14)

    -- Dock an Iconic.
    assertBetween   (7, 1, 4, 2) (5, 0, 4, 1) (0, 15)

  where
    assertBetween
      (expectedDockingAll, expectedDockingIconic, expectedDockingEfit, expectedDockingEfitG5)
      (expectedUndockingAll, expectedUndockingIconic, expectedUndockingEfit, expectedUndockingEfitG5)
      (startMinute, endMinute) = do
      events <- runWithAppM dbnameTest $ queryDockingEventsCount (variation startMinute endMinute)

      let minutesString = " at " ++ show startMinute ++ "-" ++ show endMinute

      assertEqual ("Expected total dockings"    ++ minutesString) expectedDockingAll    (abs (sumEvents Docking (allBikeEvents events)))
      assertEqual ("Expected Iconic dockings"   ++ minutesString) expectedDockingIconic (abs (sumEvents Docking (iconicEvents  events)))
      assertEqual ("Expected E-Fit dockings"    ++ minutesString) expectedDockingEfit   (abs (sumEvents Docking (efitEvents    events)))
      assertEqual ("Expected E-Fit G5 dockings" ++ minutesString) expectedDockingEfitG5 (abs (sumEvents Docking (efitG5Events  events)))

      assertEqual ("Expected total undockings"    ++ minutesString) expectedUndockingAll    (abs (sumEvents Undocking (allBikeEvents events)))
      assertEqual ("Expected Iconic undockings"   ++ minutesString) expectedUndockingIconic (abs (sumEvents Undocking (iconicEvents  events)))
      assertEqual ("Expected E-Fit undockings"    ++ minutesString) expectedUndockingEfit   (abs (sumEvents Undocking (efitEvents    events)))
      assertEqual ("Expected E-Fit G5 undockings" ++ minutesString) expectedUndockingEfitG5 (abs (sumEvents Undocking (efitG5Events  events)))

    -- Query docking and undocking events between two minutes (timestamps), for all stations.
    variation startMinute endMinute = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 startMinute 0)))
      , LatestTime   (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 endMinute   0)))
      ]

-- | HUnit test to query the number of {bikes, docks} {available, disabled} and {iconic, efit, efit g5} available across entire system at a point in time.
unit_querySystemStatus :: IO ()
unit_querySystemStatus = do
  runWithAppMSuppressLog dbnameTest setupTestDatabase
  initDBWithExportedData

  ctz <- getCurrentTimeZone
  systemStatus <- runWithAppM dbnameTest $
    querySystemStatusAtRange (earliest ctz) (latest ctz) 60 -- 60 minute interval
  assertEqual "" (expected ctz) systemStatus
  where
    earliest ctz = localTimeToUTC ctz (LocalTime (fromGregorian 2023 10 30) (TimeOfDay 07 00 00))
    latest   ctz = localTimeToUTC ctz (LocalTime (fromGregorian 2023 10 30) (TimeOfDay 08 00 00))
    expected ctz = [ ( earliest ctz -- Latest time
                     , 6041 -- Total available bikes
                     , 201  -- Total disabled  bikes
                     , 7345 -- Total available docks
                     , 62   -- Total disabled  docks
                     , 5666 -- Total available iconic  bikes
                     , 102  -- Total available efit    bikes
                     , 273  -- Total available efit g5 bikes
                     )
                   , ( latest ctz -- Latest time
                     , 3 -- Total available bikes
                     , 1 -- Total disabled  bikes
                     , 9 -- Total available docks
                     , 0 -- Total disabled  docks
                     , 3 -- Total available iconic  bikes
                     , 0 -- Total available efit    bikes
                     , 0 -- Total available efit g5 bikes
                     ) ]
