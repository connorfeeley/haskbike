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
  setupTestDatabase
  initDBWithAllTestData

  chargings <- runWithAppM dbnameTest (queryChargingEventsCount variation)
  assertEqual "Expected number of chargings for entire system" 2 (sumAllCharging chargings)
  where
    -- Query for all stations, for all data in the test dataset.
    variation = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (read "2023-01-01") (timeOfDayToTime midnight))
      , LatestTime   (UTCTime (read "2024-01-01") (timeOfDayToTime midnight))
      ]

-- | HUnit test to query all charging events (using exported database dump).
unit_queryChargings' :: IO ()
unit_queryChargings' = do
  setupTestDatabase
  initDBWithExportedData

  assertChargings 135 41 94 (runWithAppM dbnameTest $ queryChargingEventsCount variation)
  where
    -- Query for all stations, for all data in the test dataset.
    variation = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (read "2023-01-01") (timeOfDayToTime midnight))
      , LatestTime   (UTCTime (read "2024-01-01") (timeOfDayToTime midnight))
      ]

-- | HUnit test to query all charging events (using manually constructed test data).
unit_queryChargingsManual :: IO ()
unit_queryChargingsManual = do
  setupTestDatabase

  runWithAppM dbnameTest $ do
    -- Insert the single manually constructed station information.
    void $ insertStationInformation [manualStationInformation]
    -- Insert manually constructed station status.
    void $ insertStationStatus manualStatus

  -- Check charging events.
  void $ do
    -- First E-Fit charged.
    assertChargings 1 1 0 (between 0 5)

    -- Second two bikes (E-Fit and E-Fit G5) charged.
    assertChargings 3 2 1 (between 0 8)

    -- Genuinely broken bike is docked.
    assertChargings 3 2 1 (between 0 9)

    assertChargings 3 2 1 (between 0 10)

    -- Dock a dead E-Fit G5, dock a charged E-Fit, undock the charged E-Fit, charge the initally docked E-Fit G5.
    assertChargings 4 2 2 (between 0 14)

    -- Dock an Iconic.
    assertChargings 4 2 2 (between 0 15)
  where
    -- | Get charging events between two timestamps (with varying minute values).
    between startMinute endMinute = runWithAppM dbnameTest $ queryChargingEventsCount (variation startMinute endMinute)

    -- Query charging events between two minutes, for all stations.
    variation startMinute endMinute = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 startMinute 0)))
      , LatestTime   (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 endMinute   0)))
      ]

-- | HUnit test to query all charging events (using manually constructed test data).
unit_queryDockingsManual :: IO ()
unit_queryDockingsManual = do
  setupTestDatabase

  runWithAppM dbnameTest $ do
    -- Insert the single manually constructed station information.
    void $ insertStationInformation [manualStationInformation]
    -- Insert manually constructed station status.
    void $ insertStationStatus manualStatus

  -- Check docking events.
  void $ do
    -- First E-Fit charged.
    assertDockings   2 0 2 0 (between 0 5)
    assertUndockings 1 0 1 0 (between 0 5)

    -- Second two bikes (E-Fit and E-Fit G5) charged.
    assertDockings   4 0 3 1 (between 0 8)
    assertUndockings 2 0 2 0 (between 0 8)

    -- Genuinely broken bike is docked.
    assertDockings   4 0 3 1 (between 0 9)
    assertUndockings 4 0 3 1 (between 0 9)

    assertDockings   4 0 3 1 (between 0 10)
    assertUndockings 4 0 3 1 (between 0 10)

    -- Dock a dead E-Fit G5, dock a charged E-Fit, undock the charged E-Fit, charge the initally docked E-Fit G5.
    assertDockings   6 0 4 2 (between 0 14)
    assertUndockings 5 0 4 1 (between 0 14)

    -- Dock an Iconic.
    assertDockings   7 1 4 2 (between 0 15)
    assertUndockings 5 0 4 1 (between 0 15)

  where
    -- | Get docking events between two timestamps (with varying minute values).
    between startMinute endMinute = runWithAppM dbnameTest $ queryDockingEventsCount (variation startMinute endMinute)

    -- | Assert that the number of docking events is as expected.
    assertDockings expectedAll expectedIconic expectedEfit expectedEfitG5 cond = do
      events <- cond
      assertEqual "Expected total dockings"    expectedAll    (abs (sumEvents Docking (allBikeEvents events)))
      assertEqual "Expected Iconic dockings"   expectedIconic (abs (sumEvents Docking (iconicEvents  events)))
      assertEqual "Expected E-Fit dockings"    expectedEfit   (abs (sumEvents Docking (efitEvents    events)))
      assertEqual "Expected E-Fit G5 dockings" expectedEfitG5 (abs (sumEvents Docking (efitG5Events  events)))

    -- | Assert that the number of undocking events is as expected.
    assertUndockings expectedAll expectedIconic expectedEfit expectedEfitG5 cond = do
      events <- cond
      assertEqual "Expected total undockings"    expectedAll    (abs (sumEvents Undocking (allBikeEvents events)))
      assertEqual "Expected Iconic undockings"   expectedIconic (abs (sumEvents Undocking (iconicEvents  events)))
      assertEqual "Expected E-Fit undockings"    expectedEfit   (abs (sumEvents Undocking (efitEvents    events)))
      assertEqual "Expected E-Fit G5 undockings" expectedEfitG5 (abs (sumEvents Undocking (efitG5Events  events)))

    -- Query charging events between two minutes, for all stations.
    variation startMinute endMinute = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 startMinute 0)))
      , LatestTime   (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 endMinute   0)))
      ]

-- | HUnit test to query all charging events (using exported database dump).
-- unit_queryDisabledBikeSeconds :: IO ()
-- unit_queryDisabledBikeSeconds = do
--   setupTestDatabase
--   initDBWithExportedData

--   assertChargings 135 41 94 (runWithAppM dbnameTest $ queryDisabledBikeSeconds variation)
--   where
--     -- Query for all stations, for all data in the test dataset.
--     variation = StatusVariationQuery (Just 7001)
--       [ EarliestTime (UTCTime (read "2023-01-01") (timeOfDayToTime midnight))
--       , LatestTime   (UTCTime (read "2024-01-01") (timeOfDayToTime midnight))
--       ]

-- | HUnit test to query the number of {bikes, docks} {available, disabled} and {iconic, efit, efit g5} available across entire system at a point in time.
unit_querySystemStatus :: IO ()
unit_querySystemStatus = do
  setupTestDatabase
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
