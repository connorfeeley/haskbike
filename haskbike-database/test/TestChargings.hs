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

import           Data.Functor                           ( void )
import           Data.Time

import           Haskbike.Database.EventCounts
import           Haskbike.Database.Operations
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Test.Utils

import           Test.Tasty.HUnit

import           UnliftIO


-- * Test units.

-- | HUnit test to query all charging events.
unit_queryChargings :: IO ()
unit_queryChargings = withTempDbM Silent (setupTestDatabase >> initDBWithStationTestData) $ do
  chargings <- queryChargingEventsCount variation
  liftIO $ assertEqual "Expected number of chargings for entire system" (-1, 0, 1, 0, 0, 0) (sumTuples chargings)
  where
    -- Query for all stations, for all data in the test dataset.
    variation = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (read "2023-01-01") (timeOfDayToTime midnight))
      , LatestTime   (UTCTime (read "2024-01-01") (timeOfDayToTime midnight))
      ]

-- | Sum the per-bike-type charging counts across all stations. Drops the StationInformation.
-- Tuple order: (totalDisabledChange, EFit, EFitG5, Cosmo, Astro, Metro).
sumTuples :: Num a => [(b, a, a, a, a, a, a)] -> (a, a, a, a, a, a)
sumTuples = foldr (\(_, a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2)
                     -> (a1 + a2, b1 + b2, c1 + c2, d1 + d2, e1 + e2, f1 + f2))
                  (0, 0, 0, 0, 0, 0)

-- | HUnit test to query all charging events (using exported database dump).
unit_queryChargings' :: IO ()
unit_queryChargings' = withTempDbM Silent (setupTestDatabase >> initDBWithExportedData) $ do
  chargings <- queryChargingEventsCount variation
  liftIO $ assertEqual "Expected number of charging for entire system" (-110, 29, 81, 0, 0, 0) (sumTuples chargings)
  where
    -- Query for all stations, for all data in the test dataset.
    variation = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (read "2023-01-01") (timeOfDayToTime midnight))
      , LatestTime   (UTCTime (read "2024-01-01") (timeOfDayToTime midnight))
      ]

-- | HUnit test to query all charging events (using manually constructed test data).
unit_queryChargingsManual :: IO ()
unit_queryChargingsManual = withTempDbM Silent setupTestDatabase $ do
  let ct = UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime midnight)

  -- Insert the single manually constructed station information.
  void $ insertStationInformation [(ct, manualStationInformation)]
  -- Insert manually constructed station status.
  void $ insertStationStatus manualStatus

  -- Check charging events. Expected tuple is (totalDisabled, EFit, EFitG5, Cosmo, Astro, Metro);
  -- the test fixtures only exercise EFit/EFitG5, so the new e-bike types are 0.
  void $ do
    -- First E-Fit charged.
    assertEqualBetweenMinute (-1, 1, 0, 0, 0, 0) 0 5

    -- -- Second two bikes (E-Fit and E-Fit G5) charged.
    assertEqualBetweenMinute (-3, 2, 1, 0, 0, 0) 0 8

    -- -- Genuinely broken bike is docked.
    assertEqualBetweenMinute (-3, 2, 1, 0, 0, 0) 0 9

    assertEqualBetweenMinute (-3, 2, 1, 0, 0, 0) 0 10

    -- -- Dock a dead E-Fit G5, dock a charged E-Fit, undock the charged E-Fit, charge the initally docked E-Fit G5.
    assertEqualBetweenMinute (-4, 2, 2, 0, 0, 0) 0 14

    -- Dock an Iconic.
    assertEqualBetweenMinute (-4, 2, 2, 0, 0, 0) 0 15
  where
    -- | Get charging events between two timestamps (with varying minute values).
    assertEqualBetweenMinute expected startMinute endMinute = do
      chargings <- queryChargingEventsCount (variation startMinute endMinute)
      liftIO $ assertEqual "Expected number of chargings for entire system" expected (sumTuples chargings)

    -- Query charging events between two minutes, for all stations.
    variation startMinute endMinute = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 startMinute 0)))
      , LatestTime   (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 endMinute   0)))
      ]

-- | HUnit test to query all charging events (using manually constructed test data).
unit_queryDockingsManual :: IO ()
unit_queryDockingsManual = withTempDbM Silent setupTestDatabase $ do
  -- Insert the single manually constructed station information.
  void $ insertStationInformation [(ct, manualStationInformation)]
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
    ct = UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime midnight)
    assertBetween
      (expectedDockingAll, expectedDockingIconic, expectedDockingEfit, expectedDockingEfitG5)
      (expectedUndockingAll, expectedUndockingIconic, expectedUndockingEfit, expectedUndockingEfitG5)
      (startMinute, endMinute) = do
      events <- queryDockingEventsCount (variation startMinute endMinute)

      let minutesString = " at " ++ show startMinute ++ "-" ++ show endMinute

      liftIO $ assertEqual ("Expected total dockings"    ++ minutesString) expectedDockingAll    (abs (sumEvents Docking (allBikeEvents events)))
      liftIO $ assertEqual ("Expected Iconic dockings"   ++ minutesString) expectedDockingIconic (abs (sumEvents Docking (iconicEvents  events)))
      liftIO $ assertEqual ("Expected E-Fit dockings"    ++ minutesString) expectedDockingEfit   (abs (sumEvents Docking (efitEvents    events)))
      liftIO $ assertEqual ("Expected E-Fit G5 dockings" ++ minutesString) expectedDockingEfitG5 (abs (sumEvents Docking (efitG5Events  events)))

      liftIO $ assertEqual ("Expected total undockings"    ++ minutesString) expectedUndockingAll    (abs (sumEvents Undocking (allBikeEvents events)))
      liftIO $ assertEqual ("Expected Iconic undockings"   ++ minutesString) expectedUndockingIconic (abs (sumEvents Undocking (iconicEvents  events)))
      liftIO $ assertEqual ("Expected E-Fit undockings"    ++ minutesString) expectedUndockingEfit   (abs (sumEvents Undocking (efitEvents    events)))
      liftIO $ assertEqual ("Expected E-Fit G5 undockings" ++ minutesString) expectedUndockingEfitG5 (abs (sumEvents Undocking (efitG5Events  events)))

    -- Query docking and undocking events between two minutes (timestamps), for all stations.
    variation startMinute endMinute = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 startMinute 0)))
      , LatestTime   (UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 endMinute   0)))
      ]

-- | HUnit test to query the number of {bikes, docks} {available, disabled} and {iconic, efit, efit g5} available across entire system at a point in time.
unit_querySystemStatus :: IO ()
unit_querySystemStatus = withTempDbM Silent (setupTestDatabase >> initDBWithExportedData) $ do
  systemStatus <- querySystemStatusAtRange (earliest ctz) (latest ctz) 60 -- 60 minute interval
  liftIO $ assertEqual "" (expected ctz) systemStatus
  where
    ctz :: TimeZone = read "EDT"
    earliest tz = localTimeToUTC tz (LocalTime (fromGregorian 2023 10 30) (TimeOfDay 07 00 00))
    latest   tz = localTimeToUTC tz (LocalTime (fromGregorian 2023 10 30) (TimeOfDay 08 00 00))
    -- Per-bike-type 8-tuple order: (Boost, Iconic, EFit, EFitG5, CHLOE, Cosmo, Astro, Metro).
    expected tz = [ ( earliest tz -- Latest time
                     , 6099 -- Total available bikes
                     , 192  -- Total disabled  bikes
                     , 7284 -- Total available docks
                     , 64   -- Total disabled  docks
                     , (0, 5714, 106, 279, 0, 0, 0, 0)
                     )
                   , ( latest tz -- Latest time
                     , 6038 -- Total available bikes
                     , 200  -- Total disabled  bikes
                     , 7336 -- Total available docks
                     , 62   -- Total disabled  docks
                     , (0, 5663, 102, 273, 0, 0, 0, 0)
                     ) ]
