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
     ) where

import           API.Types                       ()
import qualified API.Types                       as AT
import           API.Utils

import           AppEnv

import           Data.Functor                    ( void )
import           Data.Time

import           Database.BikeShare.ImportExport
import           Database.BikeShare.Operations
import           Database.BikeShare.Types        ( StationStatus )
import           Database.BikeShare.Utils

import           Test.Tasty.HUnit

import           TestDatabase


-- * Test setup.

-- | Initialize empty database from exported station information and station status JSON.
initDBWithExportedData :: IO ()
initDBWithExportedData = do
  importDbTestData "test/dumps/" "station_information_2023-10-30.json" "station_status_2023-10-30_2023-10-30.json"


-- * Manually-created test data.

-- | Manually constructed station information record for testing.
manualStationInformation :: AT.StationInformation
manualStationInformation =
  AT.StationInformation { AT.infoStationId = 7001
                        , AT.infoName = "Wellesley Station Green P"
                        , AT.infoPhysicalConfiguration = AT.ElectricBikeStation
                        , AT.infoLat = 43.66496415990742
                        , AT.infoLon = -79.38355031526893
                        , AT.infoAltitude = Just 0.0
                        , AT.infoAddress = Just "Yonge / Wellesley"
                        , AT.infoCapacity = 23
                        , AT.infoIsChargingStation = True
                        , AT.infoRentalMethods = [AT.Key, AT.TransitCard, AT.CreditCard, AT.Phone]
                        , AT.infoIsValetStation = Just False
                        , AT.infoIsVirtualStation = False
                        , AT.infoGroups = []
                        , AT.infoObcn = "416-617-9576"
                        , AT.infoNearbyDistance = 500.0
                        , AT.infoBluetoothId = ""
                        , AT.infoRideCodeSupport = True
                        , AT.infoRentalUris = AT.RentalURIs "" "" ""
                        }


manualStatus :: [AT.StationStatus]
manualStatus = map (stationStatusFromSimple baseStatus) manualSimpleStatus


manualSimpleStatus :: [StationStatusSimple]
manualSimpleStatus =
  zipWith (\m statusFunc -> statusFunc (TimeOfDay 0 m 0))
  [1..] -- incrementing minutes
  [ \t -> mkStatusSimple t 0 0 0 0 -- (0) Empty station.
  -- One E-Fit docking and undocking.
  , \t -> mkStatusSimple t 0 1 0 0 -- (1) Dock an E-Fit.
  , \t -> mkStatusSimple t 0 0 0 0 -- (2) Undock an E-Fit.
  -- Charge a bike.
  , \t -> mkStatusSimple t 0 0 0 1 -- (3) A mysterious disabled bike appears. Spooky.
  , \t -> mkStatusSimple t 0 1 0 0 -- (4) The mysterious disabled bike is charged, becoming an E-Fit. Wow.
  -- Charge two bikes.
  , \t -> mkStatusSimple t 0 0 0 1 -- (5) A mysterious disabled bike appears. Our ghost returns.
  , \t -> mkStatusSimple t 0 0 0 2 -- (6) A mysterious disabled bike appears. Our ghost has friends.
  , \t -> mkStatusSimple t 0 1 1 0 -- (7) Both disabled bikes are charged, becoming an E-Fit and an E-Fit G5. Thanks, Shift Transit.
  -- Tricksy hobbits docked a busted bike.
  , \t -> mkStatusSimple t 0 0 0 1 -- (8) A mysterious disabled bike appears. Our ghost is injured.
  , \t -> mkStatusSimple t 0 0 0 0 -- (9) The disabled bike disappears. Suspicous.
  -- Dock a dead E-Fit, dock a charged E-Fit, undock the charged E-Fit, charge the initally docked E-Fit.
  , \t -> mkStatusSimple t 0 0 0 1 -- (10) Dock a mystery bike.
  , \t -> mkStatusSimple t 0 1 0 1 -- (11) Dock a (charged) E-Fit.
  , \t -> mkStatusSimple t 0 0 0 1 -- (12) Undock a (charged) E-Fit.
  , \t -> mkStatusSimple t 0 0 1 0 -- (13) Mystery bike charged up and became an E-Fit G5.
  , \t -> mkStatusSimple t 1 0 1 0 -- (14) Dock an Iconic.
  ]


-- * Utilities.

-- | Assert that the number of charging events is as expected.
assertChargings :: Int -> Int -> Int -> IO [(StationStatus, [ChargingEvent])] -> IO ()
assertChargings expectedAll expectedEfit expectedEfitG5 cond = do
  assertChargings' expectedAll    sumAllCharging    cond
  assertChargings' expectedEfit   sumEfitCharging   cond
  assertChargings' expectedEfitG5 sumEfitG5Charging cond

-- | Assert that the given charging event condition holds for the expected number of charging events.
assertChargings' :: (Eq p, Show p) => p -> (a -> p) -> IO a -> IO ()
assertChargings' chargings sumType cond =
  cond >>= assertEqual "Expected number of chargings" chargings . sumType



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
