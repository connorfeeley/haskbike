{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- |

module Utils where

import qualified API.Types                       as AT
import           API.Utils

import           Data.Time

import           Database.BikeShare.ImportExport
import           Database.BikeShare.Operations
import           Database.BikeShare.Types

import           Test.Tasty.HUnit


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
