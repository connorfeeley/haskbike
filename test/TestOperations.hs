
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Test the database's charging count calculations and docking/undocking counts.

module TestOperations
     ( unit_queryFieldIntegrals
     , unit_queryStatusFactors
     , unit_stationEmptyTime
     ) where

import qualified API.StationStatus                          as AT
import           API.Utils

import           AppEnv

import           Control.Monad                              ( void )

import           Data.Fixed                                 ( Pico )
import           Data.Time

import           Database.Beam
import           Database.BikeShare.Expressions
import           Database.BikeShare.Operations
import           Database.BikeShare.Operations.Factors
import           Database.BikeShare.Operations.StationEmpty
import           Database.BikeShare.StatusVariationQuery
import           Database.BikeShare.Utils

import           Test.Tasty.HUnit

import           TestDatabase

import           Utils


-- * Test units.

-- | HUnit test to query each column multiplied by the time delta (in seconds) between the previous row.
unit_queryFieldIntegrals :: IO ()
unit_queryFieldIntegrals = do
  _ <- liftIO $ runWithAppMSuppressLog dbnameTest setupTestDatabase
  _ <- initDBWithExportedData

  let variation = StatusVariationQuery (Just 7001) [ EarliestTime (UTCTime (fromGregorian 2023 10 30) (timeOfDayToTime midnight))
                                                   , LatestTime   (UTCTime (fromGregorian 2023 10 31) (timeOfDayToTime midnight))
                                                   ]
  -- Verified with a spreadsheet.
  let expected7001 =
        StatusIntegral { intStatusStationId          = 7001
                       , intStatusVariation          = variation
                       , intStatusCharging           = True
                       , intStatusCapacity           = 23
                       , intStatusTotalSeconds       = 42805
                       , intStatusSecBikesAvailable  = 296528
                       , intStatusSecBikesDisabled   = 496422
                       , intStatusSecDocksAvailable  = 191565
                       , intStatusSecDocksDisabled   = 0
                       , intStatusSecIconicAvailable = 62361
                       , intStatusSecEfitAvailable   = 54744
                       , intStatusSecEfitG5Available = 179423
                       }

  integrals <- runWithAppM dbnameTest $ queryIntegratedStatus variation

  assertEqual "Expected values of column integrals" [expected7001] integrals

-- | HUnit test to calculate usage factors (query each column multiplied by the time delta (in seconds) between the previous row, divided by total time, divided by capacity).
unit_queryStatusFactors :: IO ()
unit_queryStatusFactors = do
  _ <- liftIO $ runWithAppMSuppressLog dbnameTest setupTestDatabase
  _ <- initDBWithExportedData

  let variation = StatusVariationQuery (Just 7001) [ EarliestTime (UTCTime (fromGregorian 2023 10 30) (timeOfDayToTime midnight))
                                                   , LatestTime   (UTCTime (fromGregorian 2023 10 31) (timeOfDayToTime midnight))
                                                   ]
  let expected7001 =
        StatusFactor { statusFactorStationId       = 7001
                     , statusFactorVariation       = variation
                     , statusFactorCharging        = True
                     , statusFactorCapacity        = 23
                     , statusFactorTotalSeconds    = 42805
                     , statusFactorBikesAvailable  = 0.3011919574612881
                     , statusFactorBikesDisabled   = 0.5042300015743792
                     , statusFactorDocksAvailable  = 0.19457804096433268
                     , statusFactorDocksDisabled   = 0.0
                     , statusFactorIconicAvailable = 6.334184852440035e-2
                     , statusFactorEfitAvailable   = 5.560504410801258e-2
                     , statusFactorEfitG5Available = 0.18224506482887512
                     , statusFactorNormalizedIconicAvailable = 0.2103039173366428
                     , statusFactorNormalizedEfitAvailable   = 0.18461662979549995
                     , statusFactorNormalizedEfitG5Available = 0.6050794528678574
                     }

  integrals <- runWithAppM dbnameTest $ queryStatusFactors variation

  assertEqual "Expected status factors" [expected7001] integrals

  assertEqual "Expected sum of status factors equals 1.0" 1.0 (sumStatusFactors expected7001)

  assertEqual "Expected sum of normalized bike status factors equals 1.0" 1.0 (sumBikeStatusFactors expected7001)


-- | Test station empty query.
unit_stationEmptyTime :: IO ()
unit_stationEmptyTime = do
  runWithAppMSuppressLog dbnameTest $ void $ do
    setupTestDatabase

    insertStationInformation ct [manualStationInformation]
    insertStationStatus statusInsert

  check 1 (calendarTimeTime nominalDay)
  check 2 (calendarTimeTime (nominalDay / 2))
  check 3 (calendarTimeTime 0)
  check 4 (calendarTimeTime nominalDay)
  check 5 (calendarTimeTime (nominalDay / 4))
  check 6 (calendarTimeTime (60*5))
  check 7 (calendarTimeTime (60*5))

  where
    ct = UTCTime (fromGregorian 2023 01 01) (timeOfDayToTime (TimeOfDay 0 0 0))

    statusInsert :: [AT.StationStatus]
    statusInsert = map (stationStatusFromSimple baseStatus) simpleStatus

    simpleStatus :: [StationStatusSimple]
    simpleStatus =
      map (\(d, t, (bikesAv, docksAv)) ->
             StationStatusSimple 7001 (UTCTime d (timeOfDayToTime t)) 0 bikesAv 0 docksAv 0 bikesAv 0 0
          )
      [ (fromGregorian 2023 01 01, TimeOfDay  0 0 0,  (0, 0))

      , (fromGregorian 2023 01 02, TimeOfDay  0 0 0,  (0, 0))
      , (fromGregorian 2023 01 02, TimeOfDay 12 0 0,  (1, 0))

      -- No data for day 3 intentionally.

      , (fromGregorian 2023 01 04, TimeOfDay 00 00 00, (0, 0))

      , (fromGregorian 2023 01 05, TimeOfDay 00 00 00, (1, 0))
      , (fromGregorian 2023 01 05, TimeOfDay 06 00 00, (1, 0))
      , (fromGregorian 2023 01 05, TimeOfDay 12 00 00, (0, 0))
      , (fromGregorian 2023 01 05, TimeOfDay 18 00 00, (1, 0))

      , (fromGregorian 2023 01 06, TimeOfDay 00 00 00, (1, 0))
      , (fromGregorian 2023 01 06, TimeOfDay 23 55 00, (0, 0))

      , (fromGregorian 2023 01 07, TimeOfDay 00 05 00, (1, 0))
      ]

toDuration :: Pico -> CalendarDiffTime
toDuration = calendarTimeTime . secondsToNominalDiffTime

check :: DayOfMonth -> CalendarDiffTime -> IO ()
check d expected = do
  empty <- runWithAppMSuppressLog dbnameTest $ withPostgres $ do
    runSelectReturningList $ selectWith $
      queryStationEmptyTime
      (UTCTime (fromGregorian 2023 01 d)      (timeOfDayToTime (TimeOfDay 0 0 0)))
      (UTCTime (fromGregorian 2023 01 (d +1)) (timeOfDayToTime (TimeOfDay 0 0 0)))
  assertEqual ("Station empty time " <> show d) expected ((toDuration . fromIntegral . snd . head) empty)
