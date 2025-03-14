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
     ( unit_cacheStationOccupancy
     , unit_queryFieldIntegrals
     , unit_queryStatusFactors
     , unit_stationEmptyTime
     , unit_stationEmptyTimeExported
     ) where

import           Control.Monad.Catch                           ( MonadCatch )

import           Data.Fixed                                    ( Pico )
import           Data.Int                                      ( Int32 )
import           Data.Time

import           Database.Beam

import qualified Haskbike.API.StationStatus                    as AT
import           Haskbike.API.Utils
import           Haskbike.AppEnv
import           Haskbike.Database.Operations
import           Haskbike.Database.Operations.Factors
import           Haskbike.Database.Operations.StationOccupancy
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationOccupancy
import           Haskbike.Database.Test.Utils

import           Test.Tasty.HUnit

import           UnliftIO


-- * Test units.

-- | HUnit test to query each column multiplied by the time delta (in seconds) between the previous row.
unit_queryFieldIntegrals :: IO ()
unit_queryFieldIntegrals = withTempDbM Silent (setupTestDatabase >> initDBWithExportedData) $ do
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

  integrals <- queryIntegratedStatus variation

  liftIO $ assertEqual "Expected values of column integrals" [expected7001] integrals

-- | HUnit test to calculate usage factors (query each column multiplied by the time delta (in seconds) between the previous row, divided by total time, divided by capacity).
unit_queryStatusFactors :: IO ()
unit_queryStatusFactors = withTempDbM Silent (setupTestDatabase >> initDBWithExportedData) $ do
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

  integrals <- queryStatusFactors variation

  liftIO $ assertEqual "Expected status factors" [expected7001] integrals

  liftIO $ assertEqual "Expected sum of status factors equals 1.0" 1.0 (sumStatusFactors expected7001)

  liftIO $ assertEqual "Expected sum of normalized bike status factors equals 1.0" 1.0 (sumBikeStatusFactors expected7001)


-- | Test station empty query.
unit_stationEmptyTime :: IO ()
unit_stationEmptyTime = withTempDbM Silent setupTestDatabase $ do
  insertStationInformation [(ct, manualStationInformation)]
  insertStationStatus statusInsert

  check 1 (Just (calendarTimeTime nominalDay))
  check 2 (Just (calendarTimeTime (nominalDay / 2)))
  check 3 (Just (calendarTimeTime 0))
  check 4 (Just (calendarTimeTime nominalDay))
  check 5 (Just (calendarTimeTime (nominalDay / 4)))
  check 6 (Just (calendarTimeTime (60*5)))
  check 7 (Just (calendarTimeTime (60*5)))

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
      , (fromGregorian 2023 01 01, TimeOfDay 12 0 0,  (0, 0))

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

check :: (HasEnv env m, MonadIO m, MonadFail m, MonadUnliftIO m, MonadCatch m)
      => DayOfMonth -> Maybe CalendarDiffTime -> m ()
check d expected = do
  empty <- queryStationOccupancy 0 0 (Nothing :: Maybe Int32)
    (UTCTime (fromGregorian 2023 01 d)      (timeOfDayToTime (TimeOfDay 0 0 0)))
    (UTCTime (fromGregorian 2023 01 (d +1)) (timeOfDayToTime (TimeOfDay 0 0 0)))
  liftIO $ assertEqual ("Station empty time " <> show d) expected ((secondsToDuration . _stnOccEmptySec . snd . head) empty)
  where secondsToDuration Nothing  = Nothing
        secondsToDuration (Just x) = Just ((toDuration . fromIntegral) x)


unit_stationEmptyTimeExported :: IO ()
unit_stationEmptyTimeExported = withTempDbM Silent initSteps $ do
  result <- queryStationOccupancy 0 0 (Nothing :: Maybe Int32)
    (UTCTime (fromGregorian 2024 01 03) (timeOfDayToTime (TimeOfDay 0 0 0)))
    (UTCTime (fromGregorian 2024 01 04) (timeOfDayToTime (TimeOfDay 0 0 0)))

  liftIO $ case result of
    [(_inf, occ)] -> do
      -- FIXME: calculated in excel that we should be expecting 50595 seconds empty.
      assertEqual "Station empty time (exported)" (Just 51104) (_stnOccEmptySec occ)
      assertEqual "Station full time  (exported)" (Just 0)     (_stnOccFullSec  occ)
    [] -> assertFailure "No result returned."
    _ -> assertFailure "Multiple results returned."
  where
    initSteps = setupTestDatabase >> initDBWithExportedDataDate (Just 7001) startDay endDay
    startDay  = fromGregorian 2024 01 03
    endDay    = fromGregorian 2024 01 04

unit_cacheStationOccupancy :: IO ()
unit_cacheStationOccupancy = withTempDbM Silent initSteps $ do
  occ1 <- queryAndCacheOccupancy
  assertOccupancy 1 ((_stnOccCalculated . snd . head) occ1) occ1

  occ2 <- queryAndCacheOccupancy
  assertOccupancy 1 ((_stnOccCalculated . snd . head) occ2) occ2
  where
    initSteps = setupTestDatabase >> initDBWithExportedDataDate (Just 7001) startDay endDay
    startDay  = fromGregorian 2024 01 03
    endDay    = fromGregorian 2024 01 04
    expectedOccupancy calculated =
      [ StationOccupancy { _stnOccInfo        = StationInformationId 7001 (UTCTime (fromGregorian 2023 09 24) (timeOfDayToTime (TimeOfDay 17 58 22)))
                         , _stnOccCalculated  = calculated -- Use calculated time from returned record (since it defaults to the current time).
                         , _stnOccRangeStart  = UTCTime (fromGregorian 2024 01 03) (timeOfDayToTime (TimeOfDay 00 00 00))
                         , _stnOccRangeEnd    = UTCTime (fromGregorian 2024 01 04) (timeOfDayToTime (TimeOfDay 00 00 00))
                         , _stnOccEmptyThresh = 0
                         , _stnOccFullThresh  = 0
                         , _stnOccEmptySec    = Just 51104
                         , _stnOccFullSec     = Just 0
                         }
      ]
    queryAndCacheOccupancy =
      queryStationOccupancy 0 0
      (Nothing :: Maybe Int32)
      (UTCTime startDay (timeOfDayToTime midnight))
      (UTCTime endDay   (timeOfDayToTime midnight))

    assertOccupancy :: MonadIO m => Int -> UTCTime -> [(StationInformation, StationOccupancy)] -> m ()
    assertOccupancy expectedLength expectedCalculated occ = liftIO $ do
      assertEqual "Expected number of station occupancy records" expectedLength (length occ)
      assertEqual "Cached 1 station occupancy record" (expectedOccupancy expectedCalculated) (map snd occ)
