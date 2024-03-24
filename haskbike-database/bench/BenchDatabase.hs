-- | Benchmark suite.

module BenchDatabase where

import           Control.Monad                             ( void )

import           Data.Time

import           Database.Beam

import           Haskbike.AppEnv
import           Haskbike.Database.Operations
import           Haskbike.Database.Operations.StationEmpty
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Test.Utils


benchStationEmptyTime :: Maybe Int -> IO ()
benchStationEmptyTime station = withTempDbM Silent (setupTestDatabase >> initDBWithAllTestData) $ do
  void $ withPostgres $ runSelectReturningList $ select $ queryStationEmptyFullTime station
    (UTCTime (fromGregorian 2023 11 01) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2023 11 02) (timeOfDayToTime midnight))


benchQueryChargings :: IO ()
benchQueryChargings = withTempDbM Silent (setupTestDatabase >> initDBWithAllTestData) $ do
  void $ queryChargingEventsCount variation
  where
    -- Query for all stations, for all data in the test dataset.
    variation = StatusVariationQuery Nothing
      [ EarliestTime (UTCTime (read "2023-01-01") (timeOfDayToTime midnight))
      , LatestTime   (UTCTime (read "2024-01-01") (timeOfDayToTime midnight))
      ]
