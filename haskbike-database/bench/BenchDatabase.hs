{-# LANGUAGE PartialTypeSignatures #-}
-- | Benchmark suite.

module BenchDatabase
     ( benchStationEmptyTime
     , benchWithTmp
     , bgroupDatabase
     , statusVariationAll
     ) where

import qualified Codec.Compression.Zstd                        as Z

import           Control.Monad                                 ( void )
import           Control.Monad.Catch                           ( MonadCatch )

import           Data.Aeson
import qualified Data.ByteString                               as B
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres

import           Haskbike.AppEnv
import           Haskbike.Database.Expressions
import           Haskbike.Database.Operations
import           Haskbike.Database.Operations.Factors
import           Haskbike.Database.Operations.StationOccupancy
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationOccupancy
import           Haskbike.Database.Test.Utils
import           Haskbike.TimeInterval                         ( minsPerHourlyInterval )

import           Test.Tasty.Bench


-- * Common helpers.

benchWithTmp :: String -> AppM a -> Benchmark
benchWithTmp name = bench name . whnfIO . withTempDbM Silent (setupTestDatabase >> initDBWithStationTestData) . void

-- FIXME: should combine with function above.
benchWithQueryTmp :: String -> AppM a -> Benchmark
benchWithQueryTmp name = bench name . whnfIO . withTempDbM Silent (setupTestDatabase >> initDBWithStationQueryLogData) . void

statusVariationAll :: StatusVariationQuery
statusVariationAll = StatusVariationQuery Nothing
                     [ EarliestTime (UTCTime (read "2023-01-01") (timeOfDayToTime midnight))
                     , LatestTime   (UTCTime (read "2024-01-01") (timeOfDayToTime midnight))
                     ]


earliestTime, latestTime :: UTCTime
earliestTime = UTCTime (read "2024-01-01") (timeOfDayToTime midnight)
latestTime   = UTCTime (read "2024-01-01") (timeOfDayToTime midnight)


selectWithPostgres :: FromBackendRow Postgres a => SqlSelect Postgres a -> AppM [a]
selectWithPostgres = withPostgres . runSelectReturningList


-- * Benchmarks.

-- | Benchmark group for database operations.
bgroupDatabase :: Benchmark
bgroupDatabase = bgroup "Database operations"
  [ benchWithTmp "Charging infrastructure"      . selectWithPostgres . selectWith $
    queryChargingInfrastructureE latestTime
  , benchWithTmp "System status"                . withPostgres . runSelectReturningList . selectWith $
    querySystemStatusAtRangeExpr earliestTime latestTime (minsPerHourlyInterval 4)
  , benchWithTmp "Charging events"              $ queryChargingEventsCount statusVariationAll
  , benchWithTmp "Query status factors"         $ queryStatusFactors statusVariationAll
  , benchWithTmp "Docking/undocking events"     $ queryDockingEventsCount statusVariationAll
  , benchWithTmp "Station occupancy (7001)"     $ benchStationEmptyTime (Just 7001)
  , benchWithTmp "Station occupancy (all)"      $ benchStationEmptyTime Nothing
  , benchWithTmp "Station information decoding" $ benchStationInformationDecoding "test/dumps/station_information_7001_2024-01-03_2024-01-04.json.zst"
  , benchWithTmp "Field integrals"              benchFieldIntegrals
  , benchWithQueryTmp "Latest queries"          . withPostgres . runSelectReturningList . select $ queryLatestQueryLogsE
  ]


benchStationEmptyTime :: (MonadCatch m, HasEnv env m) => Maybe Int -> m [(StationInformation, StationOccupancy)]
benchStationEmptyTime station = void query >> query -- Run query twice.
  where
    query = withPostgresTransaction $
      queryStationOccupancyE 0 0 station
      (UTCTime (fromGregorian 2023 11 01) (timeOfDayToTime midnight))
      (UTCTime (fromGregorian 2023 11 02) (timeOfDayToTime midnight))


benchStationInformationDecoding :: (HasEnv env m, MonadCatch m) => FilePath -> m [StationInformationT Identity]
benchStationInformationDecoding filePath = do
  contents <- liftIO . B.readFile $ filePath
  case Z.decompress contents of
    Z.Skip                    -> error "StationInformation: either frame was empty, or compression was done in streaming mode for path: "
    Z.Error   err             -> error err
    Z.Decompress decompressed -> do
      let info :: Either String [StationInformation] = eitherDecodeStrict decompressed
      case info of
        Left err    -> error err
        Right info' -> do
          let infoWithReported = map (\i -> (_infoReported i, fromBeamStationInformationToJSON i)) info'
          insertStationInformation infoWithReported

benchFieldIntegrals :: (HasEnv env m, MonadCatch m) => m ()
benchFieldIntegrals = do
  let variation = StatusVariationQuery (Just 7001) [ EarliestTime (UTCTime (fromGregorian 2023 10 30) (timeOfDayToTime midnight))
                                                   , LatestTime   (UTCTime (fromGregorian 2023 10 31) (timeOfDayToTime midnight))
                                                   ]

  void $ queryIntegratedStatus variation
