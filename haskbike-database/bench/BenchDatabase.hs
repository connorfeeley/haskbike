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
import qualified Data.ByteString.Lazy                          as BL
import           Data.Int                                      ( Int32 )
import           Data.Maybe                                    ( fromMaybe )
import           Data.Time

import           Database.Beam
import           Database.Beam.Postgres

import           Haskbike.API.ResponseWrapper
import           Haskbike.AppEnv
import           Haskbike.Database.Expressions
import           Haskbike.Database.Operations
import           Haskbike.Database.Operations.Factors
import           Haskbike.Database.Operations.StationOccupancy
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Test.Utils
import           Haskbike.TimeInterval                         ( minsPerHourlyInterval )

import           Test.Tasty.Bench


-- * Common helpers.

benchWithTmp :: String -> AppM a -> Benchmark
benchWithTmp name = bench name . whnfIO . withTempDbM Silent (setupTestDatabase >> initDBWithAllTestData) . void

statusVariationAll :: StatusVariationQuery
statusVariationAll = StatusVariationQuery Nothing
                     [ EarliestTime (UTCTime (read "2023-01-01") (timeOfDayToTime midnight))
                     , LatestTime   (UTCTime (read "2024-01-01") (timeOfDayToTime midnight))
                     ]

earliestTime, latestTime :: UTCTime
earliestTime = UTCTime (read "2024-01-01") (timeOfDayToTime midnight)
latestTime   = UTCTime (read "2024-01-01") (timeOfDayToTime midnight)


-- * Benchmarks.

-- | Benchmark group for database operations.
bgroupDatabase :: Benchmark
bgroupDatabase = bgroup "Database operations"
  [ benchWithTmp "Charging infrastructure"   . selectWithPostgres . selectWith $
    queryChargingInfrastructure latestTime
  , benchWithTmp "System status"             . withPostgres . runSelectReturningList . selectWith $
    querySystemStatusAtRangeExpr earliestTime latestTime (minsPerHourlyInterval 4)
  , benchWithTmp "Charging events"           $ queryChargingEventsCount statusVariationAll
  , benchWithTmp "Query status factors"      $ queryStatusFactors statusVariationAll
  , benchWithTmp "Docking/undocking events"  $ queryDockingEventsCount statusVariationAll
  , benchWithTmp "Station empty time (7001)" $ benchStationEmptyTime (Just 7001)
  , benchWithTmp "Station empty time (all)"  $ benchStationEmptyTime Nothing
  , benchWithTmp "Station information decoding" $ benchStationInformationDecoding "test/dumps/station_information_7001_2024-01-03_2024-01-04.json.zst"
  ]

selectWithPostgres :: FromBackendRow Postgres a => SqlSelect Postgres a -> AppM [a]
selectWithPostgres = withPostgres . runSelectReturningList

benchStationEmptyTime :: (MonadCatch m, HasEnv env m) => Maybe Int -> m [(StationInformationT Identity, (Maybe Int32, Maybe Int32))]
benchStationEmptyTime station =
  withPostgres . runSelectReturningList . select $ queryStationEmptyFullTime station
    (UTCTime (fromGregorian 2023 11 01) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2023 11 02) (timeOfDayToTime midnight))

benchStationInformationDecoding :: (HasEnv env m, MonadCatch m) => FilePath -> m [StationInformationT Identity]
benchStationInformationDecoding filePath = do
  contents <- liftIO . B.readFile $ filePath
  case Z.decompress contents of
    Z.Skip                    -> error $ "StationInformation: either frame was empty, or compression was done in streaming mode for path: "
    Z.Error   err             -> error err
    Z.Decompress decompressed -> do
      let info :: Either String [StationInformation] = eitherDecodeStrict decompressed
      case info of
        Left err    -> error err
        Right info' -> do
          let infoWithReported = map (\i -> (_infoReported i, fromBeamStationInformationToJSON i)) info'
          insertStationInformation infoWithReported
