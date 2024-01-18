-- | Benchmark suite.

module Main where

import           AppEnv

import           Data.Time

import           Database.Beam
import           Database.BikeShare.Operations
import           Database.BikeShare.Operations.StationEmpty
import           Database.BikeShare.StatusVariationQuery
import           Database.BikeShare.Utils

import           Test.Tasty.Bench

import           TestChargings

import           Utils


fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

main :: IO ()
main = defaultMain
  [ -- bgroup "Fibonacci numbers"
    -- [ bench "fifth"     $ nf fibo  5
    -- , bench "tenth"     $ nf fibo 10
    -- , bench "twentieth" $ nf fibo 20
    -- ],
    bgroup "Database operations"
      [ -- bench "Query bike chargings" $ whnfIO unit_queryChargings
      -- ,
        bench "Station empty time" $ whnfIO benchStationEmptyTime
      ]
  ]

benchStationEmptyTime :: IO ()
benchStationEmptyTime = do
  _ <- runWithAppMDebug "haskbike" $ withPostgres $ runSelectReturningList $ selectWith $ queryStationEmptyTime (UTCTime (fromGregorian 2024 01 01) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2024 01 12) (timeOfDayToTime midnight))
  pure ()
