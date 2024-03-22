-- | Benchmark suite.

module Main where

import           BenchDatabase

import           Control.Monad                             ( void )

import           Data.Time

import           Database.Beam

import           Haskbike.AppEnv
import           Haskbike.Database.Operations
import           Haskbike.Database.Operations.StationEmpty
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Utils

import           Test.Tasty.Bench


main :: IO ()
main = defaultMain
  [ bgroup "Database operations"
    [ bench "Query bike chargings" $ whnfIO benchQueryChargings
    , bench "Station empty time (7001)" $ whnfIO $ benchStationEmptyTime (Just 7001)
    , bench "Station empty time (all)"  $ whnfIO $ benchStationEmptyTime Nothing
    ]
  ]
