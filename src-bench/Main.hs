-- | Benchmark suite.

module Main where

import           AppEnv

import           BenchServer

import           Control.Monad                              ( void )

import           Data.Time

import           Database.Beam
import           Database.BikeShare.Operations
import           Database.BikeShare.Operations.StationEmpty
import           Database.BikeShare.StatusVariationQuery
import           Database.BikeShare.Utils

import           Server.VisualizationAPI

import           Test.Tasty.Bench

import           TestChargings

import           Utils


fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

main :: IO ()
main = defaultMain
  [ bgroup "Database operations"
    [ bench "Query bike chargings" $ whnfIO unit_queryChargings
    , bench "Station empty time (7001)" $ whnfIO $ benchStationEmptyTime (Just 7001)
    , bench "Station empty time (all)"  $ whnfIO $ benchStationEmptyTime Nothing
    ]
  ]
