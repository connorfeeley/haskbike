-- | Benchmark suite.

module Main
     ( main
     ) where

import           BenchDatabase

import           Criterion.Main


main :: IO ()
main = defaultMain
  [ bgroup "Database operations"
    [ bench "Query bike chargings" $ whnfIO benchQueryChargings
    , bench "Station empty time (7001)" $ whnfIO $ benchStationEmptyTime (Just 7001)
    , bench "Station empty time (all)"  $ whnfIO $ benchStationEmptyTime Nothing
    ]
  ]
