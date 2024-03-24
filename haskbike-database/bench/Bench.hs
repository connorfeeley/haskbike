-- | Benchmark suite.

module Main
     ( main
     ) where

import           BenchDatabase

import           Test.Tasty.Bench


main :: IO ()
main = defaultMain
  [ bgroupDatabase
  ]
