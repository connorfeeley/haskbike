-- | Main entry point to the test suite.

module Main where

import           Test.Framework (defaultMain)

import qualified TestClient
import qualified TestDatabase
import qualified TestDecoding

main :: IO ()
main = defaultMain $ concat [ TestClient.tests
                            , TestDecoding.tests
                            , TestDatabase.tests
                            ]
