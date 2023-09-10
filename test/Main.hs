-- | Main entry point to the test suite.

module Main where

import           Test.Framework (defaultMain)

import qualified TestClient
import qualified TestDecoding
import qualified TestDatabase

main :: IO ()
main = defaultMain
    [ TestDecoding.test_Decoding    -- Decoding HUnit tests
    , TestClient.test_Client        -- Client HUnit tests
    , TestDatabase.test_Database    -- Database HUnit tests
    ]
