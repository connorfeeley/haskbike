{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestClient where

import           Control.Exception              (Exception (displayException),
                                                 SomeException, try)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertFailure)

import           Network.HTTP.Client            (newManager)
import           Network.HTTP.Client.TLS        (tlsManagerSettings)
import           Servant.Client

import           Client

-- | Mark a test as expected to fail.
markAsExpectedFailure :: IO () -> IO ()
markAsExpectedFailure testFunc = do
  result <- try testFunc :: IO (Either SomeException ())
  case result of
    Left _  -> return ()
    Right _ -> assertFailure "Expected failure, but test passed"

-- | Run a query and assert that it succeeds.
runQuery :: ClientM a -> IO ()
runQuery query = do
  clientManager <- newManager tlsManagerSettings
  result  <- runClientM query (mkClientEnv clientManager clientBaseUrl)
  case result of
    -- Client parsed an error response
    Left err -> assertFailure $ "ErrorResponse: " ++ displayException err
    -- Client parsed a successful response
    Right _  -> return ()
  where
  clientBaseUrl = BaseUrl Https "toronto.publicbikesystem.net" 443 "customer/gbfs/v2"

test_Client :: Test
test_Client = testGroup "Client tests"
  [ testCase "parse versions" test_parseVersions
  , testCase "parse vehicle types" test_parseVehicleTypes
  , testCase "parse station information" test_parseStationInformation
  , testCase "parse station status" test_parseStationStatus
  , testCase "parse system regions" test_parseSystemRegions
  , testCase "parse system pricing plans" test_parseSystemPricingPlans
  ]

test_parseVersions            :: IO ()
test_parseVersions            = runQuery versions

test_parseVehicleTypes        :: IO ()
test_parseVehicleTypes        = runQuery vehicleTypes

test_parseStationInformation  :: IO ()
test_parseStationInformation  = runQuery stationInformation

test_parseStationStatus       :: IO ()
test_parseStationStatus       = runQuery stationStatus

test_parseSystemRegions       :: IO ()
test_parseSystemRegions       = runQuery systemRegions

test_parseSystemPricingPlans  :: IO ()
test_parseSystemPricingPlans  = runQuery systemPricingPlans
