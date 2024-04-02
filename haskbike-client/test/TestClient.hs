-- | Test the client functions.

module TestClient
     ( tests
     ) where

import           Control.Exception   ( SomeException, try )
import           Control.Monad       ( void )

import           Data.Either         ( isRight )

import           Haskbike.API.Client

import           Prelude             hiding ( log, unwords )

import           Test.Tasty
import           Test.Tasty.HUnit


{-|
Check that the result of decoding the query response was successful.
This has the side-effect of forcing the evaluation of the response.
-}
checkResponse :: Either a b -> Assertion
checkResponse = assertBool "Response decoding failed" . isRight


-- | Mark a test as expected to fail.
_markAsExpectedFailure :: IO () -> IO ()
_markAsExpectedFailure testFunc = do
  result <- try testFunc :: IO (Either SomeException ())
  case result of
    Left _  -> return ()
    Right _ -> assertFailure "Expected failure, but test passed"


tests :: TestTree
tests = testGroup "Client tests"
  [ parseVersions
  , parseVehicleTypes
  , parseStationInformation
  , parseStationStatus
  , parseSystemInformation
  , parseSystemRegions
  , parseSystemPricingPlans
  ]

-- | Test the client functions.
parseVersions           :: TestTree
parseVersions           = testCase "Parse versions"             (void . checkResponse =<< runQueryWithEnv versions)

parseVehicleTypes       :: TestTree
parseVehicleTypes       = testCase "Parse vehicle types"        (void . checkResponse =<< runQueryWithEnv vehicleTypes)

parseStationInformation :: TestTree
parseStationInformation = testCase "Parse station information"  (void . checkResponse =<< runQueryWithEnv stationInformation)

parseStationStatus      :: TestTree
parseStationStatus      = testCase "Parse station status"       (void . checkResponse =<< runQueryWithEnv stationStatus)

parseSystemInformation  :: TestTree
parseSystemInformation  = testCase "Parse system information"   (void . checkResponse =<< runQueryWithEnv systemInformation)

parseSystemRegions      :: TestTree
parseSystemRegions      = testCase "Parse system regions"       (void . checkResponse =<< runQueryWithEnv systemRegions)

parseSystemPricingPlans :: TestTree
parseSystemPricingPlans = testCase "Parse system pricing plans" (void . checkResponse =<< runQueryWithEnv systemPricingPlans)
