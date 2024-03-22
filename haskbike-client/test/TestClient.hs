-- | Test the client functions.
module TestClient where

import           Control.Exception   ( SomeException, try )
import           Control.Monad       ( void )

import           Data.Either         ( isRight )

import           Haskbike.API.Client

import           Prelude             hiding ( log, unwords )

import           Test.Tasty.HUnit


{-|
Check that the result of decoding the query response was successful.
This has the side-effect of forcing the evaluation of the response.
-}
checkResponse :: Either a b -> Assertion
checkResponse = assertBool "Response decoding failed" . isRight


-- | Mark a test as expected to fail.
markAsExpectedFailure :: IO () -> IO ()
markAsExpectedFailure testFunc = do
  result <- try testFunc :: IO (Either SomeException ())
  case result of
    Left _  -> return ()
    Right _ -> assertFailure "Expected failure, but test passed"


-- | Test the client functions.
unit_parseVersions, unit_parseVehicleTypes, unit_parseStationInformation, unit_parseStationStatus :: IO ()
unit_parseSystemInformation, unit_parseSystemRegions, unit_parseSystemPricingPlans :: IO ()
unit_parseVersions           = void . checkResponse =<< runQueryWithEnv versions
unit_parseVehicleTypes       = void . checkResponse =<< runQueryWithEnv vehicleTypes
unit_parseStationInformation = void . checkResponse =<< runQueryWithEnv stationInformation
unit_parseStationStatus      = void . checkResponse =<< runQueryWithEnv stationStatus
unit_parseSystemInformation  = void . checkResponse =<< runQueryWithEnv systemInformation
unit_parseSystemRegions      = void . checkResponse =<< runQueryWithEnv systemRegions
unit_parseSystemPricingPlans = void . checkResponse =<< runQueryWithEnv systemPricingPlans
