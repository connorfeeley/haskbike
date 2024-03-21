-- | Test the client functions.
module TestClient where

import           Control.Exception   ( SomeException, try )
import           Control.Monad       ( void )

import           Haskbike.API.Client

import           Prelude             hiding ( log, unwords )

import           Test.Tasty.HUnit


-- | Mark a test as expected to fail.
markAsExpectedFailure :: IO () -> IO ()
markAsExpectedFailure testFunc = do
  result <- try testFunc :: IO (Either SomeException ())
  case result of
    Left _  -> return ()
    Right _ -> assertFailure "Expected failure, but test passed"

unit_parseVersions           :: IO ()
unit_parseVersions           = void $ runQueryWithEnv versions

unit_parseVehicleTypes       :: IO ()
unit_parseVehicleTypes       = void $ runQueryWithEnv vehicleTypes

unit_parseStationInformation :: IO ()
unit_parseStationInformation = void $ runQueryWithEnv stationInformation

unit_parseStationStatus      :: IO ()
unit_parseStationStatus      = void $ runQueryWithEnv stationStatus

unit_parseSystemInformation  :: IO ()
unit_parseSystemInformation  = void $ runQueryWithEnv systemInformation

unit_parseSystemRegions      :: IO ()
unit_parseSystemRegions      = void $ runQueryWithEnv systemRegions

unit_parseSystemPricingPlans :: IO ()
unit_parseSystemPricingPlans = void $ runQueryWithEnv systemPricingPlans
