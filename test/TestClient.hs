{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestClient where

import           Control.Exception       (Exception (displayException),
                                          SomeException, try)
import           Test.Tasty.HUnit

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
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

unit_parseVersions            :: IO ()
unit_parseVersions            = runQuery versions

unit_parseVehicleTypes        :: IO ()
unit_parseVehicleTypes        = runQuery vehicleTypes

unit_parseStationInformation  :: IO ()
unit_parseStationInformation  = runQuery stationInformation

unit_parseStationStatus       :: IO ()
unit_parseStationStatus       = runQuery stationStatus

unit_parseSystemRegions       :: IO ()
unit_parseSystemRegions       = runQuery systemRegions

unit_parseSystemPricingPlans  :: IO ()
unit_parseSystemPricingPlans  = runQuery systemPricingPlans
