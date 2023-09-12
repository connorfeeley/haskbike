{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestClient where

import           Client

import           Control.Exception       (Exception (displayException),
                                          SomeException, try)
import           Test.Tasty.HUnit

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client
import Control.Monad (void)


-- | Mark a test as expected to fail.
markAsExpectedFailure :: IO () -> IO ()
markAsExpectedFailure testFunc = do
  result <- try testFunc :: IO (Either SomeException ())
  case result of
    Left _  -> return ()
    Right _ -> assertFailure "Expected failure, but test passed"

-- | Run a query and assert that it succeeds.
queryApi :: ClientM a -> IO a
queryApi query = do
  clientManager <- newManager tlsManagerSettings
  result  <- runClientM query (mkClientEnv clientManager clientBaseUrl)
  case result of
    -- Client parsed an error response
    Left err -> assertFailure $ "ErrorResponse: " ++ displayException err
    -- Client parsed a successful response
    Right resp  -> return resp
  where
  clientBaseUrl = BaseUrl Https "toronto.publicbikesystem.net" 443 "customer/gbfs/v2"

unit_parseVersions           :: IO ()
unit_parseVersions           = void $ queryApi versions

unit_parseVehicleTypes       :: IO ()
unit_parseVehicleTypes       = void $ queryApi vehicleTypes

unit_parseStationInformation :: IO ()
unit_parseStationInformation = void $ queryApi stationInformation

unit_parseStationStatus      :: IO ()
unit_parseStationStatus      = void $ queryApi stationStatus

unit_parseSystemRegions      :: IO ()
unit_parseSystemRegions      = void $ queryApi systemRegions

unit_parseSystemPricingPlans :: IO ()
unit_parseSystemPricingPlans = void $ queryApi systemPricingPlans
