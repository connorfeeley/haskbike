{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestClient where

import           API.Client
import qualified API.Poll           as Poll
import Database.Utils

import           Test.Tasty.HUnit

import           Control.Exception  (SomeException, try)
import           Control.Monad      (void)
import           System.IO          (stdout)
import           System.IO.Silently (hSilence, silence)
import           UnliftIO           (stderr, timeout)


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

unit_parseSystemRegions      :: IO ()
unit_parseSystemRegions      = void $ runQueryWithEnv systemRegions

unit_parseSystemPricingPlans :: IO ()
unit_parseSystemPricingPlans = void $ runQueryWithEnv systemPricingPlans

unit_poll :: IO ()
unit_poll = void $
  timeout 1000000 $ do
    conn <- setupDatabaseName dbnameTest
    hSilence [ {- stdout, stderr -} ] $ pure $ Poll.pollClient conn
