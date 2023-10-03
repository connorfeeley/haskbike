-- | Test the client functions.

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestClient where

import           API.Client

import           AppEnv

import qualified CLI.Poll          as Poll

import           Colog             ( Message, WithLog, pattern W )

import           Control.Exception ( SomeException, try )
import           Control.Monad     ( void )

import           Data.Time         ( getCurrentTimeZone )

import           Database.Utils

import           Test.Tasty.HUnit

import           UnliftIO          ( MonadIO, MonadUnliftIO, liftIO, timeout )

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
unit_poll = do
  timeZone <- getCurrentTimeZone
  runApp (mainEnv W timeZone) doPoll
  where
    doPoll :: (App ~ m, WithLog env Message m, MonadIO m, MonadUnliftIO m) => m ()
    doPoll = void $ timeout 1000000 $ do -- Terminate after 1 second
      conn <- liftIO $ setupDatabaseName dbnameTest
      void $ Poll.pollClient conn
