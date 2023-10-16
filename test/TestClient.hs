-- | Test the client functions.
module TestClient where

import           API.Client

import           AppEnv

import qualified CLI.Poll                 as Poll

import           Colog                    ( log, pattern I, pattern W )

import           Control.Exception        ( SomeException, try )
import           Control.Monad            ( void )

import           Data.Text                ( pack, unwords )
import           Data.Time                ( getCurrentTimeZone )

import           Database.BikeShare.Utils

import           Prelude                  hiding ( log, unwords )

import           Test.Tasty.HUnit

import           UnliftIO                 ( timeout )


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

  -- Establish a connection to the database, drop all tables, and re-initialize it.
  (name, host, port, username, password) <- pure (dbnameTest, "", "", "", "")
  conn <- connectDbName name host port username password >>= dropTables >>= migrateDatabase

  -- Create the application environment.
  let env = mainEnv W False False timeZone conn

  -- Log the database connection parameters.
  runAppM env (log I $ "Connected to database using: " <> unwords [ "dbname=" <> pack name
                                                                  , pack host
                                                                  , pack port
                                                                  , pack username
                                                                  , pack "password=***" -- Don't log the password.
                                                                  ])
  runAppM env doPoll
  where
    doPoll :: AppM ()
    doPoll = void $ timeout 1000000 $ do -- Terminate after 1 second
      void Poll.pollClient
