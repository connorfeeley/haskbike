-- | Test the client functions.
module TestClient where

import           API.Client

import           AppEnv

import qualified CLI.Poll                  as Poll

import           Colog                     ( logInfo, pattern W )

import           Control.Exception         ( SomeException, try )
import           Control.Monad             ( void )

import           Data.Pool
import qualified Data.Text.Lazy            as TL
import           Data.Time                 ( getCurrentTimeZone )

import           Database.Beam.Postgres    ( close, connect )
import           Database.BikeShare
import           Database.BikeShare.Utils

import           Network.HTTP.Client       ( newManager )
import           Network.HTTP.Client.TLS   ( tlsManagerSettings )

import           Prelude                   hiding ( log, unwords )

import           Test.Tasty.HUnit

import           Text.Pretty.Simple.Extras

import           UnliftIO                  ( liftIO, timeout )


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

unit_poll :: IO ()
unit_poll = do
  timeZone <- getCurrentTimeZone

  -- Establish a connection to the database, drop all tables, and re-initialize it.
  -- Establish a connection to the database.
  connInfo <- mkDbConnectInfo dbnameTest
  pool <- newPool (defaultPoolConfig (connect connInfo) close 30 5)
  runWithAppM dbnameTest dropTables

  clientManager <- liftIO $ newManager tlsManagerSettings

  -- Create the application environment.
  let env = mainEnv W False False timeZone pool clientManager

  -- Log the database connection parameters.
  runAppM env $
    logInfo $ "Connected to database using: " <> (TL.toStrict . pShowCompact) connInfo
  runAppM env migrateDB
  runAppM env doPoll
  where
    doPoll :: AppM ()
    doPoll = void $ timeout 1000000 $ do -- Terminate after 1 second
      void Poll.pollClient
