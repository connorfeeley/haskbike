-- | This module contains the BikeShare API client.

{-# LANGUAGE DataKinds #-}

module API.Client
     ( bikeShareAPIClient
     , handleResponse
     , mkClientManager
     , run
     , runQuery
     , runQueryWithEnv
     , runQueryWithManager
     , stationInformation
     , stationStatus
     , systemInformation
     , systemPricingPlans
     , systemRegions
     , vehicleTypes
     , versions
     ) where

import           API.BikeShare
import           API.Types

import           AppEnv

import           BikeShareAPI

import           Data.Aeson              ( Object )
import           Data.Proxy

import           Network.HTTP.Client     ( Manager, newManager )
import           Network.HTTP.Client.TLS ( tlsManagerSettings )

import           Servant.API
import           Servant.Client

import           Text.Pretty.Simple      ( pPrintString )

import           UnliftIO

-- | The BikeShare API client.
bikeShareAPIClient :: Proxy BikeShareAPI
bikeShareAPIClient = Proxy

-- | The BikeShare API client functions.
versions            :: ClientM Object
vehicleTypes        :: ClientM Object
stationInformation  :: ClientM (ResponseWrapper [StationInformation])
stationStatus       :: ClientM (ResponseWrapper [StationStatus])
systemRegions       :: ClientM Object
systemInformation   :: ClientM (ResponseWrapper SystemInformation)
systemPricingPlans  :: ClientM Object
( versions
  :<|> vehicleTypes
  :<|> stationInformation
  :<|> stationStatus
  :<|> systemRegions
  :<|> systemInformation
  :<|> systemPricingPlans
  ) = client bikeShareAPIClient

mkClientManager :: IO Manager
mkClientManager = newManager tlsManagerSettings

runQuery :: Manager -> ClientM a -> IO (Either ClientError a)
runQuery clientManager endpoint = runClientM endpoint (mkClientEnv clientManager bikeshareBaseUrl)

runQueryWithEnv :: ClientM a -> IO (Either ClientError a)
runQueryWithEnv query = do
  clientManager <- mkClientManager
  runQuery clientManager query

handleResponse :: (Exception e, Show a) => String -> Either e a -> IO ()
handleResponse name response = case response of
    Left err -> print $ displayException err
    Right a  -> pPrintString $ name ++ show a

-- | A simple example of using the BikeShare API client.
run :: IO ()
run = do
    clientManager <- mkClientManager

    --
    -- Example usage of API client functions.
    --
    runQuery clientManager versions           >>= handleResponse "Versions"
    runQuery clientManager vehicleTypes       >>= handleResponse "Vehicle Types"
    runQuery clientManager stationInformation >>= handleResponse "Station Information"
    runQuery clientManager stationStatus      >>= handleResponse "Station Status"
    runQuery clientManager systemRegions      >>= handleResponse "System Regions"
    runQuery clientManager systemInformation  >>= handleResponse "System Information"
    runQuery clientManager systemPricingPlans >>= handleResponse "System Pricing Plans"

-- | Run API query using client manager from environment monad.
runQueryWithManager :: WithAppMEnv (Env env) Message m => ClientM a -> m (Either ClientError a)
runQueryWithManager query = do
  clientManager <- withManager
  liftIO $ runQuery clientManager query
