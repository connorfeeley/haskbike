{-# LANGUAGE DataKinds #-}

-- | This module contains the BikeShare API client.

module Haskbike.API.Client
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

import           Control.Monad.Catch             ( MonadCatch, MonadThrow )

import           Data.Aeson                      ( Object )
import           Data.Proxy

import           Haskbike.API.APIVersion
import           Haskbike.API.BikeShare
import           Haskbike.API.BikeShareAPI
import           Haskbike.API.ResponseWrapper
import           Haskbike.API.StationInformation
import           Haskbike.API.StationStatus
import           Haskbike.API.SystemInformation
import           Haskbike.API.SystemPricingPlan
import           Haskbike.API.SystemRegion
import           Haskbike.API.VehicleTypeFull
import           Haskbike.AppEnv

import           Network.HTTP.Client             ( Manager, newManager )
import           Network.HTTP.Client.TLS         ( tlsManagerSettings )

import           Servant.API
import           Servant.Client

import           Text.Pretty.Simple              ( pPrintString )

import           UnliftIO

-- | The BikeShare API client.
bikeShareAPIClient :: Proxy BikeShareAPI
bikeShareAPIClient = Proxy

-- | The BikeShare API client functions.
versions            :: ClientM (ResponseWrapper [APIVersion])
vehicleTypes        :: ClientM (ResponseWrapper [VehicleTypeFull])
stationInformation  :: ClientM (ResponseWrapper [StationInformation])
stationStatus       :: ClientM (ResponseWrapper [StationStatus])
systemRegions       :: ClientM (ResponseWrapper [SystemRegion])
systemInformation   :: ClientM (ResponseWrapper SystemInformation)
systemPricingPlans  :: ClientM (ResponseWrapper [SystemPricingPlan])
(      versions
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
runQueryWithManager :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m) => ClientM a -> m (Either ClientError a)
runQueryWithManager query = do
  clientManager <- withManager
  liftIO $ runQuery clientManager query
