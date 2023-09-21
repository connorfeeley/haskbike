-- | This module contains the BikeShare API client.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Client
     ( bikeShareAPIClient
     , handleResponse
     , mkClientManager
     , run
     , runQuery
     , runQueryWithEnv
     , stationInformation
     , stationStatus
     , systemInformation
     , systemPricingPlans
     , systemRegions
     , vehicleTypes
     , versions
     ) where

import           API.Types               ( StationInformationResponse, StationStatusResponse )

import           BikeShareAPI

import           Control.Exception       ( Exception (displayException) )

import           Data.Aeson              ( Object )
import           Data.Proxy

import           Network.HTTP.Client     ( Manager, newManager )
import           Network.HTTP.Client.TLS ( tlsManagerSettings )

import           Servant.API
import           Servant.Client

import           Text.Pretty.Simple      ( pPrintString )

-- | The BikeShare API client.
bikeShareAPIClient :: Proxy BikeShareAPI
bikeShareAPIClient = Proxy

-- | The BikeShare API client functions.
versions            :: ClientM Object
vehicleTypes        :: ClientM Object
stationInformation  :: ClientM StationInformationResponse
stationStatus       :: ClientM StationStatusResponse
systemRegions       :: ClientM Object
systemInformation   :: ClientM Object
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
  where
    bikeshareBaseUrl :: BaseUrl
    bikeshareBaseUrl = BaseUrl Https "toronto.publicbikesystem.net" 443 "customer/gbfs/v2"

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
