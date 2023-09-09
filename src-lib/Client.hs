-- | This module contains the BikeShare API client.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Client
  ( bikeShareAPIClient
  , versions
  , vehicleTypes
  , stationInformation
  , stationStatus
  , systemRegions
  , systemInformation
  , systemPricingPlans
  , run
  ) where

import           Data.Proxy
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           Text.Pretty.Simple      (pPrintString)

import           BikeShareAPI
import           Data.Aeson              (Object)

import           Control.Exception       (Exception (displayException))
import qualified StationInformation      as SI
import qualified StationStatus           as SS

-- | The BikeShare API client.
bikeShareAPIClient :: Proxy BikeShareAPI
bikeShareAPIClient = Proxy

-- | The BikeShare API client functions.
versions            :: ClientM Object
vehicleTypes        :: ClientM Object
stationInformation  :: ClientM SI.StationInformationResponse
stationStatus       :: ClientM SS.StationStatusResponse
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

runQuery :: Manager-> ClientM a -> IO (Either ClientError a)
runQuery clientManager endpoint = runClientM endpoint (mkClientEnv clientManager bikeshareBaseUrl)
  where
    bikeshareBaseUrl :: BaseUrl
    bikeshareBaseUrl = BaseUrl Https "toronto.publicbikesystem.net" 443 "customer/gbfs/v2"

handleResponse :: (Exception e, Show a) => String -> Either e a -> IO ()
handleResponse name response = case response of
    Left err -> print $ displayException err
    Right a  -> pPrintString $ name ++ show a

-- | A simple example of using the BikeShare API client.
run :: IO ()
run = do
    clientManager <- newManager tlsManagerSettings

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
