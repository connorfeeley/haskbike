-- | This module contains the BikeShare API client.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Data.Proxy
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           Text.Pretty.Simple      (pPrintString)

import           BikeShareAPI
import           Data.Aeson              (Object)

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

-- | A simple example of using the BikeShare API client.
run :: IO ()
run = do
    clientManager <- newManager tlsManagerSettings

    --
    -- Example usage of API client functions.
    --

    -- Get versions of the API.
    versionsRes <- runClientM versions (mkClientEnv clientManager bikeshareBaseUrl)
    case versionsRes of
        Left err           -> print err
        Right versionsList -> pPrintString $ "Versions" ++ show versionsList

    -- Get vehicle types.
    vehicleTypesRes <- runClientM vehicleTypes (mkClientEnv clientManager bikeshareBaseUrl)
    case vehicleTypesRes of
        Left err -> print err
        Right vehicleTypesList -> pPrintString $ "Vehicle Types" ++ show vehicleTypesList

    -- Get station information.
    stationInformationRes <- runClientM stationInformation (mkClientEnv clientManager bikeshareBaseUrl)
    case stationInformationRes of
        Left err -> print err
        Right stationInformationList -> pPrintString $ "Station Information" ++ show stationInformationList

    -- Get station status.
    stationStatusRes <- runClientM stationStatus (mkClientEnv clientManager bikeshareBaseUrl)
    case stationStatusRes of
        Left err -> print err
        Right stationStatusList -> pPrintString $ "Station Status" ++ show stationStatusList

    -- Get system regions.
    systemRegionsRes <- runClientM systemRegions (mkClientEnv clientManager bikeshareBaseUrl)
    case systemRegionsRes of
        Left err -> print err
        Right systemRegionsList -> pPrintString $ "System Regions" ++ show systemRegionsList

    -- Get system information.
    systemInformationRes <- runClientM systemInformation (mkClientEnv clientManager bikeshareBaseUrl)
    case systemInformationRes of
        Left err -> print err
        Right systemInformationList -> pPrintString $ "System Information" ++ show systemInformationList

    -- Get system pricing plans.
    systemPricingPlansRes <- runClientM systemPricingPlans (mkClientEnv clientManager bikeshareBaseUrl)
    case systemPricingPlansRes of
        Left err -> print err
        Right systemPricingPlansList -> pPrintString $ "System Pricing Plans" ++ show systemPricingPlansList

  where
    bikeshareBaseUrl :: BaseUrl
    bikeshareBaseUrl = BaseUrl Https "toronto.publicbikesystem.net" 443 "customer/gbfs/v2"
