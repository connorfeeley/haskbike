-- | This module contains the BikeShare API client.

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}

module API.DebugClient
     ( bikeShareAPIClient
       -- , handleResponse
       -- , mkClientManager
       -- , run
       -- , runQuery
       -- , runQueryWithEnv
     , stationInformation
     , stationStatus
     , systemInformation
     , systemPricingPlans
     , systemRegions
     , vehicleTypes
     , versions
     ) where

import           API.Types

import           BikeShareAPI

import           Control.Monad.Free

import           Data.Aeson                         ( Object )

import qualified Network.HTTP.Client                as HTTP
import           Network.HTTP.Client.TLS            ( tlsManagerSettings )

import           Servant
import           Servant.Client.Free
import qualified Servant.Client.Internal.HttpClient as I




-- | The BikeShare API client.
bikeShareAPIClient :: Proxy BikeShareAPI
bikeShareAPIClient = Proxy

-- | The BikeShare API client functions.
versions            :: Free ClientF Object
vehicleTypes        :: Free ClientF Object
stationInformation  :: Free ClientF (ResponseWrapper [StationInformation])
stationStatus       :: Free ClientF StationStatusResponse
systemRegions       :: Free ClientF Object
systemInformation   :: Free ClientF Object
systemPricingPlans  :: Free ClientF Object
( versions
  :<|> vehicleTypes
  :<|> stationInformation
  :<|> stationStatus
  :<|> systemRegions
  :<|> systemInformation
  :<|> systemPricingPlans
  ) = client bikeShareAPIClient

-- mkClientManager :: IO HTTP.Manager
-- mkClientManager = HTTP.newManager tlsManagerSettings

-- runQuery :: HTTP.Manager -> Free ClientF a -> IO (Either ClientError a)
-- runQuery clientManager endpoint = runClientF endpoint (mkClientEnv clientManager bikeshareBaseUrl)
--   where
--     bikeshareBaseUrl :: BaseUrl
--     bikeshareBaseUrl = BaseUrl Https "toronto.publicbikesystem.net" 443 "customer/gbfs/v2"

-- runQueryWithEnv :: Free ClientF a -> IO (Either ClientError a)
-- runQueryWithEnv query = do
--   clientManager <- mkClientManager
--   runQuery clientManager query

-- handleResponse :: (Exception e, Show a) => String -> Either e a -> IO ()
-- handleResponse name response = case response of
--     Left err -> print $ displayException err
--     Right a  -> pPrintString $ name ++ show a

-- -- | A simple example of using the BikeShare API client.
-- run :: IO ()
-- run = do
--     clientManager <- mkClientManager

--     --
--     -- Example usage of API client functions.
--     --
--     runQuery clientManager versions           >>= handleResponse "Versions"
--     runQuery clientManager vehicleTypes       >>= handleResponse "Vehicle Types"
--     runQuery clientManager stationInformation >>= handleResponse "Station Information"
--     runQuery clientManager stationStatus      >>= handleResponse "Station Status"
--     runQuery clientManager systemRegions      >>= handleResponse "System Regions"
--     runQuery clientManager systemInformation  >>= handleResponse "System Information"
--     runQuery clientManager systemPricingPlans >>= handleResponse "System Pricing Plans"

_test :: IO ()
_test = case systemInformation of
    Pure n ->
        putStrLn $ "ERROR: got pure result: " ++ show n
    Free (Throw err) ->
        putStrLn $ "ERROR: got error right away: " ++ show err
    Free (RunRequest req k) -> do
        mgr <- HTTP.newManager tlsManagerSettings
        let req' = I.defaultMakeClientRequest bikeshareBaseUrl req
        putStrLn $ "Making request: " ++ show req'
        res' <- HTTP.httpLbs req' mgr
        putStrLn $ "Got response: " ++ show res'
        let res = I.clientResponseToResponse id res'

        case k res of
            Pure n ->
                putStrLn $ "Expected 1764, got " ++ show n
            _ ->
                putStrLn "ERROR: didn't get a response"

bikeshareBaseUrl :: BaseUrl
bikeshareBaseUrl = BaseUrl Https "toronto.publicbikesystem.net" 443 "customer/gbfs/v2"
