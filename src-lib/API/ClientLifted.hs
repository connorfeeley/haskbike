-- | This module provides a lifted version of the API.Client module.

module API.ClientLifted
     ( liftClientM
     , runQueryM
       -- Lifted API client functions
     , stationInformationM
     , stationStatusM
     , systemInformationM
     , systemPricingPlansM
     , systemRegionsM
     , vehicleTypesM
     , versionsM
     ) where

import           API.BikeShare
import           API.Client
import           API.ResponseWrapper
import           API.StationInformation
import           API.StationStatus
import           API.SystemInformation

import           AppEnv

import           Colog                  ( logException )

import           Control.Monad.Catch

import           Data.Aeson             ( Object )

import           Prelude                hiding ( log )

import           Servant.Client

import           UnliftIO


runQueryM :: (m ~ AppM) => ClientM a -> m (Either ClientError a)
runQueryM query = do
  env <- ask
  let clientManager = envClientManager env
  liftIO $ runClientM query (mkClientEnv clientManager bikeshareBaseUrl)


liftClientM :: ClientM a -> AppM a
liftClientM clientM = do
  env <- ask
  let manager = envClientManager env
  eitherResult <- liftIO $ runClientM clientM (mkClientEnv manager (envBaseUrl env))
  case eitherResult of
    Left err -> do
      logException err
      throwM err
    Right res -> return res


versionsM:: AppM Object
versionsM = liftClientM versions

vehicleTypesM :: AppM Object
vehicleTypesM = liftClientM vehicleTypes

stationInformationM :: AppM (ResponseWrapper [StationInformation])
stationInformationM = liftClientM stationInformation

stationStatusM :: AppM (ResponseWrapper [StationStatus])
stationStatusM = liftClientM stationStatus

systemRegionsM :: AppM Object
systemRegionsM = liftClientM systemRegions

systemInformationM :: AppM (ResponseWrapper SystemInformation)
systemInformationM = liftClientM systemInformation

systemPricingPlansM :: AppM Object
systemPricingPlansM = liftClientM systemPricingPlans
