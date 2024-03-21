-- | This module provides a lifted version of the Haskbike.Haskbike.API.Client module.

module Haskbike.API.ClientLifted
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

import           Colog                           ( logException )

import           Control.Monad.Catch             ( MonadCatch, MonadThrow, throwM )

import           Data.Aeson                      ( Object )

import           Haskbike.API.APIVersion
import           Haskbike.API.BikeShare
import           Haskbike.API.BikeShareAPI
import           Haskbike.API.Client
import           Haskbike.API.ResponseWrapper
import           Haskbike.API.StationInformation
import           Haskbike.API.StationStatus
import           Haskbike.API.SystemInformation
import           Haskbike.API.SystemPricingPlan
import           Haskbike.API.SystemRegion
import           Haskbike.API.VehicleTypeFull
import           Haskbike.AppEnv

import           Prelude                         hiding ( log )

import           Servant.Client

import           UnliftIO


runQueryM :: (HasEnv env m, MonadIO m, MonadThrow m) => ClientM a -> m (Either ClientError a)
runQueryM query = do
  clientManager <- getClientManager
  liftIO $ runClientM query (mkClientEnv clientManager bikeshareBaseUrl)


liftClientM :: (HasEnv env m, MonadIO m, MonadThrow m) => ClientM a -> m a
liftClientM clientM = do
  baseUrl <- getBaseUrl
  manager <- getClientManager
  eitherResult <- liftIO $ runClientM clientM (mkClientEnv manager baseUrl)
  case eitherResult of
    Left err -> do
      logException err
      throwM err
    Right res -> return res


versionsM :: (HasEnv env m, MonadIO m, MonadThrow m)
          => m (ResponseWrapper [APIVersion])
versionsM = liftClientM versions

vehicleTypesM :: (HasEnv env m, MonadIO m, MonadThrow m)
              => m (ResponseWrapper [VehicleTypeFull])
vehicleTypesM = liftClientM vehicleTypes

stationInformationM :: (HasEnv env m, MonadIO m, MonadThrow m)
                    => m (ResponseWrapper [StationInformation])
stationInformationM = liftClientM stationInformation

stationStatusM :: (HasEnv env m, MonadIO m, MonadThrow m)
               => m (ResponseWrapper [StationStatus])
stationStatusM = liftClientM stationStatus

systemRegionsM :: (HasEnv env m, MonadIO m, MonadThrow m)
               => m (ResponseWrapper [SystemRegion])
systemRegionsM = liftClientM systemRegions

systemInformationM :: (HasEnv env m, MonadIO m, MonadThrow m)
                   => m (ResponseWrapper SystemInformation)
systemInformationM = liftClientM systemInformation

systemPricingPlansM :: (HasEnv env m, MonadIO m, MonadThrow m)
                    => m (ResponseWrapper [SystemPricingPlan])
systemPricingPlansM = liftClientM systemPricingPlans
