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

import           API.APIVersion         ( APIVersion )
import           API.BikeShare
import           API.Client
import           API.ResponseWrapper
import           API.StationInformation ( StationInformation )
import           API.StationStatus      ( StationStatus )
import           API.SystemInformation  ( SystemInformation )
import           API.SystemPricingPlan  ( SystemPricingPlan )
import           API.SystemRegion       ( SystemRegion )
import           API.VehicleTypeFull    ( VehicleTypeFull )

import           AppEnv

import           Colog                  ( logException )

import           Control.Monad.Catch

import           Prelude                hiding ( log )

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
