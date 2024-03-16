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

import           Colog                  ( HasLog, logException )

import           Control.Monad.Catch
import           Control.Monad.Reader   ( MonadReader )

import           Data.Aeson             ( Object )

import           Prelude                hiding ( log )

import           Servant.Client

import           UnliftIO


runQueryM :: (WithAppMEnv (Env env) Message m)
          => ClientM a
          -> m (Either ClientError a)
runQueryM query = do
  env <- ask
  let clientManager = envClientManager env
  liftIO $ runClientM query (mkClientEnv clientManager bikeshareBaseUrl)


liftClientM :: (MonadReader (Env e) m, MonadIO m, HasLog (Env e) Message m, MonadThrow m)
            => ClientM b -> m b
liftClientM clientM = do
  env <- ask
  let manager = envClientManager env
  eitherResult <- liftIO $ runClientM clientM (mkClientEnv manager (envBaseUrl env))
  case eitherResult of
    Left err -> do
      logException err
      throwM err
    Right res -> return res


versionsM :: (WithAppMEnv (Env env) Message m) => m Object
versionsM = liftClientM versions

vehicleTypesM :: (WithAppMEnv (Env env) Message m) => m Object
vehicleTypesM = liftClientM vehicleTypes

stationInformationM :: (WithAppMEnv (Env env) Message m) => m (ResponseWrapper [StationInformation])
stationInformationM = liftClientM stationInformation

stationStatusM :: (WithAppMEnv (Env env) Message m) => m (ResponseWrapper [StationStatus])
stationStatusM = liftClientM stationStatus

systemRegionsM :: (WithAppMEnv (Env env) Message m) => m Object
systemRegionsM = liftClientM systemRegions

systemInformationM :: (WithAppMEnv (Env env) Message m) => m (ResponseWrapper SystemInformation)
systemInformationM = liftClientM systemInformation

systemPricingPlansM :: (WithAppMEnv (Env env) Message m) => m Object
systemPricingPlansM = liftClientM systemPricingPlans
