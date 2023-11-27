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

import           API.Client
import           API.Types

import           AppEnv

import           Colog                ( WithLog, logException )

import           Control.Monad.Catch
import           Control.Monad.Reader ( MonadReader (..) )

import           Data.Aeson           ( Object )

import           Prelude              hiding ( log )

import           Servant.Client

import           UnliftIO


runQueryM :: (WithLog env Message m, MonadIO m, MonadUnliftIO m, MonadReader (Env m) m) => ClientM a -> m (Either ClientError a)
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

stationInformationM :: AppM StationInformationResponse
stationInformationM = liftClientM stationInformation

stationStatusM :: AppM StationStatusResponse
stationStatusM = liftClientM stationStatus

systemRegionsM :: AppM Object
systemRegionsM = liftClientM systemRegions

systemInformationM :: AppM Object
systemInformationM = liftClientM systemInformation

systemPricingPlansM :: AppM Object
systemPricingPlansM = liftClientM systemPricingPlans
