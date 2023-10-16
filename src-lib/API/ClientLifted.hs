-- | This module provides a lifted version of the API.Client module.

module API.ClientLifted
     ( liftClientM
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

import           Colog                ( logException )

import           Control.Monad.Catch
import           Control.Monad.Reader ( ask )

import           Data.Aeson           ( Object )

import           Prelude              hiding ( log )

import           Servant.Client



liftClientM :: ClientM a -> App a
liftClientM clientM = do
  env <- ask
  let manager = envClientManager env
  eitherResult <- liftIO $ runClientM clientM (mkClientEnv manager (envBaseUrl env))
  case eitherResult of
    Left err -> do
      logException err
      throwM err
    Right res -> return res


versionsM:: App Object
versionsM = liftClientM versions

vehicleTypesM :: App Object
vehicleTypesM = liftClientM vehicleTypes

stationInformationM :: App StationInformationResponse
stationInformationM = liftClientM stationInformation

stationStatusM :: App StationStatusResponse
stationStatusM = liftClientM stationStatus

systemRegionsM :: App Object
systemRegionsM = liftClientM systemRegions

systemInformationM :: App Object
systemInformationM = liftClientM systemInformation

systemPricingPlansM :: App Object
systemPricingPlansM = liftClientM systemPricingPlans
