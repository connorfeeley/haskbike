-- | Mock server for the PBSC bikeshare API.

module Haskbike.API.MockServer
     ( runMockServer
     ) where

import           Colog

import           Control.Monad.Catch                  ( MonadCatch, throwM )
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Function                        ( (&) )
import           Data.Proxy                           ( Proxy (..) )
import qualified Data.Text                            as T
import           Data.Time

import           Haskbike.API.APIVersion
import           Haskbike.API.BikeShareAPI
import           Haskbike.API.ResponseWrapper
import           Haskbike.API.SystemInformation
import           Haskbike.API.VehicleTypeFull
import           Haskbike.AppEnv

import           Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.Gzip          ( GzipFiles (..), GzipSettings (..), defaultGzipSettings, gzip )
import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )

import qualified Servant                              as S
import           Servant
import qualified Servant.Server.Generic               as S
import           Servant.Server.Generic

import           UnliftIO


-- | Mock server handlers.
mockServer :: ( WithEnv (Env m) m, WithEnv (Env m) m )
           => BikeShareAPIRoutes (AsServerT m)
mockServer = BikeShareAPIRoutes { _versions           = mkResponseWrapper mockVersions
                                , _vehicleTypes       = mkResponseWrapper mockVehicleTypes
                                , _stationInformation = mkResponseWrapper []
                                , _stationStatus      = mkResponseWrapper []
                                , _systemRegions      = mkResponseWrapper []
                                , _systemInformation  = mkResponseWrapper mockSysInf
                                , _systemPricingPlans = mkResponseWrapper []
                                }

-- | Run the mock server.
runMockServer :: (HasEnv (Env AppM) m, MonadCatch m, MonadUnliftIO m, MonadFail m)
              => Int -> m ()
runMockServer port = do
  env <- ask
  liftIO $ runSettings serverSettings (logStdoutDev (serverApp env))
  where
    apiProxy :: Proxy BikeShareAPI
    apiProxy = Proxy

    serverSettings :: Settings
    serverSettings = defaultSettings
                  & setPort port
                  & setTimeout 5

-- | Natural transformation between server monad and Servant 'Handler' monad.
serverNt :: Env AppM -> AppM a -> S.Handler a
serverNt env action =
  liftIO $
    runReaderT (unAppM action) env `catch` exceptionHandler
  where
    exceptionHandler :: SomeException -> IO a
    exceptionHandler ex = throwM (servantErrFromEx ex)

-- | Error handler.
servantErrFromEx :: SomeException -> S.ServerError
servantErrFromEx _ex = S.err500 { S.errBody = "Internal server error" }

-- | Generic server representation.
serverApp :: Env AppM -> S.Application
serverApp state = S.genericServeT (serverNt state) mockServer


-- * Mock data.

-- | Create a 'ResponseWrapper' with some defaults.
mkResponseWrapper dat = do
  ct <- liftIO getCurrentTime
  pure $
    ResponseWrapper { _respLastUpdated = ct
                    , _respTtl         = 35
                    , _respVer         = "2.3"
                    , _respData        = dat
                    }

-- | Mock response for 'APIVersion' endpoint.
mockVersions = [ APIVersion { _apiVersion = 2.3, _apiUrl = "localhost" } ]

mockVehicleTypes =
  [ VehicleTypeFull Iconic "bicycle"           "human"     0.0 "ICONIC"  "186-1"
  , VehicleTypeFull Fit    "bicycle"           "human"     0.0 "FIT"     "186-1"
  , VehicleTypeFull Boost  "bicycle" "electric_assist"     0.0 "BOOST"   "186-2"
  , VehicleTypeFull EFit   "bicycle" "electric_assist"     0.0 "EFIT"    "186-2"
  , VehicleTypeFull EFitG5 "bicycle" "electric_assist" 60000.0 "EFIT G5" "186-2"
  ]

-- | Mock response for 'SystemInformation' endpoint.
mockSysInf =
  SystemInformation
  { _sysInfStationCount = 792
  , _sysInfVehicleCount = SystemInformationVehicleCount
                          { sysInfMechanicalCount = 7907
                          , sysInfEbikeCount      =  887
                          }
  , _sysInfBuildHash            = "432ff52"
  , _sysInfBuildLabel           = "2024-01-29"
  , _sysInfBuildNumber          = "40"
  , _sysInfBuildVersion         = "2023.2"
  , _sysInfLanguage             = "en"
  , _sysInfMobileHeadVersion    = 2
  , _sysInfMobileMinSuppVersion = 1
  , _sysInfName                 = "bike_share_toronto"
  , _sysInfSysId                = "bike_share_toronto"
  , _sysInfTimeZone             = "America/Toronto"
  }
