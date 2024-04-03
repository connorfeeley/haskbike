{-# LANGUAGE RecordWildCards #-}

-- | Mock server for the PBSC bikeshare API.

module Haskbike.API.MockServer
     ( MockServerLog (..)
     , mockServerBaseUrl
     , runMockQuery
     , runMockQueryWithEnv
     , runMockServer
     ) where

import           Colog

import           Control.Monad.Catch                  ( MonadCatch, throwM )
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Function                        ( (&) )
import qualified Data.Map                             as Map
import           Data.Proxy                           ( Proxy (..) )
import qualified Data.Text                            as T
import           Data.Time

import           Haskbike.API.APIVersion
import           Haskbike.API.BikeShareAPI
import           Haskbike.API.Classes                 ( HasDataField )
import           Haskbike.API.Client                  ( mkClientManager )
import           Haskbike.API.ResponseWrapper
import           Haskbike.API.StationInformation
import           Haskbike.API.StationStatus
import           Haskbike.API.SystemInformation
import           Haskbike.API.SystemPricingPlan
import           Haskbike.API.SystemRegion
import           Haskbike.API.VehicleType
import           Haskbike.API.VehicleTypeFull
import           Haskbike.AppEnv

import           Network.HTTP.Client                  ( Manager )
import           Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.Gzip          ( GzipFiles (..), GzipSettings (..), defaultGzipSettings, gzip )
import           Network.Wai.Middleware.RequestLogger ( logStdout, logStdoutDev )

import qualified Servant                              as S
import           Servant
import           Servant.Client                       ( BaseUrl (BaseUrl), ClientError, ClientM, Scheme (Http),
                                                        mkClientEnv, runClientM )
import qualified Servant.Server.Generic               as S
import           Servant.Server.Generic

import           UnliftIO


-- * Mock server implementation.

-- | Mock server handlers.
mockServer :: ( WithEnv (Env m) m, WithEnv (Env m) m )
           => BikeShareAPIRoutes (AsServerT m)
mockServer = BikeShareAPIRoutes { _versions           = mkResponseWrapper mockVersions
                                , _vehicleTypes       = mkResponseWrapper mockVehicleTypes
                                , _stationInformation = mkResponseWrapper mockStationInformation
                                , _stationStatus      = mkResponseWrapper mockStationStatus
                                , _systemRegions      = mkResponseWrapper [] -- Toronto's API returns empty data.
                                , _systemInformation  = mkResponseWrapper mockSysInf
                                , _systemPricingPlans = mkResponseWrapper mockSysPricingPlans
                                }


routesLinks :: BikeShareAPIRoutes (AsLink Link)
routesLinks = allFieldLinks

data MockServerLog where
  MockServerLogNever     :: MockServerLog
  MockServerLogStdout    :: MockServerLog
  MockServerLogStdoutDev :: MockServerLog

-- | Run the mock server.
runMockServer :: (HasEnv (Env AppM) m, MonadCatch m, MonadUnliftIO m, MonadFail m)
              => MockServerLog -> Int -> m ()
runMockServer shouldLog port = do
  env <- ask
  liftIO $ runSettings serverSettings (logger shouldLog (serverApp env))
  where
    logger MockServerLogNever     = id
    logger MockServerLogStdout    = logStdout
    logger MockServerLogStdoutDev = logStdoutDev
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


-- * Helpers

runMockQuery :: Manager -> ClientM a -> IO (Either ClientError a)
runMockQuery clientManager endpoint = runClientM endpoint (mkClientEnv clientManager mockServerBaseUrl)

runMockQueryWithEnv :: ClientM a -> IO (Either ClientError a)
runMockQueryWithEnv query = do
  clientManager <- mkClientManager
  runMockQuery clientManager query

-- | Default 'BaseUrl' of mock server (NOTE: port can be configured via CLI).
mockServerBaseUrl :: BaseUrl
mockServerBaseUrl = BaseUrl Http "localhost" 8082 ""


-- * Mock data.

-- | Create a 'ResponseWrapper' with some defaults.
mkResponseWrapper :: MonadIO m => a -> m (ResponseWrapper a)
mkResponseWrapper dat = do
  ct <- liftIO getCurrentTime
  pure $
    ResponseWrapper { _respLastUpdated = ct
                    , _respTtl         = 35
                    , _respVer         = "2.3"
                    , _respData        = dat
                    }

-- | Mock response for 'APIVersion' endpoint.
mockVersions :: [APIVersion]
mockVersions = [APIVersion { _apiVersion = 2.3, _apiUrl = "localhost" }]

mockVehicleTypes =
  [ VehicleTypeFull Iconic "bicycle"           "human"     0.0 "ICONIC"  "186-1"
  , VehicleTypeFull Fit    "bicycle"           "human"     0.0 "FIT"     "186-1"
  , VehicleTypeFull Boost  "bicycle" "electric_assist"     0.0 "BOOST"   "186-2"
  , VehicleTypeFull EFit   "bicycle" "electric_assist"     0.0 "EFIT"    "186-2"
  , VehicleTypeFull EFitG5 "bicycle" "electric_assist" 60000.0 "EFIT G5" "186-2"
  ]

mockStationInformation = map mkStationInformation [7000..8000]

mkStationInformation infoStationId =
  StationInformation { .. }
  where
    infoName                  = (T.pack . show) infoStationId
    infoPhysicalConfiguration = Regular
    infoLat                   =  43.639832
    infoLon                   = -79.395954
    infoAltitude              = Just 0.0
    infoAddress               = (Just . T.pack . show) infoStationId
    infoCapacity              = 35
    infoIsChargingStation     = False
    infoRentalMethods         = [ Key, TransitCard, CreditCard, Phone ]
    infoIsValetStation        = False
    infoIsVirtualStation      = False
    infoGroups                = []
    infoObcn                  = "647-643-9607"
    infoNearbyDistance        = 500.0
    infoBluetoothId           = ""
    infoRideCodeSupport       = True
    infoRentalUris            = RentalURIs "" "" ""

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


mockStationStatus =
  [ StationStatus { _statusStationId             = 7001
                  , _statusNumBikesAvailable     = 1
                  , _statusNumBikesDisabled      = 1
                  , _statusNumDocksAvailable     = 21
                  , _statusNumDocksDisabled      = 1
                  , _statusLastReported          = Just (UTCTime (fromGregorian 2024 04 02) (timeOfDayToTime (TimeOfDay 23 41 45)))
                  , _statusIsChargingStation     = True
                  , _statusStatus                = InService
                  , _statusIsInstalled           = True
                  , _statusIsRenting             = True
                  , _statusIsReturning           = True
                  , _statusTraffic               = Nothing
                  , _statusVehicleDocksAvailable = [ VehicleDock [ "ICONIC", "BOOST", "EFIT", "EFIT G5" ] 21 ]
                  , _statusVehicleTypesAvailable =
                    Map.fromList [ ( Boost,  VehicleType Boost   0 )
                                 , ( Iconic, VehicleType Iconic  1 )
                                 , ( EFit,   VehicleType EFit    0 )
                                 , ( EFitG5, VehicleType EFitG5  0 )
                                 ]
                  }
  ]


-- | Mock response for 'SystemPricingPlans'
mockSysPricingPlans = map mkSystemPricingPlan
  [ ("186",    "Annual 30",                      105.0,  "Unlimited 30-min trips on classic bikes")
  , ("186-1",  "Annual 30",                      105.0,  "Unlimited 30-min trips on classic bikes")
  , ("186-2",  "Annual 30",                      105.0,  "Unlimited 30-min trips on classic bikes")
  , ("191",    "CMP-City of Toronto",             90.0,  "CMP-City of Toronto")
  , ("191-2",  "CMP-City of Toronto",             90.0,  "CMP-City of Toronto")
  , ("208",    "Annual 45",                      120.0,  "Unlimited 45-min trips on classic bikes")
  , ("208-1",  "Annual 45",                      120.0,  "Unlimited 45-min trips on classic bikes")
  , ("208-2",  "Annual 45",                      120.0,  "Unlimited 45-min trips on classic bikes")
  , ("209",    "Corporate 30",                    84.0,  "Corporate 30")
  , ("209-1",  "Corporate 30",                    84.0,  "Corporate 30")
  , ("209-2",  "Corporate 30",                    84.0,  "Corporate 30")
  , ("210-1",  "Corporate 45",                    96.0,  "Corporate 45")
  , ("210-2",  "Corporate 45",                    96.0,  "Corporate 45")
  , ("210",    "Corporate 45",                    96.0,  "Corporate 45")
  , ("211",    "OPTION 2 TEST",                    0.0,  "")
  , ("211-1",  "OPTION 2 TEST",                    0.0,  "")
  , ("211-2",  "OPTION 2 TEST",                    0.0,  "")
  , ("224",    "VIP Trial",                        0.0,  "VIP Trial")
  , ("224-1",  "VIP Trial",                        0.0,  "VIP Trial")
  , ("224-2",  "VIP Trial",                        0.0,  "VIP Trial")
  , ("251",    "Classic Bike DayPass",            15.0,  "90 min rides on classic bikes")
  , ("251-1",  "Classic Bike DayPass",            15.0,  "90 min rides on classic bikes")
  , ("251-2",  "Classic Bike DayPass",            15.0,  "90 min rides on classic bikes")
  , ("252-1",  "Pay-As-You-Go ($1 Unlock Fee)",    0.0,  "+Classic bike $0.12/min; Ebike $0.20/min  ")
  , ("252-2",  "Pay-As-You-Go ($1 Unlock Fee)",    0.0,  "+Classic bike $0.12/min; Ebike $0.20/min  ")
  , ("252",    "Pay-As-You-Go ($1 Unlock Fee)",    0.0,  "+Classic bike $0.12/min; Ebike $0.20/min  ")
  , ("258",    "Annual 30 - ODSP",               105.0,  "Unlimited 30-min trips with any bike")
  , ("258-1",  "Annual 30 - ODSP",               105.0,  "Unlimited 30-min trips with any bike")
  , ("258-2",  "Annual 30 - ODSP",               105.0,  "Unlimited 30-min trips with any bike")
  , ("259-1",  "Annual 45 - ODSP",               120.0,  "Unlimited 45-min trips with any bike")
  , ("259-2",  "Annual 45 - ODSP",               120.0,  "Unlimited 45-min trips with any bike")
  , ("259",    "Annual 45 - ODSP",               120.0,  "Unlimited 45-min trips with any bike")
  , ("264",    "Annual 30 3 Install",            105.0,  "3 monthly installments of $35 + tax")
  , ("264-1",  "Annual 30 3 Install",            105.0,  "3 monthly installments of $35 + tax")
  , ("264-2",  "Annual 30 3 Install",            105.0,  "3 monthly installments of $35 + tax")
  , ("265",    "Annual 45 3 Install",            120.0,  "3 monthly installments of $40 + tax")
  , ("265-1",  "Annual 45 3 Install",            120.0,  "3 monthly installments of $40 + tax")
  , ("265-2",  "Annual 45 3 Install",            120.0,  "3 monthly installments of $40 + tax")
  ]
  where
    mkSystemPricingPlan (sysPricingPlanId, sysPricingPlanName, sysPricingPlanPrice, sysPricingPlanDescription) = SystemPricingPlan {..}
    sysPricingPlanCurrency = "CAD"
    sysPricingPlanTaxable  = True
