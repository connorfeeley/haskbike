{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

-- | This module contains the route definitions for the visualization server.

module Server.Routes
     ( API (..)
     , BikeShareExplorerAPI
     , server
     ) where

import           AppEnv

import           Colog

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.BikeShare
import           Database.BikeShare.Expressions
import           Database.BikeShare.Operations          ( StatusThreshold (..), StatusVariationQuery (..),
                                                          queryChargingEventsCountExpr )

import           Fmt

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic

import           Server.Data.StationStatusVisualization
import           Server.DataAPI
import           Server.Page.IndexPage
import           Server.Page.SideMenu
import           Server.Page.StationList
import           Server.Page.StationStatusVisualization
import           Server.VisualizationAPI

import           ServerEnv

data API mode where
  API :: { version           :: mode :- "version" :> Get '[JSON] Version
         , home              :: mode :- Get '[HTML] (PureSideMenu IndexPage)
         , stationData       :: mode :- NamedRoutes DataAPI
         , visualizationPage :: mode :- NamedRoutes VisualizationAPI
         , static            :: mode :- NamedRoutes StaticAPI
         } -> API mode
  deriving stock Generic

type Version = ((String, String), (String, String)) -- This will do for the sake of example.

type BikeShareExplorerAPI = NamedRoutes API

versionHandler :: ServerAppM Version
versionHandler = pure (("version", "0.0.1"), ("git-version", "0.0.1"))

server ::  API (AsServerT ServerAppM)
server = API { version = versionHandler
             , home = homePageHandler
             , stationData = statusHandler
             , visualizationPage = visualizationHandler
             , static = staticHandler
             }

-- * Serve static files.

data StaticAPI mode where
  StaticAPI ::
    { staticApi :: mode :- "static" :> Raw
    } -> StaticAPI mode
  deriving stock Generic

-- * Handlers.

statusHandler :: DataAPI (AsServerT ServerAppM)
statusHandler =  DataAPI { dataForStation = stationStatusData }

staticHandler :: StaticAPI (AsServerT ServerAppM)
staticHandler =  StaticAPI $ serveDirectoryWebApp "static-files"

visualizationHandler :: VisualizationAPI (AsServerT ServerAppM)
visualizationHandler = VisualizationAPI { pageForStation = stationStatusVisualizationPage
                                        , stationList = stationListPage
                                        }

stationStatusData :: Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StationStatusVisualization]
stationStatusData stationId startTime endTime = do
  logInfo $ format "Creating JSON payload for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime
  generateJsonDataSource stationId startTime endTime


stationStatusVisualizationPage :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM (PureSideMenu StationStatusVisualizationPage)
stationStatusVisualizationPage (Just stationId) startTime endTime = do
  logInfo $ format "Rendering page for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifter into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  info <- liftIO $ runAppM appEnv (withPostgres $ runSelectReturningOne $ select $ infoByIdExpr [fromIntegral stationId])
  ctz <- liftIO getCurrentTimeZone
  chargings <- liftIO $ runAppM appEnv $
    queryChargingEventsCountExpr -- FIXME: hardcoded
    (StatusVariationQuery (Just 7001) [ EarliestTime (localTimeToUTC ctz (LocalTime (read "2023-10-19") (read "00:00:00.0")))
                                      , LatestTime   (localTimeToUTC ctz (LocalTime (read "2023-10-19") (read "04:00:00.0")))
                                      ])
  case info of
    Just info' -> do
      logInfo $ "Matched station information: " <> (info' ^. infoName)
      logInfo $ "Static path: " <> toUrlPiece (fieldLink staticApi)
      let visualizationPage = StationStatusVisualizationPage { _statusVisPageStationInfo = info'
                                                             , _statusVisPageStationId   = stationId
                                                             , _statusVisPageTimeRange   = TimePair startTime endTime
                                                             , _statusVisPageTimeZone    = tz
                                                             , _statusVisPageCurrentUtc  = currentUtc
                                                             , _statusVisPageChargings   = head chargings ^. _5 & fromIntegral
                                                             , _statusVisPageDataLink    = fieldLink dataForStation stationId startTime endTime
                                                             , _statusVisPageStaticLink  = fieldLink staticApi
                                                             }
      pure PureSideMenu { visPageParams = visualizationPage
                        , staticLink = fieldLink staticApi
                        }
    _ ->  throwError err404 { errBody = "Unknown station ID." }
stationStatusVisualizationPage Nothing _ _ =
  throwError err404 { errBody = "Station ID parameter is required." }

stationListPage :: ServerAppM (PureSideMenu StationList)
stationListPage = do
  appEnv <- asks serverAppEnv
  logInfo $ format "Rendering station list"
  info <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ select $ do
    all_ (bikeshareDb ^. bikeshareStationInformation)
  let page = StationList { _stationList = info
                         , _staticLink = fieldLink staticApi
                         , _visualizationPageLink  = fieldLink pageForStation
                         }
  pure PureSideMenu { visPageParams = page
                    , staticLink = fieldLink staticApi
                    }

homePageHandler :: ServerAppM (PureSideMenu IndexPage)
homePageHandler = do
  _appEnv <- asks serverAppEnv
  let page = IndexPage { _indexStaticLink = fieldLink staticApi
                         }
  pure PureSideMenu { visPageParams = page
                    , staticLink = fieldLink staticApi
                    }

-- routesLinks :: API (AsLink Link)
-- routesLinks = allFieldLinks

-- apiProxy :: Proxy (ToServantApi API)
-- apiProxy = genericApi (Proxy :: Proxy API)
