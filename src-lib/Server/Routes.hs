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

import           Data.List                              ( sortOn )
import qualified Data.Text                              as T
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.BikeShare
import           Database.BikeShare.Expressions
import           Database.BikeShare.Operations

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
import           Server.Page.SystemStatusVisualization
import           Server.VisualizationAPI

import           ServerEnv

import           TextShow

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
                                        , systemStatus = systemStatusVisualizationPage
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
  -- AppM actions can be lifted into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  info <- liftIO $ runAppM appEnv (withPostgres $ runSelectReturningOne $ select $ infoByIdExpr [fromIntegral stationId])

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime))
  let earliest = earliestTime times
  let latest = latestTime times

  let variation = StatusVariationQuery (Just (fromIntegral stationId)) [ EarliestTime (localTimeToUTC tz earliest)
                                                                       , LatestTime   (localTimeToUTC tz latest)
                                                                       ]
  logDebug $ format "earliest={}, latest={}" earliest latest

  -- * Query the database for the number of bikes charged at this station.
  logDebug $ "Querying chargings for station " <> showt stationId
  chargings <- liftIO $ runAppM appEnv $ queryChargingEventsCount variation
  logDebug $ "Chargings (all): " <> showt (sumAllCharging chargings)

  -- * Query the database for the number of bikes docked and undocked across entire system.
  logDebug $ "Querying events for station " <> showt stationId
  events <- liftIO $ runAppM appEnv $ queryDockingEventsCount variation

  logDebug $ "Dockings (all): "   <> showt (sumEvents Docking   (allBikeEvents events))
  logDebug $ "Undockings (all): " <> showt (sumEvents Undocking (allBikeEvents events))

  case info of
    Just info' -> do
      logInfo $ "Matched station information: " <> info' ^. infoName
      logInfo $ "Static path: " <> toUrlPiece (fieldLink staticApi)
      let visualizationPage = StationStatusVisualizationPage { _statusVisPageStationInfo   = info'
                                                             , _statusVisPageStationId     = stationId
                                                             , _statusVisPageTimeRange     = TimePair startTime endTime
                                                             , _statusVisPageTimeZone      = tz
                                                             , _statusVisPageCurrentUtc    = currentUtc
                                                             , _statusVisPageDockingEvents = events
                                                             , _statusVisPageChargings     = chargings
                                                             , _statusVisPageDataLink      = fieldLink dataForStation stationId startTime endTime
                                                             , _statusVisPageStaticLink    = fieldLink staticApi
                                                             }
      pure PureSideMenu { visPageParams = visualizationPage
                        , staticLink = fieldLink staticApi
                        }
    _ ->  throwError err404 { errBody = "Unknown station ID." }
stationStatusVisualizationPage Nothing _ _ =
  throwError err404 { errBody = "Station ID parameter is required." }

systemStatusVisualizationPage :: Maybe LocalTime -> Maybe LocalTime -> ServerAppM (PureSideMenu SystemStatusVisualizationPage)
systemStatusVisualizationPage startTime endTime = do
  logInfo $ format "Rendering page for {start time: {}, end time: {}} " startTime endTime
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifted into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime))
  let earliest = earliestTime times
  let latest = latestTime times
  let variation = StatusVariationQuery Nothing [ EarliestTime (localTimeToUTC tz earliest)
                                               , LatestTime   (localTimeToUTC tz latest)
                                               ]
  logDebug $ format "earliest={}, latest={}" earliest latest

  -- * Query the database for the number of bikes charged across entire system.
  logDebug "Querying chargings for entire system"
  chargings <- liftIO $ runAppM appEnv $ queryChargingEventsCount variation
  logDebug $ "Chargings (all): " <> showt (sumAllCharging chargings)

  -- * Query the database for the number of bikes docked and undocked across entire system.
  logDebug "Querying events for entire system"
  events <- liftIO $ runAppM appEnv $ queryDockingEventsCount variation

  logDebug $ "Dockings (all): "   <> showt (sumEvents Docking   (allBikeEvents events))
  logDebug $ "Undockings (all): " <> showt (sumEvents Undocking (allBikeEvents events))

  logInfo $ "Static path: " <> toUrlPiece (fieldLink staticApi)
  let visualizationPage = SystemStatusVisualizationPage { _systemStatusVisPageTimeRange     = TimePair startTime endTime
                                                        , _systemStatusVisPageTimeZone      = tz
                                                        , _systemStatusVisPageCurrentUtc    = currentUtc
                                                        , _systemStatusVisPageDockingEvents = events
                                                        , _systemStatusVisPageChargings     = chargings
                                                        -- , _systemStatusVisPageDataLink      = fieldLink dataForStation stationId startTime endTime
                                                        , _systemStatusVisPageStaticLink    = fieldLink staticApi
                                                        }
  pure PureSideMenu { visPageParams = visualizationPage
                    , staticLink = fieldLink staticApi
                    }

stationListPage :: Maybe T.Text -> ServerAppM (PureSideMenu StationList)
stationListPage selection = do
  appEnv <- asks serverAppEnv
  logInfo $ format "Rendering station list"
  info <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ select $ all_ (bikeshareDb ^. bikeshareStationInformation)

  -- Convert 'station-type' query-param to 'StationRadioInputSelection' value.
  selectionVal <- case T.toLower <$> selection of
    Just "regular"  -> logInfo "Filtering for regular stations" >> pure SelectionRegular
    Just "charging" -> logInfo "Filtering for charging stations" >> pure SelectionCharging
    Just "all"      -> logInfo "Filtering for all stations" >> pure SelectionAll
    _               -> logInfo "No filter applied" >> pure SelectionAll
  let sortedInfo = sortOn _infoStationId info
  let page = StationList { _stationList = sortedInfo
                         , _staticLink = fieldLink staticApi
                         , _stationListSelection = selectionVal
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
