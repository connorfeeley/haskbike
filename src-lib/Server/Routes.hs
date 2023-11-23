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

import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Csv                                 ( encodeDefaultOrderedByName )
import           Data.Default.Class                       ( def )
import           Data.Int                                 ( Int32 )
import           Data.List                                ( sortOn )
import           Data.Maybe                               ( fromMaybe, listToMaybe )
import           Data.Text                                ( Text )
import qualified Data.Text                                as T
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.BikeShare
import           Database.BikeShare.Expressions
import           Database.BikeShare.Operations
import           Database.BikeShare.Operations.Factors
import           Database.BikeShare.Operations.FactorsCSV
import           Database.BikeShare.StatusVariationQuery

import           Fmt

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic

import           Server.ComponentsAPI
import           Server.Data.StationStatusVisualization
import           Server.DataAPI
import           Server.Page.IndexPage
import           Server.Page.SideMenu
import           Server.Page.StationList
import           Server.Page.StationStatusVisualization
import           Server.Page.StatusVisualization
import           Server.Page.SystemStatusVisualization
import           Server.VisualizationAPI

import           ServerEnv

import           TextShow

import           TimeInterval

import           UnliftIO                                 ( concurrently )

import           Version                                  ( getCabalVersion, getGitVersion )


data API mode where
  API :: { version           :: mode :- "version" :> Get '[JSON] Version
         , home              :: mode :- Get '[HTML] (PureSideMenu IndexPage)
         , stationData       :: mode :- NamedRoutes DataAPI
         , visualizationPage :: mode :- NamedRoutes VisualizationAPI
         , componentsPage    :: mode :- NamedRoutes ComponentsAPI
         , static            :: mode :- NamedRoutes StaticAPI
         } -> API mode
  deriving stock Generic

type Version = ((String, String), (String, String)) -- This will do for the sake of example.

type BikeShareExplorerAPI = NamedRoutes API

versionHandler :: ServerAppM Version
versionHandler = pure (("version", getCabalVersion), ("git-version", getGitVersion))

server :: API (AsServerT ServerAppM)
server = API { version = versionHandler
             , home = homePageHandler
             , stationData = statusHandler
             , visualizationPage = visualizationHandler
             , componentsPage = componentsHandler
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
statusHandler =  DataAPI { dataForStation       = stationStatusData
                         , integralsForStation  = stationIntegralData
                         , factorsForStation    = stationFactorData
                         , performanceCsv       = performanceCsvHandler
                         }

componentsHandler :: ComponentsAPI (AsServerT ServerAppM)
componentsHandler = ComponentsAPI { dockingsForStation   = dockingsPage
                                  , undockingsForStation = undockingsPage
                                  }


staticHandler :: StaticAPI (AsServerT ServerAppM)
staticHandler =  StaticAPI $ serveDirectoryWebApp "static-files"

visualizationHandler :: VisualizationAPI (AsServerT ServerAppM)
visualizationHandler = VisualizationAPI { pageForStation = stationStatusVisualizationPage
                                        , systemStatus = systemStatusVisualizationPage
                                        , stationList = stationListPage
                                        }

stationStatusData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StationStatusVisualization]
stationStatusData stationId startTime endTime = do
  logInfo $ format "Creating JSON payload for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime
  dataSource <- generateJsonDataSource stationId startTime endTime
  logDebug "Created JSON payload"
  pure dataSource


stationIntegralData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StatusIntegral]
stationIntegralData stationId startTime endTime = do
  logInfo $ format "Creating integral JSON payload for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime
  dataSource <- generateJsonDataSourceIntegral  stationId startTime endTime
  logDebug "Created integral JSON payload"
  pure dataSource


stationFactorData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [StatusFactor]
stationFactorData stationId startTime endTime = do
  logInfo $ format "Creating factor JSON payload for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime
  dataSource <- generateJsonDataSourceFactor  stationId startTime endTime
  logDebug "Created factor JSON payload"
  pure dataSource

performanceCsvHandler :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM (Headers '[Header "Content-Disposition" Text] ByteString)
performanceCsvHandler stationId startTime endTime = do
  logInfo $ format "Creating performance data CSV payload for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime

  appEnv <- getAppEnvFromServer
  let tz = envTimeZone appEnv
  currentUtc <- liftIO getCurrentTime

  let params = StatusDataParams tz currentUtc (TimePair startTime endTime)
  let range = enforceTimeRangeBounds params
  let variation = StatusVariationQuery (fromIntegral <$> stationId)
        [ EarliestTime (localTimeToUTC tz (earliestTime range))
        , LatestTime   (localTimeToUTC tz (latestTime range))
        ]

  integrals <- liftIO $ runAppM appEnv $ queryIntegratedStatus variation

  logDebug "Created performance data CSV payload"

  let fileContent = encodeIntegrals integrals

  let stationIdString :: String = maybe "system" (format "station-{}") stationId
  let filename :: String = format "{}-performance-{}-{}.csv" stationIdString (earliestTime range) (latestTime range)
  pure $ addHeader (format "attachment; filename=\"{}\"" (replaceSpaces filename)) (fileContent :: ByteString)
  where
    encodeIntegrals = encodeDefaultOrderedByName . map (PerformanceDataCSV . integralToPerformanceData)

replaceSpaces :: String -> String
replaceSpaces [] = []
replaceSpaces (x:xs)
    | x == ' ' = "-" ++ replaceSpaces xs
    | otherwise = x : replaceSpaces xs


-- | Create common values between system and station status visualization page record.
statusVisualizationPage :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime
                        -> ServerAppM (TimePair (Maybe LocalTime), TimeZone, UTCTime, [DockingEventsCount], [(StationInformation, Int32, Int32, Int32)])
statusVisualizationPage stationId startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifted into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  logInfo $ format "Rendering page for {station ID: {}, start time: {}, end time: {}} " stationId startTime endTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime))
  let earliest = earliestTime times'
  let latest = latestTime times'

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  logDebug $ format "Earliest={}, latest={}" earliest latest

  -- * Query the database for the number of bikes charged at this station, and number of bikes docked and undocked at this station.
  logDebug $ "Querying chargings and events for station " <> showt stationId
  (chargings, events) <- concurrently
      (liftIO $ runAppM appEnv $ queryChargingEventsCount variation)
      (liftIO $ runAppM appEnv $ queryDockingEventsCount variation)

  logDebug $ "Dockings (all): "   <> showt (sumEvents Docking   (allBikeEvents events))
  logDebug $ "Undockings (all): " <> showt (sumEvents Undocking (allBikeEvents events))

  pure (TimePair startTime endTime, tz, currentUtc, events, chargings)


-- | Create the station status visualization page record.
stationStatusVisualizationPage :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM (PureSideMenu StationStatusVisualizationPage)
stationStatusVisualizationPage (Just stationId) startTime endTime = do
  appEnv <- asks serverAppEnv

  info <- liftIO $ runAppM appEnv (withPostgres $ runSelectReturningOne $ select $ infoByIdExpr [fromIntegral stationId])

  (timePair, tz, currentUtc, events, chargingEvents) <- statusVisualizationPage (Just stationId)  startTime endTime
  case info of
    Just info' -> do
      logInfo $ "Matched station information: " <> info' ^. infoName
      logInfo $ "Static path: " <> toUrlPiece (fieldLink staticApi)
      let visualizationPage = StationStatusVisualizationPage { _statusVisPageStationInfo   = info'
                                                             , _statusVisPageStationId     = stationId
                                                             , _statusVisPageTimeRange     = timePair
                                                             , _statusVisPageTimeZone      = tz
                                                             , _statusVisPageCurrentUtc    = currentUtc
                                                             , _statusVisPageDockingEvents = events
                                                             , _statusVisPageChargings     = chargingEvents
                                                             , _statusVisPageDataLink      = fieldLink dataForStation (Just stationId) startTime endTime
                                                             , _statusVisPageStaticLink    = fieldLink staticApi
                                                             }
      pure PureSideMenu { visPageParams = visualizationPage
                        , staticLink = fieldLink staticApi
                        }
    _ ->  throwError err404 { errBody = "Unknown station ID." }
stationStatusVisualizationPage Nothing _ _ =
  throwError err404 { errBody = "Station ID parameter is required." }


-- | Create the system status visualization page record.
systemStatusVisualizationPage :: Maybe LocalTime -> Maybe LocalTime -> ServerAppM (PureSideMenu SystemStatusVisualizationPage)
systemStatusVisualizationPage startTime endTime = do
  (timePair, tz, currentUtc, events, chargingEvents) <- statusVisualizationPage Nothing startTime endTime

  appEnv <- asks serverAppEnv

  let latest    = maybe currentUtc (localTimeToUTC tz) endTime
  let earliest  = hourBefore latest
  let increment = minsPerHourlyInterval 4 -- 15 minutes

  -- TODO: querySystemStatusAtTime should probably just return this type directly.
  systemStatus <- liftIO $ runAppM appEnv $ querySystemStatusAtRange earliest latest increment
  let systemStatusInfo st =
        SystemStatusVisualizationInfo
        { sysStatVisInfNumStations   = 0
        , sysStatVisInfNumBikesAvail = st ^. _2 & fromIntegral
        , sysStatVisInfNumBikesDisab = st ^. _3 & fromIntegral
        , sysStatVisInfNumDocksAvail = st ^. _4 & fromIntegral
        , sysStatVisInfNumDocksDisab = st ^. _5 & fromIntegral
        , sysStatVisInfNumIconic     = st ^. _6 & fromIntegral
        , sysStatVisInfNumEfit       = st ^. _7 & fromIntegral
        , sysStatVisInfNumEfitG5     = st ^. _8 & fromIntegral
        }

  let visualizationPage = SystemStatusVisualizationPage { _systemStatusVisPageTimeRange     = timePair
                                                        , _systemStatusVisPageTimeZone      = tz
                                                        , _systemStatusVisPageCurrentUtc    = currentUtc
                                                        , _systemStatusVisPageInfo          = (fromMaybe def . listToMaybe . reverse . map systemStatusInfo) systemStatus -- use the latest value
                                                        , _systemStatusVisPageDockingEvents = events
                                                        , _systemStatusVisPageChargings     = chargingEvents
                                                        , _systemStatusVisPageDataLink      = fieldLink dataForStation Nothing startTime endTime
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


dockingsPage :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM (DockingEventsHeader 'Docking)
dockingsPage stationId startTime endTime = pure $ DockingEventsHeader ([] :: [DockingEventsCount])

undockingsPage :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM (DockingEventsHeader 'Undocking)
undockingsPage stationId startTime endTime = pure $ DockingEventsHeader ([] :: [DockingEventsCount])

chargingsPage :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM ChargingEventsHeader
chargingsPage stationId startTime endTime = pure $ ChargingEventsHeader ([] :: [(StationInformation, Int32, Int32, Int32)])
