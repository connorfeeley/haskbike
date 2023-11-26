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

import           API.StationStatus                        ( TorontoVehicleType (..) )

import           AppEnv

import           Colog

import           Control.Lens
import           Control.Monad.Except

import           Data.ByteString.Lazy                     ( ByteString )
import           Data.Csv                                 ( encodeDefaultOrderedByName )
import           Data.Default.Class                       ( def )
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
import           Server.DebugAPI
import           Server.Page.IndexPage
import           Server.Page.SideMenu
import           Server.Page.StationList
import           Server.Page.StationStatusVisualization
import           Server.Page.SystemStatusVisualization
import           Server.StatusDataParams
import           Server.VisualizationAPI

import           ServerEnv

import           TimeInterval

import           Version

data API mode where
  API :: { debug             :: mode :- NamedRoutes DebugAPI
         , home              :: mode :- Get '[HTML] (PureSideMenu IndexPage)
         , stationData       :: mode :- NamedRoutes DataAPI
         , visualizationPage :: mode :- NamedRoutes VisualizationAPI
         , componentsPage    :: mode :- NamedRoutes ComponentsAPI
         , static            :: mode :- NamedRoutes StaticAPI
         } -> API mode
  deriving stock Generic

type BikeShareExplorerAPI = NamedRoutes API

server :: API (AsServerT ServerAppM)
server = API { debug             = debugApiHandler
             , home              = homePageHandler
             , stationData       = statusHandler
             , visualizationPage = visualizationHandler
             , componentsPage    = componentsHandler
             , static            = staticHandler
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
                         , dockingEventsData    = handleDockingEventsData
                         , chargingEventsData   = handleChargingEventsData
                         }

staticHandler :: StaticAPI (AsServerT ServerAppM)
staticHandler =  StaticAPI $ serveDirectoryWebApp "static-files"

visualizationHandler :: VisualizationAPI (AsServerT ServerAppM)
visualizationHandler = VisualizationAPI
  { pageForStation = stationStatusVisualizationPage
  , systemStatus   = systemStatusVisualizationPage
  , stationList    = stationListPage
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

  let params = StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc)
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


-- | Create the station status visualization page record.
stationStatusVisualizationPage :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM (PureSideMenu StationStatusVisualizationPage)
stationStatusVisualizationPage (Just stationId) startTime endTime = do
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  currentUtc <- liftIO getCurrentTime

  info <- liftIO $ runAppM appEnv (withPostgres $ runSelectReturningOne $ select $ infoByIdExpr [fromIntegral stationId])

  case info of
    Just info' -> do
      logInfo $ "Matched station information: " <> info' ^. infoName
      logInfo $ "Static path: " <> toUrlPiece (fieldLink staticApi)
      let visualizationPage = StationStatusVisualizationPage { _statusVisPageStationInfo    = info'
                                                             , _statusVisPageStationId      = stationId
                                                             , _statusVisPageTimeRange      = TimePair startTime endTime tz currentUtc
                                                             , _statusVisPageTimeZone       = tz
                                                             , _statusVisPageCurrentUtc     = currentUtc
                                                             , _statusVisPageDataLink       = fieldLink dataForStation (Just stationId) startTime endTime
                                                             , _statusVisPageStaticLink     = fieldLink staticApi
                                                             }
      pure PureSideMenu { visPageParams = visualizationPage
                        , staticLink    = fieldLink staticApi
                        , versionText   = getGitHash
                        }
    _ ->  throwError err404 { errBody = "Unknown station ID." }
stationStatusVisualizationPage Nothing _ _ =
  throwError err404 { errBody = "Station ID parameter is required." }


-- | Create the system status visualization page record.
systemStatusVisualizationPage :: Maybe LocalTime -> Maybe LocalTime -> ServerAppM (PureSideMenu SystemStatusVisualizationPage)
systemStatusVisualizationPage startTime endTime = do
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  currentUtc <- liftIO getCurrentTime

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

  let visualizationPage = SystemStatusVisualizationPage { _systemStatusVisPageTimeRange     = TimePair startTime endTime tz currentUtc
                                                        , _systemStatusVisPageTimeZone      = tz
                                                        , _systemStatusVisPageCurrentUtc    = currentUtc
                                                        , _systemStatusVisPageInfo          = (fromMaybe def . listToMaybe . reverse . map systemStatusInfo) systemStatus -- use the latest value
                                                        , _systemStatusVisPageDataLink      = fieldLink dataForStation Nothing startTime endTime
                                                        , _systemStatusVisPageStaticLink    = fieldLink staticApi
                                                        }
  pure PureSideMenu { visPageParams = visualizationPage
                    , staticLink    = fieldLink staticApi
                    , versionText   = getGitHash
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
                    , staticLink    = fieldLink staticApi
                    , versionText   = getGitHash
                    }

homePageHandler :: ServerAppM (PureSideMenu IndexPage)
homePageHandler = do
  _appEnv <- asks serverAppEnv
  let page = IndexPage { _stationStatusLink = fieldLink pageForStation
                       }
  pure PureSideMenu { visPageParams = page
                    , staticLink    = fieldLink staticApi
                    , versionText   = getGitHash
                    }

-- routesLinks :: API (AsLink Link)
-- routesLinks = allFieldLinks

-- apiProxy :: Proxy (ToServantApi API)
-- apiProxy = genericApi (Proxy :: Proxy API)

handleDockingEventsData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [DockingEventsCount]
handleDockingEventsData stationId startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifted into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc))
  let (earliest, latest) = (earliestTime times', latestTime times')

  logInfo $ format "Rendering page for {station ID: {}, start time: {}, end time: {}} " stationId earliest latest

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  liftIO $ runAppM appEnv $ queryDockingEventsCount variation

handleChargingEventsData :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM [ChargingEvent]
handleChargingEventsData stationId startTime endTime = do
  -- Accessing the inner environment by using the serverEnv accessor.
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifted into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  -- TODO: awkward having to compute time bounds here and in 'StationStatusVisualization'
  let times' = enforceTimeRangeBounds (StatusDataParams tz currentUtc (TimePair startTime endTime tz currentUtc))
  let (earliest, latest) = (earliestTime times', latestTime times')

  logInfo $ format "Rendering page for {station ID: {}, start time: {}, end time: {}} " stationId earliest latest

  let variation = StatusVariationQuery (fromIntegral <$> stationId) [ EarliestTime (localTimeToUTC tz earliest)
                                                                    , LatestTime   (localTimeToUTC tz latest)
                                                                    ]
  events <- liftIO $ runAppM appEnv $ queryChargingEventsCount variation
  let result = map (\(_info, _totalCount, efitCount, efitG5Count) ->
                      [ ChargingEvent EFit (fromIntegral efitCount)
                      , ChargingEvent EFitG5 (fromIntegral efitG5Count)
                      ])
               events
  pure $ concat result
