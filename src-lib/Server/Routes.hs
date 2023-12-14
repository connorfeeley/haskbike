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
     , latestQueries
     , server
     ) where

import           API.StationStatus                            ( TorontoVehicleType (..) )

import           AppEnv

import           Colog

import           Control.Lens                                 hiding ( reuse )
import           Control.Monad.Except

import           Data.Default.Class                           ( def )
import           Data.List                                    ( sortOn )
import           Data.Maybe                                   ( fromMaybe, listToMaybe )
import qualified Data.Text                                    as T
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.BikeShare.Expressions
import           Database.BikeShare.Operations
import           Database.BikeShare.Tables.StationInformation

import           Fmt

import           Lucid                                        ( ToHtml )

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic

import           Server.Classes                               ( ToHtmlComponents )
import           Server.Components.LatestQueries
import           Server.ComponentsAPI
import           Server.DataAPI
import           Server.DebugAPI
import           Server.Page.IndexPage
import           Server.Page.PerformanceCSV
import           Server.Page.SideMenu
import           Server.Page.StationList
import           Server.Page.StationStatusVisualization
import           Server.Page.SystemInfoVisualization
import           Server.Page.SystemStatusVisualization
import           Server.RobotsTXT
import           Server.VisualizationAPI

import           ServerEnv

import           TimeInterval

import           Version

data API mode where
  API :: { debugApi          :: mode :- NamedRoutes DebugAPI
         , home              :: mode :- Get '[HTML] (PureSideMenu IndexPage)
         , dataApi           :: mode :- NamedRoutes DataAPI
         , visualizationApi  :: mode :- NamedRoutes VisualizationAPI
         , componentsApi     :: mode :- NamedRoutes ComponentsAPI
         , static            :: mode :- NamedRoutes StaticAPI
         , robots            :: mode :- NamedRoutes RobotsAPI
         } -> API mode
  deriving stock Generic

type BikeShareExplorerAPI = NamedRoutes API

server :: API (AsServerT ServerAppM)
server = API { debugApi          = debugApiHandler
             , home              = homePageHandler
             , dataApi           = statusHandler
             , visualizationApi  = visualizationHandler
             , componentsApi     = componentsHandler
             , static            = staticHandler
             , robots            = robotsHandler
             }

-- * Serve static files.

data StaticAPI mode where
  StaticAPI ::
    { staticApi :: mode :- "static" :> Raw
    } -> StaticAPI mode
  deriving stock Generic

-- * Handlers.

staticHandler :: StaticAPI (AsServerT ServerAppM)
staticHandler =  StaticAPI $ serveDirectoryWebApp "static-files"

visualizationHandler :: VisualizationAPI (AsServerT ServerAppM)
visualizationHandler = VisualizationAPI
  { pageForStation     = stationStatusVisualizationPage
  , systemStatus       = systemStatusVisualizationPage
  , stationList        = stationListPage
  , systemInfo         = systemInfoVisualizationPage
  , performanceCsvPage = performanceCsvPageHandler
  }

getLatestQueries :: ServerAppM LatestQueries
getLatestQueries = do
  appEnv <- asks serverAppEnv
  latest <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith queryLatestQueryLogs
  pure $ latestQueryLogsToMap (envTimeZone appEnv) latest

-- | 'SideMenu' smart constructor.
sideMenu :: (ToHtml a, ToHtmlComponents a) => a -> ServerAppM (PureSideMenu a)
sideMenu page = do
  latest <- getLatestQueries
  pure $
    PureSideMenu
    { visPageParams = page
    , staticLink    = fieldLink staticApi
    , versionText   = getGitHash
    , latestQueries = latest
    }


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
      sideMenu $
        StationStatusVisualizationPage { _statusVisPageStationInfo    = info'
                                       , _statusVisPageStationId      = stationId
                                       , _statusVisPageTimeRange      = TimePair startTime endTime tz currentUtc
                                       , _statusVisPageTimeZone       = tz
                                       , _statusVisPageCurrentUtc     = currentUtc
                                       , _statusVisPageDataLink       = fieldLink dataForStation (Just stationId) startTime endTime
                                       , _statusVisPageStaticLink     = fieldLink staticApi
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

  sideMenu $
    SystemStatusVisualizationPage { _systemStatusVisPageTimeRange     = TimePair startTime endTime tz currentUtc
                                  , _systemStatusVisPageTimeZone      = tz
                                  , _systemStatusVisPageCurrentUtc    = currentUtc
                                  , _systemStatusVisPageInfo          = (fromMaybe def . listToMaybe . reverse . map systemStatusInfo) systemStatus -- use the latest value
                                  , _systemStatusVisPageDataLink      = fieldLink dataForStation Nothing startTime endTime
                                  , _systemStatusVisPageStaticLink    = fieldLink staticApi
                                  }

stationListPage :: Maybe T.Text -> ServerAppM (PureSideMenu StationList)
stationListPage selection = do
  appEnv <- asks serverAppEnv
  logInfo $ format "Rendering station list"

  latest <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith queryLatestStatuses

  -- Convert 'station-type' query-param to 'StationRadioInputSelection' value.
  selectionVal <- case T.toLower <$> selection of
    Just "regular"  -> logInfo "Filtering for regular stations" >> pure SelectionRegular
    Just "charging" -> logInfo "Filtering for charging stations" >> pure SelectionCharging
    Just "all"      -> logInfo "Filtering for all stations" >> pure SelectionAll
    _               -> logInfo "No filter applied" >> pure SelectionAll
  let sorted = sortOn (_infoStationId . fst) latest
  sideMenu $
    StationList
    { _stationList = sorted
    , _staticLink = fieldLink staticApi
    , _stationListSelection = selectionVal
    , _visualizationPageLink  = fieldLink pageForStation
    }

-- | Create the system status visualization page record.
systemInfoVisualizationPage :: Maybe LocalTime -> Maybe LocalTime -> ServerAppM (PureSideMenu SystemInfoVisualizationPage)
systemInfoVisualizationPage startTime endTime = do
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  currentUtc <- liftIO getCurrentTime

  sideMenu $
    SystemInfoVisualizationPage
    { _sysInfoVisPageTimeRange     = TimePair startTime endTime tz currentUtc
    , _sysInfoVisPageTimeZone      = tz
    , _sysInfoVisPageCurrentUtc    = currentUtc
    , _sysInfoVisPageDataLink      = fieldLink systemInfoData startTime endTime
    , _sysInfoVisPageStaticLink    = fieldLink staticApi
    , _sysInfoVisPageSysStatusLink = fieldLink systemStatus Nothing Nothing
    }

performanceCsvPageHandler :: Maybe LocalTime -> Maybe LocalTime -> ServerAppM (PureSideMenu PerformanceCSV)
performanceCsvPageHandler startTime endTime = do
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  -- AppM actions can be lifted into ServerAppM by using a combination of liftIO and runReaderT.
  currentUtc <- liftIO getCurrentTime

  logInfo $ format "Rendering performance CSV page"

  sideMenu $
    PerformanceCSV
    { performanceCsvPageTimeRange  = TimePair startTime endTime tz currentUtc
    , performanceCsvPageTimeZone   = tz
    , performanceCsvPageCurrentUtc = currentUtc
    , performanceCsvPageDataLink   = fieldLink performanceCsv Nothing startTime endTime
    , performanceCsvPageStaticLink = fieldLink staticApi
    }

homePageHandler :: ServerAppM (PureSideMenu IndexPage)
homePageHandler = do
  _appEnv <- asks serverAppEnv
  sideMenu $
    IndexPage { _stationStatusLink = fieldLink pageForStation }

-- routesLinks :: API (AsLink Link)
-- routesLinks = allFieldLinks

-- apiProxy :: Proxy (ToServantApi API)
-- apiProxy = genericApi (Proxy :: Proxy API)

