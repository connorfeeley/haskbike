{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Haskbike.Server.API.Visualization
     ( performanceCsvPageHandler
     , systemInfoVisualizationPage
     , visualizationHandler
     ) where

import           Colog

import           Control.Lens
import           Control.Monad.Catch                             ( MonadCatch, MonadThrow )
import           Control.Monad.Except                            ( MonadError )

import           Data.Default.Class                              ( def )
import           Data.Maybe                                      ( fromMaybe, listToMaybe )
import qualified Data.Text                                       as T
import           Data.Time
import           Data.Time.Extras

import           Database.Beam

import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Expressions
import           Haskbike.Database.Operations
import           Haskbike.Database.Tables.StationInformation
import qualified Haskbike.Database.Tables.StationOccupancy       as DB
import           Haskbike.Database.Tables.StationStatus
import           Haskbike.Server.Page.List.StationList
import           Haskbike.Server.Page.PerformanceCSV
import           Haskbike.Server.Page.QueryHistory               ( QueryHistoryComponent (QueryHistoryComponent),
                                                                   QueryHistoryPage (..) )
import           Haskbike.Server.Page.SideMenu
import           Haskbike.Server.Page.StationStatusVisualization
import           Haskbike.Server.Page.SystemInfoVisualization
import           Haskbike.Server.Page.SystemStatusVisualization
import           Haskbike.Server.Routes.Data
import           Haskbike.Server.Routes.Static
import           Haskbike.Server.Routes.Visualization
import           Haskbike.ServerEnv
import           Haskbike.TimeInterval

import           Servant
import           Servant.Server.Generic

import           UnliftIO                                        ( MonadUnliftIO )


-- * Handlers.

visualizationHandler :: ( HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, MonadError ServerError m, HasServerEnv env m )
                     => VisualizationAPI (AsServerT m)
visualizationHandler = VisualizationAPI
  { visualization = visualizationRoutesHandler
  }

visualizationRoutesHandler :: ( HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, MonadError ServerError m, HasServerEnv env m )
                           => VisualizationRoutesAPI (AsServerT m)
visualizationRoutesHandler = VisualizationRoutesAPI
  { pageForStation       = stationStatusVisualizationPage
  , systemStatus         = systemStatusVisualizationPage
  , stationList          = stationListPageHandler
  , stationEmptyFullList = stationEmptyFullListPage
  , systemInfo           = systemInfoVisualizationPage
  , performanceCsvPage   = performanceCsvPageHandler
  , queryHistoryPage     = queryHistoryPageHandler
  }

-- | Create the station status visualization page record.
stationStatusVisualizationPage :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m, MonadError ServerError m, HasServerEnv env m)
                               => Maybe Int -> Maybe LocalTime -> Maybe LocalTime
                               -> m (PureSideMenu StationStatusVisualizationPage)
stationStatusVisualizationPage (Just stationId) startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  info <- withPostgres $ runSelectReturningList $ selectWith $ infoByIdExpr [fromIntegral stationId]

  case info of
    [info'] -> do
      logInfo $ "Matched station information: " <> _infoName info'
      assetsLocation <- getServerAssetsLocation
      sideMenu $
        StationStatusVisualizationPage { _statusVisPageStationInfo    = info'
                                       , _statusVisPageStationId      = stationId
                                       , _statusVisPageTimeRange      = TimePair startTime endTime tz currentUtc
                                       , _statusVisPageTimeZone       = tz
                                       , _statusVisPageCurrentUtc     = currentUtc
                                       , _statusVisPageDataLink       = fieldLink dataForStation (Just stationId) startTime endTime
                                       , _statusVisPageExternalAssets = assetsLocation
                                       }
    _noInfoFound ->  throwError err404 { errBody = "Unknown station ID." }
stationStatusVisualizationPage Nothing _ _ =
  throwError err404 { errBody = "Station ID parameter is required." }


-- | Create the system status visualization page record.
systemStatusVisualizationPage :: (HasEnv env m, MonadIO m, MonadCatch m, HasServerEnv env m)
                              => Maybe LocalTime -> Maybe LocalTime
                              -> m (PureSideMenu SystemStatusVisualizationPage)
systemStatusVisualizationPage startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  let latest    = maybe currentUtc (localTimeToUTC tz) endTime
  let earliest  = hourBefore latest -- TODO: hour before?
  let increment = minsPerHourlyInterval 4 -- 15 minutes

  -- TODO: querySystemStatusAtTime should probably just return this type directly.
  systemStatus <- querySystemStatusAtRange earliest latest increment
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

  assetsLocation <- getServerAssetsLocation
  sideMenu $
    SystemStatusVisualizationPage { _systemStatusVisPageTimeRange      = TimePair startTime endTime tz currentUtc
                                  , _systemStatusVisPageTimeZone       = tz
                                  , _systemStatusVisPageCurrentUtc     = currentUtc
                                  , _systemStatusVisPageInfo           = (fromMaybe def . listToMaybe . reverse . map systemStatusInfo) systemStatus -- use the latest value
                                  , _systemStatusVisPageDataLink       = fieldLink dataForStation Nothing startTime endTime
                                  , _systemStatusVisPageStaticLink     = fieldLink staticApi
                                  , _systemStatusVisPageExternalAssets = assetsLocation
                                  }

-- | Display a list of stations.
stationListPageHandler :: (HasEnv env m, MonadIO m, MonadCatch m, HasServerEnv env m)
                       => Maybe LocalTime
                       -> m (PureSideMenu (StationList [(StationInformation, StationStatus)]))
stationListPageHandler _end = do
  logInfo "Rendering station list"

  sideMenu $
    StationList
    { _stationList           = [] -- FIXME: remove; list is queried by GridJS
    , _stationTimeRange      = (Nothing, Nothing)
    , _staticLink            = fieldLink staticApi
    , _stationListInputs     = []
    , _visualizationPageLink = fieldLink pageForStation
    }

-- | Display a list of stations with their empty/full status.
stationEmptyFullListPage :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m)
                         => Maybe LocalTime -> Maybe LocalTime
                         -> m (PureSideMenu (StationList [(StationInformation, StationStatus, DB.EmptyFull)]))
stationEmptyFullListPage start end = do
  logInfo $ "Rendering station empty/full list for time [" <> tshow start <> " - " <> tshow end <> "]"

  sideMenu $
    StationList
    { _stationList           = [] -- FIXME: remove; list is queried by GridJS
    , _stationTimeRange      = (start, end)
    , _staticLink            = fieldLink staticApi
    , _stationListInputs     = []
    , _visualizationPageLink = fieldLink pageForStation
    }
  where
    tshow = T.pack . show

-- | Create the system status visualization page record.
systemInfoVisualizationPage :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m)
                            => Maybe LocalTime -> Maybe LocalTime
                            -> m (PureSideMenu SystemInfoVisualizationPage)
systemInfoVisualizationPage startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  assetsLocation <- getServerAssetsLocation
  sideMenu $
    SystemInfoVisualizationPage
    { _sysInfoVisPageTimeRange      = TimePair startTime endTime tz currentUtc
    , _sysInfoVisPageTimeZone       = tz
    , _sysInfoVisPageCurrentUtc     = currentUtc
    , _sysInfoVisPageDataLink       = fieldLink systemInfoData startTime endTime
    , _sysInfoVisPageStaticLink     = fieldLink staticApi
    , _sysInfoVisPageSysStatusLink  = fieldLink systemStatus Nothing Nothing
    , _sysInfoVisPageExternalAssets = assetsLocation
    }

performanceCsvPageHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m)
                          => Maybe LocalTime -> Maybe LocalTime
                          -> m (PureSideMenu PerformanceCSV)
performanceCsvPageHandler startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  logInfo "Rendering performance CSV page"

  sideMenu $
    PerformanceCSV
    { performanceCsvPageTimeRange  = TimePair startTime endTime tz currentUtc
    , performanceCsvPageTimeZone   = tz
    , performanceCsvPageCurrentUtc = currentUtc
    , performanceCsvPageDataLink   = fieldLink performanceCsv Nothing startTime endTime
    , performanceCsvPageStaticLink = fieldLink staticApi
    }

queryHistoryPageHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, HasServerEnv env m)
                        => Maybe EndpointQueried
                        -> Maybe LocalTime -> Maybe LocalTime
                        -> m (PureSideMenu QueryHistoryPage)
queryHistoryPageHandler ep startTime endTime = do
  logInfo "Rendering query history page"

  tz <- getTz
  let startTime' = localTimeToUTC tz <$> startTime
  let endTime'   = localTimeToUTC tz <$> endTime


  sideMenu . QueryHistoryPage $ QueryHistoryComponent ep startTime' endTime'
