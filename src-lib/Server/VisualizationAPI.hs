{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.VisualizationAPI
     ( VisualizationAPI (..)
     , performanceCsvPageHandler
     , systemInfoVisualizationPage
     , visualizationHandler
     ) where

import           Colog

import           Control.Lens
import           Control.Monad.Catch                          ( MonadCatch, MonadThrow )
import           Control.Monad.Except                         ( MonadError )

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
import           Database.BikeShare.Tables.StationStatus

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic

import           Server.Data.EmptyFullData
import           Server.DataAPI
import           Server.Page.List.StationEmptyFullList
import           Server.Page.PerformanceCSV
import           Server.Page.SelectionForm                    ( SelectionFormInput (..), StationListFilter (..) )
import           Server.Page.SideMenu
import           Server.Page.StationStatusVisualization
import           Server.Page.SystemInfoVisualization
import           Server.Page.SystemStatusVisualization
import           Server.StaticAPI
import           Server.Utils

import           ServerEnv

import           TimeInterval

import           UnliftIO                                     ( MonadUnliftIO )


-- | Visualization API handler.
data VisualizationAPI mode where
  VisualizationAPI ::
    { pageForStation :: mode :-
        "visualization" :>
          "station-status"
          :> QueryParam "station-id" Int :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu StationStatusVisualizationPage)
    , systemStatus :: mode :-
        "visualization" :>
          "system-status"
          :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu SystemStatusVisualizationPage)
    , stationList :: mode :-
        "visualization" :>
          "station-list"
          :> QueryParam "station-type" StationListFilter
          :> Get '[HTML] (PureSideMenu (StationList [(StationInformation, StationStatus)]))
    , stationEmptyFullList :: mode :-
        "visualization" :>
          "station-empty-full-list"
          :> QueryParam "start-time"   LocalTime
          :> QueryParam "end-time"     LocalTime
          :> Get '[HTML] (PureSideMenu (StationList [(StationInformation, StationStatus, EmptyFull)]))
    , systemInfo :: mode :-
        "visualization" :>
          "system-information"
          :> QueryParam "start-time" LocalTime :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu SystemInfoVisualizationPage)
    , performanceCsvPage :: mode :-
        "visualization" :>
          "system-status" :>
          "performance" :>
          "csv"
          :> QueryParam "start-time" LocalTime
          :> QueryParam "end-time" LocalTime
          :> Get '[HTML] (PureSideMenu PerformanceCSV)
    } -> VisualizationAPI mode
  deriving stock Generic

-- * Handlers.

visualizationHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m, MonadError ServerError m) => VisualizationAPI (AsServerT m)
visualizationHandler = VisualizationAPI
  { pageForStation       = stationStatusVisualizationPage
  , systemStatus         = systemStatusVisualizationPage
  , stationList          = stationListPageHandler
  , stationEmptyFullList = stationEmptyFullListPage
  , systemInfo           = systemInfoVisualizationPage
  , performanceCsvPage   = performanceCsvPageHandler
  }

-- | Create the station status visualization page record.
stationStatusVisualizationPage :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m, MonadError ServerError m) => Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> m (PureSideMenu StationStatusVisualizationPage)
stationStatusVisualizationPage (Just stationId) startTime endTime = do
  tz <- getTz
  currentUtc <- liftIO getCurrentTime

  info <- withPostgres $ runSelectReturningList $ selectWith $ infoByIdExpr [fromIntegral stationId]

  case info of
    [info'] -> do
      logInfo $ "Matched station information: " <> _infoName info'
      sideMenu $
        StationStatusVisualizationPage { _statusVisPageStationInfo    = info'
                                       , _statusVisPageStationId      = stationId
                                       , _statusVisPageTimeRange      = TimePair startTime endTime tz currentUtc
                                       , _statusVisPageTimeZone       = tz
                                       , _statusVisPageCurrentUtc     = currentUtc
                                       , _statusVisPageDataLink       = fieldLink dataForStation (Just stationId) startTime endTime
                                       , _statusVisPageStaticLink     = fieldLink staticApi
                                       }
    _noInfoFound ->  throwError err404 { errBody = "Unknown station ID." }
stationStatusVisualizationPage Nothing _ _ =
  throwError err404 { errBody = "Station ID parameter is required." }


-- | Create the system status visualization page record.
systemStatusVisualizationPage :: (HasEnv env m, MonadIO m, MonadCatch m) => Maybe LocalTime -> Maybe LocalTime -> m (PureSideMenu SystemStatusVisualizationPage)
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

  sideMenu $
    SystemStatusVisualizationPage { _systemStatusVisPageTimeRange     = TimePair startTime endTime tz currentUtc
                                  , _systemStatusVisPageTimeZone      = tz
                                  , _systemStatusVisPageCurrentUtc    = currentUtc
                                  , _systemStatusVisPageInfo          = (fromMaybe def . listToMaybe . reverse . map systemStatusInfo) systemStatus -- use the latest value
                                  , _systemStatusVisPageDataLink      = fieldLink dataForStation Nothing startTime endTime
                                  , _systemStatusVisPageStaticLink    = fieldLink staticApi
                                  }

-- | Display a list of stations.
stationListPageHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe StationListFilter -> m (PureSideMenu (StationList [(StationInformation, StationStatus)]))
stationListPageHandler stations  = stationListPage (defaultStationFilter stations)

stationListPage :: (HasEnv env m, MonadIO m, MonadCatch m) => StationListFilter -> m (PureSideMenu (StationList [(StationInformation, StationStatus)]))
stationListPage filterSelection = do
  logInfo "Rendering station list"

  latest <- withPostgres $ runSelectReturningList $ select queryLatestStatuses

  let sorted = sortOn (_infoStationId . fst) latest
  sideMenu $
    StationList
    { _stationList           = sorted
    , _stationTimeRange      = (Nothing, Nothing)
    , _staticLink            = fieldLink staticApi
    , _stationListInputs     = [ StationTypeInput filterSelection
                               , SearchInput "station-filter-input" "Type a station name, ID, or address"
                               ]
    , _visualizationPageLink = fieldLink pageForStation
    }

-- | Display a list of stations with their empty/full status.
defaultStationFilter :: Maybe StationListFilter -> StationListFilter
defaultStationFilter (Just stations) = stations
defaultStationFilter Nothing         = AllStations

stationEmptyFullListPage :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe LocalTime -> Maybe LocalTime -> m (PureSideMenu (StationList [(StationInformation, StationStatus, EmptyFull)]))
stationEmptyFullListPage start end = do
  logInfo $ "Rendering station empty/full list for time [" <> tshow start <> " - " <> tshow end <> "]"

  sideMenu $
    StationList
    { _stationList           = [] -- sorted
    , _stationTimeRange      = (start, end)
    , _staticLink            = fieldLink staticApi
    , _stationListInputs     = []
    , _visualizationPageLink = fieldLink pageForStation
    }
  where
    tshow = T.pack . show

-- | Create the system status visualization page record.
systemInfoVisualizationPage :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe LocalTime -> Maybe LocalTime -> m (PureSideMenu SystemInfoVisualizationPage)
systemInfoVisualizationPage startTime endTime = do
  tz <- getTz
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

performanceCsvPageHandler :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Maybe LocalTime -> Maybe LocalTime -> m (PureSideMenu PerformanceCSV)
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
