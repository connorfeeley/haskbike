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

import           Control.Applicative                          ( (<|>) )
import           Control.Lens

import           Data.Attoparsec.Text
import           Data.Bifunctor                               ( first )
import           Data.Default.Class                           ( def )
import           Data.Functor                                 ( ($>) )
import           Data.Int                                     ( Int32 )
import           Data.List                                    ( sortOn )
import qualified Data.Map                                     as Map
import           Data.Maybe                                   ( fromMaybe, listToMaybe, mapMaybe )
import           Data.Ord
import qualified Data.Text                                    as T
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.BikeShare.Expressions
import           Database.BikeShare.Operations
import           Database.BikeShare.Operations.StationEmpty
import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationStatus

import           Servant
import           Servant.HTML.Lucid
import           Servant.Server.Generic

import           Server.DataAPI
import           Server.Page.List.StationEmptyFullList
import           Server.Page.List.StationList
import           Server.Page.PerformanceCSV
import           Server.Page.SideMenu
import           Server.Page.StationStatusVisualization
import           Server.Page.SystemInfoVisualization
import           Server.Page.SystemStatusVisualization
import           Server.StaticAPI
import           Server.Utils

import           ServerEnv

import           TimeInterval

import           UnliftIO                                     ( concurrently )

data OrderByDirection where
  OrderByAsc  :: OrderByDirection
  OrderByDesc :: OrderByDirection
  deriving stock (Eq, Show)

instance FromHttpApiData OrderByDirection where
  parseUrlPiece :: T.Text -> Either T.Text OrderByDirection
  parseUrlPiece p = case parseOnly (asciiCI "asc"  $> OrderByAsc <|>
                                    asciiCI "desc" $> OrderByDesc) p of
    Left e  -> Left  (T.pack e)
    Right v -> Right v
  parseQueryParam = parseUrlPiece

instance ToHttpApiData OrderByDirection where
  toQueryParam = toUrlPiece
  toUrlPiece = T.pack . show


data OrderByOption where
  OrderByStationId           :: OrderByOption
  OrderByStationName         :: OrderByOption
  OrderByStationType         :: OrderByOption
  OrderByStationCapacity     :: OrderByOption
  OrderByMechanicalAvailable :: OrderByOption
  OrderByEfitAvailable       :: OrderByOption
  OrderByEfitG5Available     :: OrderByOption
  OrderByBikesDisabled       :: OrderByOption
  OrderBySecondsFull         :: OrderByOption
  OrderBySecondsEmpty        :: OrderByOption
  deriving stock (Eq, Show)

instance FromHttpApiData OrderByOption where
  parseUrlPiece :: T.Text -> Either T.Text OrderByOption
  parseUrlPiece p = case parseOnly (asciiCI "station-id"           $> OrderByStationId           <|>
                                    asciiCI "station-name"         $> OrderByStationName         <|>
                                    asciiCI "station-type"         $> OrderByStationType         <|>
                                    asciiCI "station-capacity"     $> OrderByStationCapacity     <|>
                                    asciiCI "mechanical-available" $> OrderByMechanicalAvailable <|>
                                    asciiCI "efit-available"       $> OrderByEfitAvailable       <|>
                                    asciiCI "efit-g5-available"    $> OrderByEfitG5Available     <|>
                                    asciiCI "bikes-disabled"       $> OrderByBikesDisabled       <|>
                                    asciiCI "seconds-full"         $> OrderBySecondsFull         <|>
                                    asciiCI "seconds-empty"        $> OrderBySecondsEmpty
                                   ) p of
    Left e  -> Left  (T.pack e)
    Right v -> Right v
  parseQueryParam = parseUrlPiece

instance ToHttpApiData OrderByOption where
  toQueryParam = toUrlPiece
  toUrlPiece = T.pack . show


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
          :> QueryParam "station-type" T.Text :> Get '[HTML] (PureSideMenu (StationList [(StationInformation, StationStatus)]))
    , stationEmptyFullList :: mode :-
        "visualization" :>
          "station-empty-full-list"
          :> QueryParam "station-type" T.Text
          :> QueryParam "order-by-dir" OrderByDirection
          :> QueryParam "order-by" OrderByOption
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

visualizationHandler :: VisualizationAPI (AsServerT ServerAppM)
visualizationHandler = VisualizationAPI
  { pageForStation       = stationStatusVisualizationPage
  , systemStatus         = systemStatusVisualizationPage
  , stationList          = stationListPage
  , stationEmptyFullList = stationEmptyFullListPage
  , systemInfo           = systemInfoVisualizationPage
  , performanceCsvPage   = performanceCsvPageHandler
  }

-- | Create the station status visualization page record.
stationStatusVisualizationPage :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> ServerAppM (PureSideMenu StationStatusVisualizationPage)
stationStatusVisualizationPage (Just stationId) startTime endTime = do
  appEnv <- asks serverAppEnv
  let tz = envTimeZone appEnv
  currentUtc <- liftIO getCurrentTime

  info <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith $ infoByIdExpr [fromIntegral stationId]

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

-- | Display a list of stations.
stationListPage :: Maybe T.Text -> ServerAppM (PureSideMenu (StationList [(StationInformation, StationStatus)]))
stationListPage filterSelection = do
  appEnv <- asks serverAppEnv
  logInfo "Rendering station list"

  latest <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith queryLatestStatuses

  -- Convert 'station-type' query-param to 'StationRadioInputSelection' value.
  selectionVal <- case T.toLower <$> filterSelection of
    Just "regular"  -> logInfo "Filtering for regular stations" >> pure SelectionRegular
    Just "charging" -> logInfo "Filtering for charging stations" >> pure SelectionCharging
    Just "all"      -> logInfo "Filtering for all stations" >> pure SelectionAll
    _               -> logInfo "No filter applied" >> pure SelectionAll
  let sorted = sortOn (_infoStationId . fst) latest
  sideMenu $
    StationList
    { _stationList           = sorted
    , _staticLink            = fieldLink staticApi
    , _stationListSelection  = selectionVal
    , _visualizationPageLink = fieldLink pageForStation
    }

-- | Display a list of stations with their empty/full status.
stationEmptyFullListPage :: Maybe T.Text -> Maybe OrderByDirection -> Maybe OrderByOption -> ServerAppM (PureSideMenu (StationList [(StationInformation, StationStatus, EmptyFull)]))
stationEmptyFullListPage filterSelection orderDirSelection orderSelection = do
  appEnv <- asks serverAppEnv
  currentUtc <- liftIO getCurrentTime
  -- let currentUtc = UTCTime (fromGregorian 2024 02 15) (timeOfDayToTime midnight)
  logInfo $ "Rendering station empty/full list for time [" <> (T.pack . show) currentUtc <> "] and order [" <> (T.pack . show) orderDirSelection <> "] of [" <> (T.pack . show) orderSelection <> "]"

  -- (latest, empty) :: ((StationInformation, StationStatus), (StationInformation, Int32))
  (latest, emptyFull) <- liftIO $ concurrently (runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith queryLatestStatuses)
                                               (runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith $
                                                queryStationEmptyFullTime Nothing (addUTCTime (-24 * 60 * 60) currentUtc) currentUtc)

  let combined = combineStations latest (resultToEmptyFull emptyFull)

  -- Convert 'station-type' query-param to 'StationRadioInputSelection' value.
  selectionVal <- case T.toLower <$> filterSelection of
    Just "regular"  -> logInfo "Filtering for regular stations"  >> pure SelectionRegular
    Just "charging" -> logInfo "Filtering for charging stations" >> pure SelectionCharging
    Just "all"      -> logInfo "Filtering for all stations"      >> pure SelectionAll
    _               -> logInfo "No filter applied"               >> pure SelectionAll
  let sorted = sortOn (_infoStationId . (^. _1)) combined
  sideMenu $
    StationList
    { _stationList           = sorted
    , _staticLink            = fieldLink staticApi
    , _stationListSelection  = selectionVal
    , _visualizationPageLink = fieldLink pageForStation
    }
  where
    resultToEmptyFull = map (\(i, (e, f)) -> (i, EmptyFull ((secondsToNominalDiffTime . fromIntegral) e) ((secondsToNominalDiffTime . fromIntegral) f)))

combineStations :: [(StationInformation, StationStatus)] -> [(StationInformation, EmptyFull)] -> [(StationInformation, StationStatus, EmptyFull)]
combineStations latestStatuses empties = mapMaybe combine latestStatuses
  where
    combine (info, status) = do
      emptyFull <- Map.lookup (_infoStationId info) empties'
      return (info, status, emptyFull)

    empties' = Map.fromList (map (first _infoStationId) empties)

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

  logInfo "Rendering performance CSV page"

  sideMenu $
    PerformanceCSV
    { performanceCsvPageTimeRange  = TimePair startTime endTime tz currentUtc
    , performanceCsvPageTimeZone   = tz
    , performanceCsvPageCurrentUtc = currentUtc
    , performanceCsvPageDataLink   = fieldLink performanceCsv Nothing startTime endTime
    , performanceCsvPageStaticLink = fieldLink staticApi
    }
