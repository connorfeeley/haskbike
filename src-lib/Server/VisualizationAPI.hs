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
import           Data.List                                    ( sortOn )
import qualified Data.Map                                     as Map
import           Data.Maybe                                   ( fromMaybe, listToMaybe, mapMaybe )
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
  OrderByTimeFull            :: OrderByOption
  OrderByTimeEmpty           :: OrderByOption
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
                                    asciiCI "time-full"            $> OrderByTimeFull            <|>
                                    asciiCI "time-empty"           $> OrderByTimeEmpty
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
          :> QueryParam "station-type" StationListFilter
          :> Get '[HTML] (PureSideMenu (StationList [(StationInformation, StationStatus)]))
    , stationEmptyFullList :: mode :-
        "visualization" :>
          "station-empty-full-list"
          :> QueryParam "station-type" StationListFilter
          :> QueryParam "order-by-dir" OrderByDirection
          :> QueryParam "order-by"     OrderByOption
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
  , stationList          = stationListPageHandler
  , stationEmptyFullList = stationEmptyFullListPageHandler
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
stationListPageHandler :: Maybe StationListFilter -> ServerAppM (PureSideMenu (StationList [(StationInformation, StationStatus)]))
stationListPageHandler stations  = stationListPage (defaultStationFilter stations)

stationListPage :: StationListFilter -> ServerAppM (PureSideMenu (StationList [(StationInformation, StationStatus)]))
stationListPage filterSelection = do
  appEnv <- asks serverAppEnv
  logInfo "Rendering station list"

  latest <- liftIO $ runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith queryLatestStatuses

  let sorted = sortOn (_infoStationId . fst) latest
  sideMenu $
    StationList
    { _stationList           = sorted
    , _staticLink            = fieldLink staticApi
    , _stationListSelection  = filterSelection
    , _visualizationPageLink = fieldLink pageForStation
    }

-- | Display a list of stations with their empty/full status.
stationEmptyFullListPageHandler :: Maybe StationListFilter -> Maybe OrderByDirection -> Maybe OrderByOption -> ServerAppM (PureSideMenu (StationList [(StationInformation, StationStatus, EmptyFull)]))
stationEmptyFullListPageHandler stations dir order = stationEmptyFullListPage (defaultStationFilter stations) (defaultOrderByDirection dir) (defaultOrderByOption order)

defaultStationFilter :: Maybe StationListFilter -> StationListFilter
defaultStationFilter (Just stations) = stations
defaultStationFilter Nothing         = AllStations

defaultOrderByDirection :: Maybe OrderByDirection -> OrderByDirection
defaultOrderByDirection (Just dir) = dir
defaultOrderByDirection Nothing    = OrderByAsc

defaultOrderByOption :: Maybe OrderByOption -> OrderByOption
defaultOrderByOption (Just order) = order
defaultOrderByOption Nothing      = OrderByStationId

stationEmptyFullListPage :: StationListFilter -> OrderByDirection -> OrderByOption -> ServerAppM (PureSideMenu (StationList [(StationInformation, StationStatus, EmptyFull)]))
stationEmptyFullListPage filterSelection orderDirSelection orderSelection = do
  appEnv <- asks serverAppEnv
  currentUtc <- liftIO getCurrentTime
  -- let currentUtc = UTCTime (fromGregorian 2024 02 15) (timeOfDayToTime midnight)
  logInfo $ "Rendering station empty/full list for time [" <> (T.pack . show) currentUtc <> "] and order [" <> (T.pack . show) orderDirSelection <> "] of [" <> (T.pack . show) orderSelection <> "]"

  (latest, emptyFull) <- liftIO $ concurrently (runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith queryLatestStatuses)
                                               (runAppM appEnv $ withPostgres $ runSelectReturningList $ selectWith $
                                                queryStationEmptyFullTime (Nothing :: Maybe Integer) (addUTCTime (-24 * 60 * 60) currentUtc) currentUtc)

  let combined = combineStations latest (resultToEmptyFull emptyFull)

  let sorted = orderByOptionToSortOn orderDirSelection orderSelection combined
  sideMenu $
    StationList
    { _stationList           = sorted
    , _staticLink            = fieldLink staticApi
    , _stationListSelection  = filterSelection
    , _visualizationPageLink = fieldLink pageForStation
    }
  where
    resultToEmptyFull = map (\(i, (e, f)) -> (i, EmptyFull ((secondsToNominalDiffTime . fromIntegral) e) ((secondsToNominalDiffTime . fromIntegral) f)))

orderByOptionToSortOn :: OrderByDirection -> OrderByOption -> [(StationInformation, StationStatus, EmptyFull)] -> [(StationInformation, StationStatus, EmptyFull)]
orderByOptionToSortOn direction opt = case direction of
  OrderByAsc  -> applySort
  OrderByDesc -> reverse . applySort
  where
    applySort = case opt of
      OrderByStationId           -> sortOn (\(info, _, _)   -> _infoStationId info)
      OrderByStationName         -> sortOn (\(info, _, _)   -> _infoName info)
      OrderByStationType         -> sortOn (\(info, _, _)   -> _infoPhysicalConfiguration info)
      OrderByStationCapacity     -> sortOn (\(info, _, _)   -> _infoCapacity info)
      OrderByMechanicalAvailable -> sortOn (\(_, status, _) -> _statusNumBikesAvailable status)
      OrderByEfitAvailable       -> sortOn (\(_, status, _) -> _availableEfit $ _statusVehicleTypesAvailable status)
      OrderByEfitG5Available     -> sortOn (\(_, status, _) -> _availableEfitG5 $ _statusVehicleTypesAvailable status)
      OrderByBikesDisabled       -> sortOn (\(_, status, _) -> _statusNumBikesDisabled status)
      OrderByTimeFull            -> sortOn (\(_, _, full)   -> _fullTime full)
      OrderByTimeEmpty           -> sortOn (\(_, _, full)   -> _emptyTime full)

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
