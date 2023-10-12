-- | This module contains the CLI functions to query the database for events.

module CLI.Events
     ( bikeCountsAtMoment
     , dayTimes
     , dispatchEvents
     , formatBikeCounts
     ) where


import           API.Types                     ( TorontoVehicleType (..) )
import qualified API.Types                     as AT

import           AppEnv

import           CLI.Options
import           CLI.QueryFormat

import           Colog

import           Control.Lens                  hiding ( para )
import           Control.Monad                 ( when )

import qualified Data.Char                     as Char
import           Data.Int                      ( Int32 )
import           Data.List                     ( sortOn )
import           Data.Maybe                    ( fromMaybe )
import           Data.Ord                      ( Down (Down) )
import           Data.Text.Lazy                ( pack, unpack )
import           Data.Time                     ( addDays, getCurrentTime, utctDay )

import           Database.Beam
import           Database.BikeShare
import           Database.BikeShare.Operations

import           Fmt

import           Formatting

import           Prelude                       hiding ( log )

import           ReportTime                    ( Day, TimeOfDay (..), fromGregorian, reportTime )

import           System.Console.ANSI

import qualified Text.PrettyPrint.Boxes        as Box

import           UnliftIO

-- | Dispatch CLI arguments for debugging.
dispatchEvents :: EventSubcommand
               -> App ()
dispatchEvents (EventRange options)  = do
  log I $ format "Getting counts of each bike time every two hours between {} and {}." firstDay lastDay
  log D $ format "Options: {}" (pShowCompact options)

  -- Run queries concurrently (automatic thread pool size).
  countsAtTimes <- pooledMapConcurrently (uncurry bikeCountsAtMoment) dayTimes
  liftIO . formatBikeCounts $ countsAtTimes
  where
    firstDay = fromGregorian 2023 0 06
    lastDay  = fromGregorian 2023 10 08

dispatchEvents (EventCounts options) = do
  -- Calculate number of dockings and undockings
  log D $ format "Options: {}" (pShowCompact options)

  -- Determine current day and previous day.
  today <- liftIO $ utctDay <$> getCurrentTime
  let yesterday = previousDay today

  -- 'eventsForRange' parameters:
  let stationId :: Maybe Int = Nothing -- Get event counts for all stations.
  let bikeType = Iconic
  let eventType = Docking
  let startDay' = fromMaybe yesterday startDay
  let endDay' = fromMaybe today endDay

  -- Get undocking/docking counts.
  log I $ format "Calculating number of {} {}s for (optional) station {} (limit: {})." (showLower bikeType) (showLower eventType) (maybeF stationId) (optEventsCountLimit options)
  eventSums <- eventsForRange stationId bikeType eventType startDay' startTime endDay' endTime

  liftIO $ do
    when (length eventSums < 10) (putStrLn $ format "{} counts: {}" (show eventType) (length eventSums))

    formatDockingEventsCount $ takeMaybe limit $ sortDockingEventsCount eventType eventSums

    putStrLn $ format "\nSorted by differentials ({}):" (sortedMessage eventType)
    formatDockingEventsDifferential $ takeMaybe limit $ sortOnVariation eventType (sortedEventsBoth eventSums)
  where
    sortedMessage :: AvailabilityCountVariation -> String
    sortedMessage Docking   = format "{} >> {}" (showLower Docking) (showLower Undocking)
    sortedMessage Undocking = format "{} >> {}" (showLower Undocking) (showLower Docking)

    sortOnVariation :: AvailabilityCountVariation -> [(StationInformation, Int)] -> [(StationInformation, Int)]
    sortOnVariation eventType = case eventType of
      Docking   -> sortOn (Down . view _2)
      Undocking -> sortOn (view _2)

    limit :: Maybe Int
    limit = optEventsCountLimit options

    startDay, endDay :: Maybe Day
    startDay = optEventsCountStartDay options
    endDay = optEventsCountEndDay options

    startTime, endTime :: TimeOfDay
    startTime = fromMaybe (TimeOfDay 00 00 00) (optEventsCountStartTime options)
    endTime   = fromMaybe (TimeOfDay 00 00 00) (optEventsCountEndTime options)


-- | Show with lowercase output.
showLower :: (Show a) => a -> String
showLower = map Char.toLower . show

-- | Add the undockings and dockings for each station together, and sort the resulting list.
sortedEventsBoth :: [DockingEventsCount] -> [(StationInformation, Int)]
sortedEventsBoth = map (\counts -> (station counts, undockings counts + dockings counts))

-- | Given a 'Day', get the previous 'Day'.
previousDay :: Day -> Day
previousDay = addDays (-1)

-- | Optionally take a number of elements from a list.
takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe (Just limit) xs = take limit xs
takeMaybe Nothing xs      = xs

bikeCountsAtMoment :: Day -> TimeOfDay -> App (Day, TimeOfDay, Int32, Int32, Int32, Int32)
bikeCountsAtMoment day timeOfDay = do

  log I $ format "Getting number of bikes by type in the system on {} at {}" day timeOfDay
  statusForMoment <- queryAllStationsStatusBeforeTime (reportTime day timeOfDay)
  pure ( day
       , timeOfDay
       , totalBoost statusForMoment
       , totalIconic statusForMoment
       , totalEbikeEfit statusForMoment
       , totalEbikeEfitG5 statusForMoment
       )


-- | Create a list of (Day, TimeOfDay).
dayTimes :: [(Day, TimeOfDay)]
dayTimes = [(addDays n refDay, TimeOfDay h 0 0) | n <- [0..3], h <- [0,2..22]]
  where refDay = fromGregorian 2023 10 8  -- Replace with reference day.



totalBoost, totalIconic, totalEbikeEfit, totalEbikeEfitG5 :: Num (Columnar f Int32) => [StationStatusT f] -> Columnar f Int32
totalBoost bikeCount            = sum $ map (^. vehicle_types_available_boost)   bikeCount
totalIconic bikeCount           = sum $ map (^. vehicle_types_available_iconic)  bikeCount
totalEbikeEfit bikeCount        = sum $ map (^. vehicle_types_available_efit)    bikeCount
totalEbikeEfitG5 bikeCount      = sum $ map (^. vehicle_types_available_efit_g5) bikeCount

formatBikeCounts :: [(Day, TimeOfDay, Int32, Int32, Int32, Int32)] -> IO ()
formatBikeCounts allCounts = Box.printBox table
  where
    col_day  = Box.vcat Box.left (showFn Dull White "Date"    : map (showFn Dull Green   . show) (toListOf (traverse . _1) allCounts))
    col_time = Box.vcat Box.left (showFn Dull White "Time"    : map (showFn Vivid White  . show) (toListOf (traverse . _2) allCounts))

    col1 = Box.vcat Box.left (showFn Dull White  "Total"      : [(showFn Vivid Red    . show) (c + d + e +f) | (_, _, c, d, e, f) <- allCounts])
    col2 = Box.vcat Box.left (showFn Dull Green  "Mechanical" : [(showFn Vivid Green  . show) (c + d)        | (_, _, c, d, _, _) <- allCounts])
    col3 = Box.vcat Box.left (showFn Dull Red    "E-Bikes"    : [(showFn Vivid Red    . show) (e + f)        | (_, _, _, _, e, f) <- allCounts])
    col4 = Box.vcat Box.left (showFn Dull Yellow "E-Fit"      : [(showFn Dull Yellow  . show) e              | (_, _, _, _, e, _) <- allCounts])
    col5 = Box.vcat Box.left (showFn Dull Yellow "E-Fit G5"   : [(showFn Vivid Yellow . show) f              | (_, _, _, _, _, f) <- allCounts])

    showFn :: ColorIntensity -> Color -> String -> Box.Box
    showFn intensity colour = Box.text . (unpack . colouredText intensity colour . pack)
    table = Box.hsep 2 Box.left [col_day, col_time, col1, col2, col3, col4, col5]


-- | Get (undockings, dockings) for a day.
eventsForRange :: Maybe Int -> AT.TorontoVehicleType -> AvailabilityCountVariation -> Day -> TimeOfDay -> Day -> TimeOfDay -> App [DockingEventsCount]
eventsForRange stationId vehicleType variation earliestDay earliestTime latestDay latestTime = do
  -- Calculate number of dockings and undockings
  log D $ format "Querying {} events." (showLower variation)
  queryDockingEventsCount (queryCondition variation)
  where
    queryCondition :: AvailabilityCountVariation -> StatusVariationQuery
    queryCondition variation' =
      StatusVariationQuery
      (fromIntegral <$> stationId)
      variation'
      vehicleType
      [ EarliestTime (reportTime earliestDay earliestTime) , LatestTime (reportTime latestDay latestTime) ]

-- | Sort docking and undocking events.
sortDockingEventsCount :: AvailabilityCountVariation -> [DockingEventsCount] -> [DockingEventsCount]
sortDockingEventsCount Undocking = sortOn undockings
sortDockingEventsCount Docking   = sortOn (Down . dockings)

-- | Print docking and undocking events (with index).
formatDockingEventsCount :: [DockingEventsCount] -> IO ()
formatDockingEventsCount events = Box.printBox table
  where
    columns = zipWith (\index' counts ->
                         ( index' :: Int
                         , station counts ^. info_station_id
                         , station counts ^. info_name
                         , station counts ^. info_is_charging_station
                         , undockings counts
                         , dockings counts
                         )
                      ) [1..] events
    table = Box.hsep 1 Box.left [col1, col2, col3, col4, col5, col6]
    col1 = Box.vcat Box.left (showFn Dull Cyan   "#"          : map (showFn Dull Cyan   . show)        (toListOf (traverse . _1) columns))
    col2 = Box.vcat Box.left (showFn Dull Green  "ID"         : map (showFn Dull Green  . show)        (toListOf (traverse . _2) columns))
    col3 = Box.vcat Box.left (showFn Dull White  "Name"       : map (showFn Vivid White . read . show) (toListOf (traverse . _3) columns))
    col4 = Box.vcat Box.left (showFn Dull Yellow "Charger"    : map showBoolFn                         (toListOf (traverse . _4) columns))
    col5 = Box.vcat Box.left (showFn Dull Red    "Undockings" : map (showFn Vivid Red   . show)        (toListOf (traverse . _5) columns))
    col6 = Box.vcat Box.left (showFn Dull Blue   "Dockings"   : map (showFn Vivid Blue  . show)        (toListOf (traverse . _6) columns))

    showFn :: ColorIntensity -> Color -> String -> Box.Box
    showFn intensity colour = Box.text . (unpack . colouredText intensity colour . pack)

    showBoolFn :: Bool -> Box.Box
    showBoolFn value = if value then (showFn Vivid Yellow . show) value else (showFn Dull White . show) value


-- | Print difference between docking and undocking event counts.
formatDockingEventsDifferential :: [(StationInformation, Int)] -> IO ()
formatDockingEventsDifferential events = Box.printBox table
  where
    columns = zipWith (\index' (info, differential) ->
                         ( index' :: Int
                         , info ^. info_station_id
                         , info ^. info_name
                         , differential
                         )
                      ) [1..] events
    col1 = Box.vcat Box.left (Box.text (showFn Dull Cyan  "#")          : map (Box.text . showFn Dull Cyan   . show)        (toListOf (traverse . _1) columns))
    col2 = Box.vcat Box.left (Box.text (showFn Dull Green "ID")         : map (Box.text . showFn Dull Green  . show)        (toListOf (traverse . _2) columns))
    col3 = Box.vcat Box.left (Box.text (showFn Dull White "Name")       : map (Box.text . showFn Vivid White . read . show) (toListOf (traverse . _3) columns))
    col4 = Box.vcat Box.left (Box.text (showFn Dull Red   "Difference") : map (Box.text . showFn Vivid Red   . show)        (toListOf (traverse . _4) columns))
    showFn intensity colour = unpack . colouredText intensity colour . pack
    table = Box.hsep 1 Box.left [col1, col2, col3, col4]
