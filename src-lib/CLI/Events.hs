-- | This module contains the CLI functions to query the database for events.

module CLI.Events
     ( bikeCountsAtMoment
     , dayTimes
     , dispatchEvents
     , formatBikeCounts
     ) where


import           AppEnv

import           CLI.Options
import           CLI.QueryFormat

import           Colog

import           Control.Lens                            hiding ( para )

import qualified Data.Char                               as Char
import           Data.Int                                ( Int32 )
import           Data.List                               ( sortOn )
import           Data.Maybe                              ( fromMaybe )
import           Data.Ord                                ( Down (Down) )
import           Data.Text.Lazy                          ( pack, unpack )
import           Data.Time
import           Data.Time.Extras

import           Database.Beam
import           Database.BikeShare.Expressions
import           Database.BikeShare.Operations
import           Database.BikeShare.StationInformation
import           Database.BikeShare.StationStatus
import           Database.BikeShare.StatusVariationQuery

import           Fmt

import           Prelude                                 hiding ( log )

import           System.Console.ANSI

import           Text.Pretty.Simple.Extras
import qualified Text.PrettyPrint.Boxes                  as Box

import           UnliftIO

-- | Dispatch CLI arguments for debugging.
dispatchEvents :: EventSubcommand -> AppM ()
dispatchEvents (EventRange options)  = do
  -- For defaults
  today <- liftIO $ utctDay <$> getCurrentTime
  let yesterday = previousDay today

  -- From arguments, with defaults
  let firstDay = fromMaybe yesterday (startDay options)
  let lastDay = fromMaybe today (endDay options)

  log I $ format "Getting counts of each bike time every two hours between {} and {}." firstDay lastDay
  log D $ format "Options: {}" (pShowCompact options)

  -- Run queries concurrently (automatic thread pool size).
  countsAtTimes <- pooledMapConcurrently (uncurry bikeCountsAtMoment) (dayTimesRange firstDay lastDay)
  liftIO . formatBikeCounts $ countsAtTimes


dispatchEvents (EventCounts options) = do
  -- Calculate number of dockings and undockings
  log D $ format "Options: {}" (pShowCompact options)

  -- Determine current day and previous day.
  today <- liftIO $ utctDay <$> getCurrentTime
  let yesterday = previousDay today

  -- 'eventsForRange' parameters:
  let stationId = optEventsCountStationId options
  let startDay' = fromMaybe yesterday startDay
  let endDay' = fromMaybe today endDay

  -- Get undocking/docking counts.
  log I $ format "Calculating number of event counts for (optional) station {} (limit: {})." (maybeF stationId) (optEventsCountLimit options)
  eventSums <- eventsForRange stationId startDay' startTime endDay' endTime

  let sortOrder = Undocking
  liftIO $ do
    putStrLn $ format "\nSorted by differentials ({}):" (sortedMessage sortOrder)
    formatDockingEventsDifferential $ takeMaybe limit $ sortOnVariation sortOrder (sortedEventsBoth eventSums)

    putStrLn ""
    formatDockingEventsCount $ takeMaybe limit $ sortDockingEventsCount sortOrder (sortOnVariationTotal Undocking eventSums)
  where
    sortedMessage :: AvailabilityCountVariation -> String
    sortedMessage Docking   = format "{} >> {}" (showLower Docking) (showLower Undocking)
    sortedMessage Undocking = format "{} >> {}" (showLower Undocking) (showLower Docking)

    limit :: Maybe Int
    limit = optEventsCountLimit options

    startDay, endDay :: Maybe Day
    startDay = optEventsCountStartDay options
    endDay = optEventsCountEndDay options

    startTime, endTime :: TimeOfDay
    startTime = fromMaybe (TimeOfDay 00 00 00) (optEventsCountStartTime options)
    endTime   = fromMaybe (TimeOfDay 00 00 00) (optEventsCountEndTime options)


sortOnVariation :: AvailabilityCountVariation -> [(StationInformation, Int)] -> [(StationInformation, Int)]
sortOnVariation eventType = case eventType of
  Docking   -> sortOn (Down . view _2)
  Undocking -> sortOn (view _2)

-- | Sort a list of 'DockingEventsCount' by either the sum of 'Docking' or 'Undocking' events (across all bike types).
sortOnVariationTotal :: AvailabilityCountVariation -> [DockingEventsCount] -> [DockingEventsCount]
sortOnVariationTotal eventType events = case eventType of
  Docking   -> sortOn (\ev -> (  ev ^. eventsBoostCount  . eventsCountDockings)
                              + (ev ^. eventsIconicCount . eventsCountDockings)
                              + (ev ^. eventsEfitCount   . eventsCountDockings)
                              + (ev ^. eventsEfitG5Count . eventsCountDockings)
                      ) events
  Undocking -> sortOn (\ev -> (  ev ^. eventsBoostCount  . eventsCountUndockings)
                              + (ev ^. eventsIconicCount . eventsCountUndockings)
                              + (ev ^. eventsEfitCount   . eventsCountUndockings)
                              + (ev ^. eventsEfitG5Count . eventsCountUndockings)
                      ) events

-- | Show with lowercase output.
showLower :: (Show a) => a -> String
showLower = map Char.toLower . show

-- | Add the undockings and dockings for each station together, and sort the resulting list.
sortedEventsBoth :: [DockingEventsCount] -> [(StationInformation, Int)]
sortedEventsBoth = map (\counts -> (counts ^. eventsStation , (counts ^. eventsIconicCount . eventsCountUndockings ) + (counts ^. eventsIconicCount . eventsCountDockings)))

-- | Given a 'Day', get the previous 'Day'.
previousDay :: Day -> Day
previousDay = addDays (-1)

-- | Optionally take a number of elements from a list.
takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe (Just limit) xs = take limit xs
takeMaybe Nothing xs      = xs

bikeCountsAtMoment :: Day -> TimeOfDay -> AppM (Day, TimeOfDay, Int32, Int32, Int32, Int32)
bikeCountsAtMoment day timeOfDay = do
  log I $ format "Getting number of bikes by type in the system on {} at {}" day timeOfDay
  statusForMoment <- withPostgres $ runSelectReturningList $ select $
    queryLatestStatusBetweenExpr earliestTime latestTime
  pure ( day
       , timeOfDay
       , totalBoost statusForMoment
       , totalIconic statusForMoment
       , totalEbikeEfit statusForMoment
       , totalEbikeEfitG5 statusForMoment
       )
    where
      earliestTime, latestTime :: UTCTime
      latestTime   = UTCTime day (timeOfDayToTime timeOfDay)
      earliestTime = hourBefore latestTime

-- | Create a list of (Day, TimeOfDay).
dayTimes :: [(Day, TimeOfDay)]
dayTimes = [(addDays n refDay, TimeOfDay h 0 0) | n <- [0..3], h <- [0,2..22]]
  where refDay = fromGregorian 2023 10 8  -- Replace with reference day.


-- | Create a list of (Day, TimeOfDay) given a starting and ending day.
dayTimesRange :: Day -> Day -> [(Day, TimeOfDay)]
dayTimesRange startDay endDay = [(addDays n startDay, TimeOfDay h 0 0) | n <- [0..(diffDays endDay startDay)], h <- [0,2..22]]
-- ^ TODO: handle start and end times.


totalBoost, totalIconic, totalEbikeEfit, totalEbikeEfitG5 :: Num (Columnar f Int32) => [StationStatusT f] -> Columnar f Int32
totalBoost       bikeCount = sum $ map (^. vehicleTypesAvailableBoost ) bikeCount
totalIconic      bikeCount = sum $ map (^. vehicleTypesAvailableIconic) bikeCount
totalEbikeEfit   bikeCount = sum $ map (^. vehicleTypesAvailableEfit  ) bikeCount
totalEbikeEfitG5 bikeCount = sum $ map (^. vehicleTypesAvailableEfitG5) bikeCount

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
eventsForRange :: Maybe Int -> Day -> TimeOfDay -> Day -> TimeOfDay -> AppM [DockingEventsCount]
eventsForRange stationId earliestDay earliestTime latestDay latestTime = do
  -- Calculate number of dockings and undockings
  queryDockingEventsCount queryCondition
  where
    queryCondition :: StatusVariationQuery
    queryCondition =
      StatusVariationQuery
      (fromIntegral <$> stationId)
      [ EarliestTime (UTCTime earliestDay (timeOfDayToTime earliestTime)), LatestTime (UTCTime latestDay (timeOfDayToTime latestTime)) ]

-- | Sort docking and undocking events.
sortDockingEventsCount :: AvailabilityCountVariation -> [DockingEventsCount] -> [DockingEventsCount]
sortDockingEventsCount Undocking = sortOn (_eventsCountUndockings . _eventsIconicCount)
sortDockingEventsCount Docking   = sortOn (Down . _eventsCountDockings . _eventsIconicCount)

-- | Print docking and undocking events (with index).
formatDockingEventsCount :: [DockingEventsCount] -> IO ()
formatDockingEventsCount events = Box.printBox table
  where
    columns = zipWith (\index' counts ->
                         ( index' :: Int
                         , counts ^. eventsStation . infoStationId
                         , counts ^. eventsStation . infoName
                         , counts ^. eventsStation . infoIsChargingStation
                         -- Total counts
                         , (counts ^. eventsIconicCount . eventsCountUndockings)
                           + (counts ^. eventsEfitCount   . eventsCountUndockings )
                           + (counts ^. eventsEfitG5Count . eventsCountUndockings)
                         , (counts ^. eventsIconicCount . eventsCountDockings) + (counts ^. eventsEfitCount . eventsCountDockings) + (counts ^. eventsEfitG5Count . eventsCountUndockings)
                         -- Undocking counts by type
                         , counts ^. eventsIconicCount . eventsCountUndockings
                         , counts ^. eventsEfitCount   . eventsCountUndockings
                         , counts ^. eventsEfitG5Count . eventsCountUndockings
                         -- Docking counts by type
                         , counts ^. eventsIconicCount . eventsCountDockings
                         , counts ^. eventsEfitCount   . eventsCountDockings
                         , counts ^. eventsEfitG5Count . eventsCountDockings
                         )
                      ) [1..] events
    table = Box.punctuateH Box.left (Box.text " | ") [col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12]
    -- Station information
    col1  = Box.vcat Box.left    (showFn Dull  Cyan   "#"          : map (showFn Dull Cyan    . show)        (toListOf (traverse .  _1) columns))
    col2  = Box.vcat Box.left    (showFn Dull  Green  "ID"         : map (showFn Dull Green   . show)        (toListOf (traverse .  _2) columns))
    col3  = Box.vcat Box.left    (showFn Dull  White  "Name"       : map (showFn Vivid White  . read . show) (toListOf (traverse .  _3) columns))
    col4  = Box.vcat Box.left    (showFn Dull  Yellow "Charger"    : map showBoolFn                          (toListOf (traverse .  _4) columns))
    -- Total counts
    col5  = Box.vcat Box.center2 (showFn Dull  Red    "Total ↧"    : map (showFn Vivid White  . show)        (toListOf (traverse .  _5) columns))
    col6  = Box.vcat Box.center2 (showFn Dull  Green  "Total ↥"    : map (showFn Vivid White  . show)        (toListOf (traverse .  _6) columns))
    -- Undocking counts by type
    col7  = Box.vcat Box.center2 (showFn Dull  White  "Iconic ↧"   : map (showFn Dull  White  . show)        (toListOf (traverse .  _7) columns))
    col8  = Box.vcat Box.center2 (showFn Vivid White  "E-Fit ↧"    : map (showFn Vivid White  . show)        (toListOf (traverse .  _8) columns))
    col9  = Box.vcat Box.center2 (showFn Dull  White  "E-Fit G5 ↧" : map (showFn Dull  White  . show)        (toListOf (traverse .  _9) columns))
    -- Docking counts by type
    col10 = Box.vcat Box.center2 (showFn Vivid White  "Iconic ↥"   : map (showFn Vivid White  . show)        (toListOf (traverse . _10) columns))
    col11 = Box.vcat Box.center2 (showFn Dull  White  "E-Fit ↥"    : map (showFn Dull  White  . show)        (toListOf (traverse . _11) columns))
    col12 = Box.vcat Box.center2 (showFn Vivid White  "E-Fit G5 ↥" : map (showFn Vivid White  . show)        (toListOf (traverse . _12) columns))

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
                         , info ^. infoStationId
                         , info ^. infoName
                         , differential
                         )
                      ) [1..] events
    col1 = Box.vcat Box.left (Box.text (showFn Dull Cyan  "#")          : map (Box.text . showFn Dull Cyan   . show)        (toListOf (traverse . _1) columns))
    col2 = Box.vcat Box.left (Box.text (showFn Dull Green "ID")         : map (Box.text . showFn Dull Green  . show)        (toListOf (traverse . _2) columns))
    col3 = Box.vcat Box.left (Box.text (showFn Dull White "Name")       : map (Box.text . showFn Vivid White . read . show) (toListOf (traverse . _3) columns))
    col4 = Box.vcat Box.left (Box.text (showFn Dull Red   "Difference") : map (Box.text . showFn Vivid Red   . show)        (toListOf (traverse . _4) columns))
    showFn intensity colour = unpack . colouredText intensity colour . pack
    table = Box.hsep 1 Box.left [col1, col2, col3, col4]
