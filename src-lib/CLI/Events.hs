-- | This module contains the CLI functions to query the database for events.

module CLI.Events
     ( bikeCountsAtMoment
     , dayTimes
     , dispatchEvents
     , formatBikeCounts
     ) where


import qualified API.Types                     as AT

import           AppEnv

import           CLI.Options
import           CLI.QueryFormat

import           Colog

import           Control.Lens                  hiding ( para )
import           Control.Monad                 ( when )

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

import           Formatting                    ( pPrintCompact )

import           Prelude                       hiding ( log )

import           ReportTime                    ( Day, TimeOfDay (..), fromGregorian, reportTime )

import           System.Console.ANSI

import           Text.Pretty.Simple            ( pShow )
import qualified Text.PrettyPrint.Boxes        as Box

import           UnliftIO

-- | Dispatch CLI arguments for debugging.
dispatchEvents :: EventSubcommand
               -> App ()
dispatchEvents (EventRange options)  = do
  log I $ format "Getting counts of each bike time every two hours between {} and {}." firstDay lastDay

  -- Run queries concurrently (automatic thread pool size).
  countsAtTimes <- pooledMapConcurrently (uncurry bikeCountsAtMoment) dayTimes
  liftIO . formatBikeCounts $ countsAtTimes
  where
    firstDay = fromGregorian 2023 0 06
    lastDay  = fromGregorian 2023 10 08
dispatchEvents (EventCounts options) = do
  -- Calculate number of dockings and undockings
  log I $ format "Calculating number of dockings and undockings. Limit: {}." (optEventsCountLimit options)
  pPrintCompact options
  currentDay <- liftIO $ utctDay <$> getCurrentTime
  eventSums <- eventsForRange (fromMaybe currentDay (optEventsCountStartDay options)) (fromMaybe currentDay (optEventsCountEndDay options))

  -- Get number of bikes by type in the system, every two hours.
  log I "Getting number of bikes by type in the system."

  liftIO $ do
    when (length eventSums < 10) (putStrLn $ format "Docking and undocking counts: {}" (length eventSums))

    putStrLn "\nSorted by undockings:"
    formatDockingEventsCount $ take limit $ sortDockingEventsCount Undocking eventSums
    putStrLn "\nSorted by dockings:"
    formatDockingEventsCount $ take limit $ sortDockingEventsCount Docking eventSums

    putStrLn "\nSorted by differentials (undockings >> dockings):"
    formatDockingEventsDifferential $ take limit $ sortOn (view _2) (sortEvents eventSums)
    putStrLn "\nSorted by differentials (dockings >> undockings):"
    formatDockingEventsDifferential $ take limit $ sortOn (Down . view _2) (sortEvents eventSums)
  where
    sortEvents = sortOn snd . map (\counts -> (station counts, undockings counts + dockings counts))
    limit = fromMaybe 50 (optEventsCountLimit options)

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
eventsForRange :: Day -> Day -> App [DockingEventsCount]
eventsForRange earliestDay latestDay = do
  -- Calculate number of dockings and undockings
  log D "Querying dockings and undockings."
  queryDockingEventsCount (queryCondition Docking)
  where
    queryCondition :: AvailabilityCountVariation -> StatusVariationQuery
    queryCondition variation =
      StatusVariationQuery
      Nothing -- or (Just 7001) for only one station (7001)
      variation
      AT.EFit
      [ EarliestTime (reportTime earliestDay (TimeOfDay 00 00 00))
      , LatestTime   (reportTime latestDay   (TimeOfDay 00 00 00))
      ]

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
