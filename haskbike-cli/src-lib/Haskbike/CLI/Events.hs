-- | This module contains the CLI functions to query the database for events.

module Haskbike.CLI.Events
     ( bikeCountsAtMoment
     , dayTimes
     , dispatchEvents
     , formatBikeCounts
     ) where


import           Colog

import           Control.Lens                                hiding ( para )
import           Control.Monad.Catch                         ( MonadCatch, MonadThrow )

import qualified Data.Char                                   as Char
import           Data.Int                                    ( Int32 )
import           Data.List                                   ( sortOn )
import           Data.Maybe                                  ( fromMaybe )
import           Data.Ord                                    ( Down (Down) )
import qualified Data.Text                                   as T
import           Data.Text.Lazy                              ( pack, unpack )
import qualified Data.Text.Lazy                              as TL
import           Data.Time
import           Data.Time.Extras

import           Database.Beam

import           Haskbike.AppEnv
import           Haskbike.CLI.Options
import           Haskbike.CLI.QueryFormat
import           Haskbike.Database.EventCounts
import           Haskbike.Database.Expressions
import           Haskbike.Database.Operations
import           Haskbike.Database.StatusVariationQuery
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus

import           Prelude                                     hiding ( log )

import           System.Console.ANSI

import           Text.Pretty.Simple.Extras
import qualified Text.PrettyPrint.Boxes                      as Box

import           TextShow                                    ( showt )

import           UnliftIO

-- | Dispatch CLI arguments for debugging.
dispatchEvents :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
               => EventSubcommand -> m ()
dispatchEvents (EventRange options)  = do
  -- For defaults
  today <- liftIO $ utctDay <$> getCurrentTime
  let yesterday = previousDay today

  -- From arguments, with defaults
  let firstDay = fromMaybe yesterday (startDay options)
  let lastDay = fromMaybe today (endDay options)

  logInfo $ "Getting counts of each bike time every two hours between " <> (T.pack . show) firstDay <> " and " <> (T.pack . show) lastDay
  logDebug $ "Options: " <> (TL.toStrict . pShowCompact) options

  -- Run queries concurrently (automatic thread pool size).
  countsAtTimes <- pooledMapConcurrently (uncurry bikeCountsAtMoment) (dayTimesRange firstDay lastDay)
  liftIO . formatBikeCounts $ countsAtTimes


dispatchEvents (EventCounts options) = do
  -- Calculate number of dockings and undockings
  logDebug $ "Options: " <> (TL.toStrict . pShowCompact) options

  -- Determine current day and previous day.
  today <- liftIO $ utctDay <$> getCurrentTime
  let yesterday = previousDay today

  -- 'eventsForRange' parameters:
  let stationId = optEventsCountStationId options
  let startDay' = fromMaybe yesterday startDay
  let endDay' = fromMaybe today endDay

  -- Get undocking/docking counts.
  logInfo $ "Calculating number of event counts for (optional) station " <> showt stationId <> " (limit: " <> showt (optEventsCountLimit options) <> ")."
  eventSums <- eventsForRange stationId startDay' startTime endDay' endTime

  let sortOrder = Undocking
  liftIO $ do
    putStrLn $ "\nSorted by differentials (" <> sortedMessage sortOrder <> "):"
    formatDockingEventsDifferential $ takeMaybe limit $ sortOnVariation sortOrder (sortedEventsBoth eventSums)

    putStrLn ""
    formatDockingEventsCount $ takeMaybe limit $ sortDockingEventsCount sortOrder (sortOnVariationTotal Undocking eventSums)
  where
    sortedMessage :: AvailabilityCountVariation -> String
    sortedMessage Docking   = showLower Docking   <> " >> " <> showLower Undocking
    sortedMessage Undocking = showLower Undocking <> " >> " <> showLower Docking

    limit :: Maybe Int
    limit = optEventsCountLimit options

    startDay, endDay :: Maybe Day
    startDay = optEventsCountStartDay options
    endDay = optEventsCountEndDay options

    startTime, endTime :: TimeOfDay
    startTime = fromMaybe (TimeOfDay 00 00 00) (optEventsCountStartTime options)
    endTime   = fromMaybe (TimeOfDay 00 00 00) (optEventsCountEndTime options)


sortOnVariation :: Ord b => AvailabilityCountVariation -> [(a, b)] -> [(a, b)]
sortOnVariation eventType = case eventType of
  Docking   -> sortOn (Down . view _2)
  Undocking -> sortOn (view _2)

-- | Sort a list of 'DockingEventsCount' by either the sum of 'Docking' or 'Undocking' events (across all bike types).
sortOnVariationTotal :: AvailabilityCountVariation -> [DockingEventsCount] -> [DockingEventsCount]
sortOnVariationTotal eventType events = case eventType of
  Docking   -> sortOn (sumPerType eventsCountDockings)   events
  Undocking -> sortOn (sumPerType eventsCountUndockings) events
  where
    sumPerType field ev =
        ev ^. eventsBoostCount  . field
      + ev ^. eventsIconicCount . field
      + ev ^. eventsEfitCount   . field
      + ev ^. eventsEfitG5Count . field
      + ev ^. eventsChloeCount  . field
      + ev ^. eventsCosmoCount  . field
      + ev ^. eventsAstroCount  . field
      + ev ^. eventsMetroCount  . field

-- | Show with lowercase output.
showLower :: (Show a) => a -> String
showLower = map Char.toLower . show

-- | Add the undockings and dockings for each station together, and sort the resulting list.
sortedEventsBoth :: [DockingEventsCount] -> [(StationInformation, Int)]
sortedEventsBoth = map (\counts -> (counts ^. eventsStation, (counts ^. eventsIconicCount . eventsCountUndockings ) + (counts ^. eventsIconicCount . eventsCountDockings)))

-- | Given a 'Day', get the previous 'Day'.
previousDay :: Day -> Day
previousDay = addDays (-1)

-- | Optionally take a number of elements from a list.
takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe (Just limit) xs = take limit xs
takeMaybe Nothing xs      = xs

-- | Per-moment row: time + per-bike-type total counts (Boost, Iconic, EFit, EFitG5, CHLOE, Cosmo, Astro, Metro).
data BikeCountsRow = BikeCountsRow
  { bikeCountsDay      :: Day
  , bikeCountsTime     :: TimeOfDay
  , bikeCountsBoost    :: Int32
  , bikeCountsIconic   :: Int32
  , bikeCountsEfit     :: Int32
  , bikeCountsEfitG5   :: Int32
  , bikeCountsChloe    :: Int32
  , bikeCountsCosmo    :: Int32
  , bikeCountsAstro    :: Int32
  , bikeCountsMetro    :: Int32
  }

bikeCountsAtMoment :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                   => Day -> TimeOfDay -> m BikeCountsRow
bikeCountsAtMoment day timeOfDay = do
  logInfo $ "Getting number of bikes by type in the system on " <> (T.pack . show) day <> " at " <> (T.pack . show) timeOfDay
  statusForMoment <- withPostgres $ runSelectReturningList $ select $
    queryLatestStatusBetweenExpr earliestTime latestTime
  pure BikeCountsRow
    { bikeCountsDay     = day
    , bikeCountsTime    = timeOfDay
    , bikeCountsBoost   = totalBoost      statusForMoment
    , bikeCountsIconic  = totalIconic     statusForMoment
    , bikeCountsEfit    = totalEbikeEfit  statusForMoment
    , bikeCountsEfitG5  = totalEbikeEfitG5 statusForMoment
    , bikeCountsChloe   = totalChloe      statusForMoment
    , bikeCountsCosmo   = totalCosmo      statusForMoment
    , bikeCountsAstro   = totalAstro      statusForMoment
    , bikeCountsMetro   = totalMetro      statusForMoment
    }
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


totalBoost, totalIconic, totalEbikeEfit, totalEbikeEfitG5,
  totalChloe, totalCosmo, totalAstro, totalMetro :: Num (Columnar f Int32) => [StationStatusT f] -> Columnar f Int32
totalBoost       bikeCount = sum $ map (^. vehicleTypesAvailableBoost ) bikeCount
totalIconic      bikeCount = sum $ map (^. vehicleTypesAvailableIconic) bikeCount
totalEbikeEfit   bikeCount = sum $ map (^. vehicleTypesAvailableEfit  ) bikeCount
totalEbikeEfitG5 bikeCount = sum $ map (^. vehicleTypesAvailableEfitG5) bikeCount
totalChloe       bikeCount = sum $ map (^. vehicleTypesAvailableChloe ) bikeCount
totalCosmo       bikeCount = sum $ map (^. vehicleTypesAvailableCosmo ) bikeCount
totalAstro       bikeCount = sum $ map (^. vehicleTypesAvailableAstro ) bikeCount
totalMetro       bikeCount = sum $ map (^. vehicleTypesAvailableMetro ) bikeCount

formatBikeCounts :: [BikeCountsRow] -> IO ()
formatBikeCounts allCounts = Box.printBox table
  where
    col_day  = Box.vcat Box.left (showFn Dull White "Date"    : map (showFn Dull Green   . show . bikeCountsDay)  allCounts)
    col_time = Box.vcat Box.left (showFn Dull White "Time"    : map (showFn Vivid White  . show . bikeCountsTime) allCounts)

    mech    r = bikeCountsBoost  r + bikeCountsIconic r
    eBikes  r = bikeCountsEfit   r + bikeCountsEfitG5 r + bikeCountsCosmo r + bikeCountsAstro r + bikeCountsMetro r
    other_  r = bikeCountsChloe  r
    totalT  r = mech r + eBikes r + other_ r

    col1  = Box.vcat Box.left (showFn Dull White   "Total"      : map (showFn Vivid Red    . show . totalT)              allCounts)
    col2  = Box.vcat Box.left (showFn Dull Green   "Mechanical" : map (showFn Vivid Green  . show . mech)                allCounts)
    col3  = Box.vcat Box.left (showFn Dull Red     "E-Bikes"    : map (showFn Vivid Red    . show . eBikes)              allCounts)
    col4  = Box.vcat Box.left (showFn Dull Yellow  "Iconic"     : map (showFn Dull Yellow  . show . bikeCountsIconic)    allCounts)
    col5  = Box.vcat Box.left (showFn Dull Yellow  "Boost"      : map (showFn Dull Yellow  . show . bikeCountsBoost)     allCounts)
    col6  = Box.vcat Box.left (showFn Dull Yellow  "E-Fit"      : map (showFn Dull Yellow  . show . bikeCountsEfit)      allCounts)
    col7  = Box.vcat Box.left (showFn Dull Yellow  "E-Fit G5"   : map (showFn Vivid Yellow . show . bikeCountsEfitG5)    allCounts)
    col8  = Box.vcat Box.left (showFn Dull Cyan    "CHLOE"      : map (showFn Dull Cyan    . show . bikeCountsChloe)     allCounts)
    col9  = Box.vcat Box.left (showFn Dull Cyan    "Cosmo"      : map (showFn Dull Cyan    . show . bikeCountsCosmo)     allCounts)
    col10 = Box.vcat Box.left (showFn Dull Cyan    "Astro"      : map (showFn Dull Cyan    . show . bikeCountsAstro)     allCounts)
    col11 = Box.vcat Box.left (showFn Dull Cyan    "Metro"      : map (showFn Dull Cyan    . show . bikeCountsMetro)     allCounts)

    showFn :: ColorIntensity -> Color -> String -> Box.Box
    showFn intensity colour = Box.text . (unpack . colouredText intensity colour . pack)
    table = Box.hsep 2 Box.left
      [col_day, col_time, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11]


-- | Get (undockings, dockings) for a day.
eventsForRange :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
               => Maybe Int -> Day -> TimeOfDay -> Day -> TimeOfDay -> m [DockingEventsCount]
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
    -- Per-type pre-extracted (undocking, docking, label) so we don't try to put lenses in a list.
    typeRows :: DockingEventsCount -> [(Int, Int, T.Text)]
    typeRows c =
      [ (c ^. eventsBoostCount  . eventsCountUndockings, c ^. eventsBoostCount  . eventsCountDockings, "Boost")
      , (c ^. eventsIconicCount . eventsCountUndockings, c ^. eventsIconicCount . eventsCountDockings, "Iconic")
      , (c ^. eventsEfitCount   . eventsCountUndockings, c ^. eventsEfitCount   . eventsCountDockings, "E-Fit")
      , (c ^. eventsEfitG5Count . eventsCountUndockings, c ^. eventsEfitG5Count . eventsCountDockings, "E-Fit G5")
      , (c ^. eventsChloeCount  . eventsCountUndockings, c ^. eventsChloeCount  . eventsCountDockings, "CHLOE")
      , (c ^. eventsCosmoCount  . eventsCountUndockings, c ^. eventsCosmoCount  . eventsCountDockings, "Cosmo")
      , (c ^. eventsAstroCount  . eventsCountUndockings, c ^. eventsAstroCount  . eventsCountDockings, "Astro")
      , (c ^. eventsMetroCount  . eventsCountUndockings, c ^. eventsMetroCount  . eventsCountDockings, "Metro")
      ]

    typeLabels :: [T.Text]
    typeLabels = ["Boost", "Iconic", "E-Fit", "E-Fit G5", "CHLOE", "Cosmo", "Astro", "Metro"]

    -- Sums across all per-type pairs for a station.
    totalUn c = sum [u | (u, _, _) <- typeRows c]
    totalDk c = sum [d | (_, d, _) <- typeRows c]

    indexedRows = zip [1 :: Int ..] events

    -- Header columns and per-station-info columns.
    indexCol     = Box.vcat Box.left    (showFn Dull  Cyan   "#"       : [showFn Dull Cyan    (show i)                                       | (i, _) <- indexedRows])
    idCol        = Box.vcat Box.left    (showFn Dull  Green  "ID"      : [showFn Dull Green   (show (c ^. eventsStation . infoStationId))    | (_, c) <- indexedRows])
    nameCol      = Box.vcat Box.left    (showFn Dull  White  "Name"    : [showFn Vivid White  (read . show $ c ^. eventsStation . infoName)  | (_, c) <- indexedRows])
    chargerCol   = Box.vcat Box.left    (showFn Dull  Yellow "Charger" : [showBoolFn (c ^. eventsStation . infoIsChargingStation)            | (_, c) <- indexedRows])
    totalUnCol   = Box.vcat Box.center2 (showFn Dull  Red    "Total ↧" : [showFn Vivid White  (show (totalUn c))                             | (_, c) <- indexedRows])
    totalDkCol   = Box.vcat Box.center2 (showFn Dull  Green  "Total ↥" : [showFn Vivid White  (show (totalDk c))                             | (_, c) <- indexedRows])

    -- Per-type undocking and docking columns.
    typeUndockingCols =
      [ Box.vcat Box.center2
          (showFn Dull White (T.unpack label <> " ↧")
            : [showFn Dull White (show u) | (_, c) <- indexedRows, let (u, _, _) = typeRows c !! idx])
      | (idx, label) <- zip [0..] typeLabels
      ]
    typeDockingCols =
      [ Box.vcat Box.center2
          (showFn Dull White (T.unpack label <> " ↥")
            : [showFn Dull White (show d) | (_, c) <- indexedRows, let (_, d, _) = typeRows c !! idx])
      | (idx, label) <- zip [0..] typeLabels
      ]

    table = Box.punctuateH Box.left (Box.text " | ")
              ([indexCol, idCol, nameCol, chargerCol, totalUnCol, totalDkCol]
                 <> typeUndockingCols
                 <> typeDockingCols)

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
