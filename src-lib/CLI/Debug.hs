{-# LANGUAGE PartialTypeSignatures #-}

-- | CLI debug interface.
module CLI.Debug
     ( dispatchDebug
     ) where

import qualified API.Types                     as AT

import           AppEnv

import           CLI.Options
import           CLI.QueryFormat

import           Colog                         ( log, pattern D )

import           Control.Lens                  hiding ( para )
import           Control.Monad                 ( when )

import           Data.Int                      ( Int32 )
import           Data.List                     ( sortOn )
import           Data.Maybe                    ( fromMaybe )
import           Data.Ord                      ( Down (Down) )
import           Data.Proxy
import           Data.Text.Lazy                ( Text, pack, toStrict, unpack )

import           Database.Beam
import           Database.Beam.Schema.Tables
import           Database.BikeShare
import           Database.BikeShare.Operations

import           Prelude                       hiding ( log )

import           ReportTime                    ( Day, TimeOfDay (TimeOfDay), fromGregorian, reportTime )

import           System.Console.ANSI

import qualified Text.PrettyPrint.Boxes        as Box


-- | Dispatch CLI arguments for debugging.
dispatchDebug :: DebugMiscOptions
              -> App ()
dispatchDebug _options = do
  -- Get the number of rows in the station status table.
  numStatusRows <-
    log D "Querying number of rows in status table."
    >> fromMaybe (0 :: Int32) <$>
    (queryRowCount <$> withConn <*> pure bikeshareStationStatus >>= liftIO)

  let tableSize = tableValuesNeeded (Proxy :: Proxy StationStatusT)

  log D $ "Number of rows in station_status table: " <> toStrict (pack $ show numStatusRows)
  liftIO $ putStrLn $ "The table contains " ++ show tableSize ++ " rows."

  -- Get the size of the station information table.
  infoTableSize <- queryTableSize <$> withConn <*> pure "station_information" >>= liftIO
  let infoTableSizeText =
        maybe "Error: unable to determine station info table size."
        (\tableSize' -> "Info table uses " ++ show tableSize' ++ " of storage.")
        infoTableSize

  log D $ "Info table size: " <> toStrict (pack infoTableSizeText)

  statusTableSize <- queryTableSize <$> withConn <*> pure "station_status" >>= liftIO
  let statusTableSizeText =
        maybe "Error: unable to determine station status table size."
        (\tableSize' -> "Status table uses " ++ show tableSize' ++ " of storage.")
        statusTableSize

  log D $ "Status table size: " <> toStrict (pack statusTableSizeText)
  liftIO $ putStrLn $ "Status table size: " <> statusTableSizeText <> " rows."

  -- Calculate number of dockings and undockings
  eventSums <- eventsForRange (fromGregorian 2023 0 06) (fromGregorian 2023 10 08)
  let differentials = sortOn snd $ map (\counts -> (station counts, undockings counts + dockings counts)) eventSums

  liftIO $ do
    cliOut $ formatDatabaseStats numStatusRows infoTableSize statusTableSize

    when (length eventSums < 10) (putStrLn $ "Docking and undocking counts: " ++ show (length eventSums))

    putStrLn "Sorted by undockings:"
    formatDockingEventsCount $ sortDockingEventsCount Undocking eventSums
    putStrLn "Sorted by dockings:"
    formatDockingEventsCount $ sortDockingEventsCount Docking eventSums

    putStrLn "\nSorted by differentials (undockings >> dockings):"
    formatDockingEventsDifferential $ take 50 $ sortOn (view _2) differentials
    putStrLn "\nSorted by differentials (dockings >> undockings):"
    formatDockingEventsDifferential $ take 50 $ sortOn (Down . view _2) differentials


formatDatabaseStats :: Int32 -> Maybe String -> Maybe String -> [Text]
formatDatabaseStats numStatusRows infoTableSize statusTableSize =
  withHeader (pack "Database Statastics") [ statusRowsText
                                          , tableSizeText "  Info" infoTableSize
                                          , tableSizeText "Status" statusTableSize
                                          ]
  where
    statusRowsText :: Text
    statusRowsText = boldCode <> colouredText Vivid White " # status entries:  " <> resetIntens <> prettyNum (fromIntegral numStatusRows)
    tableSizeText :: Text -> Maybe String -> Text
    tableSizeText tableName (Just size) = boldCode <> colouredText Vivid White tableName <> " table size:  " <> resetIntens <> pack size
    tableSizeText tableName Nothing     = boldCode <> colouredText Vivid White tableName <> " table size:  " <> resetIntens <> colouredText Vivid Red "ERROR"

-- | Get (undockings, dockings) for a day.
eventsForRange :: Day -> Day -> App [DockingEventsCount]
eventsForRange earliestDay latestDay = do
  -- Calculate number of dockings and undockings
  log D "Querying dockings and undockings."
  queryDockingEventsCount <$> withConn <*> pure (queryCondition Docking) >>= liftIO
  where
    queryCondition :: AvailabilityCountVariation -> StatusVariationQuery
    queryCondition variation = StatusVariationQuery 7148 variation AT.EFit [ EarliestTime (reportTime earliestDay (TimeOfDay 00 00 00))
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
                         , undockings counts
                         , dockings counts
                         )
                      ) [1..] events
    col1 = Box.vcat Box.left (showFn Dull Cyan  "#"          : map (showFn Dull Cyan   . show)        (toListOf (traverse . _1) columns))
    col2 = Box.vcat Box.left (showFn Dull Green "ID"         : map (showFn Dull Green  . show)        (toListOf (traverse . _2) columns))
    col3 = Box.vcat Box.left (showFn Dull White "Name"       : map (showFn Vivid White . read . show) (toListOf (traverse . _3) columns))
    col4 = Box.vcat Box.left (showFn Dull Red   "Undockings" : map (showFn Vivid Red   . show)        (toListOf (traverse . _4) columns))
    col5 = Box.vcat Box.left (showFn Dull Blue  "Dockings"   : map (showFn Vivid Blue  . show)        (toListOf (traverse . _5) columns))
    showFn :: ColorIntensity -> Color -> String -> Box.Box
    showFn intensity colour = Box.text . (unpack . colouredText intensity colour . pack)
    table = Box.hsep 1 Box.left [col1, col2, col3, col4, col5]

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
                      ) [0..] events
    col1 = Box.vcat Box.left (Box.text (showFn Dull Cyan  "#")          : map (Box.text . showFn Dull Cyan   . show)        (toListOf (traverse . _1) columns))
    col2 = Box.vcat Box.left (Box.text (showFn Dull Green "ID")         : map (Box.text . showFn Dull Green  . show)        (toListOf (traverse . _2) columns))
    col3 = Box.vcat Box.left (Box.text (showFn Dull White "Name")       : map (Box.text . showFn Vivid White . read . show) (toListOf (traverse . _3) columns))
    col4 = Box.vcat Box.left (Box.text (showFn Dull Red   "Difference") : map (Box.text . showFn Vivid Red   . show)        (toListOf (traverse . _4) columns))
    showFn intensity colour = unpack . colouredText intensity colour . pack
    table = Box.hsep 1 Box.left [col1, col2, col3, col4]
