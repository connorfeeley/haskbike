{-# LANGUAGE PartialTypeSignatures #-}

-- | CLI debug interface.
module CLI.Debug
     ( dispatchDebug
     ) where

import           AppEnv

import           CLI.Options
import           CLI.QueryFormat

import           Colog                         ( log, pattern D )

import           Control.Lens

import           Data.Int                      ( Int32 )
import           Data.Maybe                    ( fromMaybe )
import           Data.Proxy
import           Data.Text.Lazy                ( Text, pack, toStrict )

import           Database.Beam
import           Database.Beam.Schema.Tables
import           Database.BikeShare            ( StationStatusT, bikeshareStationStatus )
import           Database.BikeShare.Operations
import           Database.BikeShare.Utils      ( pPrintCompact )

import           Prelude                       hiding ( log )

import           System.Console.ANSI


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
        maybe "Error: unable to determine station status table size."
        (\tableSize' -> "Status table uses " ++ show tableSize' ++ " of storage.")
        infoTableSize

  log D $ "Status table size: " <> toStrict (pack infoTableSizeText)

  statusTableSize <- queryTableSize <$> withConn <*> pure "station_status" >>= liftIO
  let statusTableSizeText =
        maybe "Error: unable to determine station status table size."
        (\tableSize' -> "Status table uses " ++ show tableSize' ++ " of storage.")
        statusTableSize

  log D $ "Status table size: " <> toStrict (pack statusTableSizeText)
  liftIO $ putStrLn $ "Status table size: " <> statusTableSizeText <> " rows."

  -- Calculate number of dockings and undockings
  dockings   <- queryDockingEventsCount <$> withConn <*> pure (queryCondition Docking)   >>= liftIO
  undockings <- queryDockingEventsCount <$> withConn <*> pure (queryCondition Undocking) >>= liftIO

  liftIO $ do
    cliOut $ formatDatabaseStats numStatusRows infoTableSize statusTableSize

    formatDockingEventsCount dockings
    pPrintCompact $ "Length of dockings: " <> show (length dockings)
    pPrintCompact $ "Sum: " <> show (sum $ dockings ^.. traverse . _2)

    formatDockingEventsCount undockings
    pPrintCompact $ "Length of undockings: " <> show (length undockings)
    pPrintCompact $ "Sum: " <> show (sum $ undockings ^.. traverse . _2)
  where
    queryCondition :: AvailabilityCountVariation -> StatusVariationQuery
    queryCondition variation = StatusVariationQuery 7148 variation [ OldestID 1890764 ]

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
