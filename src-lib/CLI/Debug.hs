{-# LANGUAGE PartialTypeSignatures #-}

-- | CLI debug interface.
module CLI.Debug
     ( dispatchDebug
     ) where


import           AppEnv

import           CLI.Options
import           CLI.QueryFormat

import           Colog                            ( log, pattern D )

import           Data.Int                         ( Int32 )
import           Data.Maybe                       ( fromMaybe )
import           Data.Proxy
import           Data.Text.Lazy                   ( Text, pack )

import           Database.Beam.Schema.Tables
import           Database.BikeShare
import           Database.BikeShare.Operations
import           Database.BikeShare.StationStatus

import           Fmt

import           Prelude                          hiding ( log )

import           System.Console.ANSI

import           UnliftIO


-- | Dispatch CLI arguments for debugging.
dispatchDebug :: DebugMiscOptions
              -> AppM ()
dispatchDebug _options = do
  -- Get the number of rows in the station status table.
  numStatusRows <-
    log D "Querying number of rows in status table."
    >> fromMaybe (0 :: Int32) <$> queryRowCount bikeshareStationStatus

  let tableSize = tableValuesNeeded (Proxy :: Proxy StationStatusT)

  log D $ format "Number of rows in station_status table: {}" numStatusRows
  liftIO $ putStrLn $ format "Info table contains {} rows" tableSize

  -- Get the size of the station information table.
  infoTableSize <- queryTableSize "station_information"
  let infoTableSizeText =
        maybe ("Error: unable to determine station info table size." :: String)
        (format "Info table uses {} of storage.")
        infoTableSize

  log D $ format "Info table size: {}" infoTableSizeText

  statusTableSize <- queryTableSize "StationStatus"
  let statusTableSizeText =
        maybe ("Error: unable to determine station status table size." :: String)
        (format "Status table uses {} of storage.")
        statusTableSize

  log D $ format "Status table size: {}" statusTableSizeText
  liftIO $ putStrLn $ format "Status table contains {} rows." tableSize

  liftIO $ do
    cliOut $ formatDatabaseStats numStatusRows infoTableSize statusTableSize


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

