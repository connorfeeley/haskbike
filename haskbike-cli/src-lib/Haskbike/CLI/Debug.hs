{-# LANGUAGE PartialTypeSignatures #-}

-- | CLI debug interface.
module Haskbike.CLI.Debug
     ( dispatchDebug
     ) where


import           Colog                                  ( logDebug )

import           Control.Monad.Catch                    ( MonadCatch )

import           Data.Maybe                             ( fromMaybe )
import           Data.Proxy
import           Data.Text.Lazy                         ( Text, pack )

import           Database.Beam.Schema.Tables

import           Haskbike.AppEnv
import           Haskbike.CLI.Options
import           Haskbike.CLI.QueryFormat
import           Haskbike.Database.BikeShare
import           Haskbike.Database.Operations
import           Haskbike.Database.Tables.StationStatus

import           Prelude                                hiding ( log )

import           System.Console.ANSI

import           TextShow                               ( showt )

import           UnliftIO


-- | Dispatch CLI arguments for debugging.
dispatchDebug :: (HasEnv env m, MonadIO m, MonadFail m, MonadUnliftIO m, MonadCatch m)
              => DebugMiscOptions
              -> m ()
dispatchDebug _options = do
  -- Get the number of rows in the station status table.
  numStatusRows <-
    logDebug "Querying number of rows in status table."
    >> queryRowCount bikeshareStationStatus

  let tableSize = tableValuesNeeded (Proxy :: Proxy StationStatusT)

  logDebug $ "Number of rows in station_status table: " <> showt numStatusRows
  liftIO $ putStrLn $ "Info table contains " <> show tableSize <> " rows"

  -- Get the size of the station information table.
  infoTableSize <- queryTableSize "station_information"
  let infoTableSizeText =
        fromMaybe ("Error: unable to determine station info table size." :: String)
        infoTableSize

  logDebug $ "Info table size: " <> showt infoTableSizeText

  statusTableSize <- queryTableSize "station_status"
  let statusTableSizeText =
        fromMaybe ("Error: unable to determine station status table size." :: String)
        statusTableSize


  logDebug $ "Status table size: " <> showt statusTableSizeText
  liftIO $ putStrLn $ "Status table contains " <> show tableSize <> " rows"

  liftIO $ do
    cliOut $ formatDatabaseStats numStatusRows infoTableSize statusTableSize


formatDatabaseStats :: Int -> Maybe String -> Maybe String -> [Text]
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

