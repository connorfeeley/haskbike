{-# LANGUAGE PartialTypeSignatures #-}
-- | CLI debug interface.
module CLI.Debug
     ( dispatchDebug
     ) where

import           AppEnv

import           CLI.Database
import           CLI.Options
import           CLI.QueryFormat
import           CLI.Utils

import           Colog                         ( Message, WithLog, log, pattern D, pattern I )

import           Control.Monad.Reader          ( asks, void, when )

import           Data.Int                      ( Int32 )
import qualified Data.List                     as List
import           Data.Maybe                    ( fromMaybe )
import           Data.Proxy
import           Data.Text.Lazy                ( Text, pack, toStrict, unpack )

import           Database.Beam
import           Database.Beam.Schema.Tables
import           Database.BikeShare            ( StationStatus, StationStatusT, bikeshareStationStatus,
                                                 d_status_last_reported, d_status_num_bikes_available,
                                                 d_status_num_bikes_disabled, d_status_num_docks_available,
                                                 d_status_num_docks_disabled, d_status_station_id,
                                                 vehicle_types_available_efit, vehicle_types_available_efit_g5,
                                                 vehicle_types_available_iconic )
import           Database.BikeShare.Operations
import           Database.PostgreSQL.Simple    ( Connection, query_ )

import           Prelude                       hiding ( log )

import           System.Console.ANSI

import           UnliftIO                      ( MonadIO, MonadUnliftIO, liftIO )



-- | Dispatch CLI arguments for debugging.
dispatchDebug :: DebugMiscOptions
              -> App ()
dispatchDebug options = do
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
        (\tableSize -> "Status table uses " ++ show tableSize ++ " of storage.")
        infoTableSize

  log D $ "Status table size: " <> toStrict (pack infoTableSizeText)

  statusTableSize <- queryTableSize <$> withConn <*> pure "station_status" >>= liftIO
  let statusTableSizeText =
        maybe "Error: unable to determine station status table size."
        (\tableSize -> "Status table uses " ++ show tableSize ++ " of storage.")
        statusTableSize

  log D $ "Status table size: " <> toStrict (pack statusTableSizeText)
  liftIO $ putStrLn $ "Status table size: " <> statusTableSizeText <> " rows."

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
