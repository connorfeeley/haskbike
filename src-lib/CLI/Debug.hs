-- | CLI debug interface.
module CLI.Debug
     ( dispatchDebug
     ) where

import           AppEnv

import           CLI.Database
import           CLI.Options
import           CLI.QueryFormat
import           CLI.Utils

import           Colog                  ( Message, WithLog, log, pattern D, pattern I )

import           Control.Monad.Reader   ( asks, void, when )

import           Data.Int               ( Int32 )
import qualified Data.List              as List
import           Data.Maybe             ( fromMaybe )
import           Data.Text.Lazy         ( Text, pack, toStrict, unpack )

import           Database.BikeShare     ( StationStatus, bikeshareStationStatus, d_status_last_reported,
                                          d_status_num_bikes_available, d_status_num_bikes_disabled,
                                          d_status_num_docks_available, d_status_num_docks_disabled,
                                          d_status_station_id, vehicle_types_available_efit,
                                          vehicle_types_available_efit_g5, vehicle_types_available_iconic )
import           Database.Operations

import           Prelude                hiding ( log )

import           System.Console.ANSI

import           UnliftIO               ( MonadIO, MonadUnliftIO, liftIO )


-- | Dispatch CLI arguments for debugging.
dispatchDebug :: DebugMiscOptions
              -> App ()
dispatchDebug options = do
  log D "Refreshing database with latest status from API."
  num_status_rows <- fromMaybe (0 :: Int32) <$> (queryRowCount <$> withConn <*> pure bikeshareStationStatus >>= liftIO)
  log D $ "Number of rows in station_status table: " <> toStrict (pack $ show num_status_rows)
  liftIO $
    cliOut $ formatDatabaseStats num_status_rows


formatDatabaseStats :: Int32 -> [Text]
formatDatabaseStats num_status_rows =
  withHeader (pack "Database Statastics") [status_rows_text]
  where
    status_rows_text :: Text
    status_rows_text = boldCode <> colouredText Vivid White "# status entries: " <> resetIntens <> prettyNum (fromIntegral num_status_rows)
