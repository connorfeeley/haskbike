-- | CLI interface for querying the database.
module CLI.Query
     ( dispatchQuery
     ) where

import           AppEnv

import           CLI.Database
import           CLI.Options
import           CLI.QueryFormat
import           CLI.Utils

import           Colog                  ( Message, WithLog, log, pattern D, pattern I )

import           Control.Monad.Reader   ( asks, when )

import qualified Data.List              as List
import           Data.Maybe             ( fromMaybe )
import           Data.Text.Lazy         ( Text, pack, toStrict, unlines, unpack )

import           Database.Beam.Postgres ( Connection )
import           Database.BikeShare     ( StationStatus, d_status_last_reported, d_status_num_bikes_available,
                                          d_status_num_bikes_disabled, d_status_num_docks_available,
                                          d_status_num_docks_disabled, d_status_station_id,
                                          vehicle_types_available_efit, vehicle_types_available_efit_g5,
                                          vehicle_types_available_iconic )
import           Database.Operations

import           Prelude                hiding ( log, unlines )

import           UnliftIO               ( MonadIO, MonadUnliftIO, liftIO )


-- | Dispatch CLI arguments to the query interface.
dispatchQuery :: (App ~ m, WithLog env Message m, MonadIO m, MonadUnliftIO m)
              => QueryOptions
              -> Connection
              -> m ()
dispatchQuery options conn = do
  -- Refresh the database if requested.
  when (optRefresh options) $ do
    log D "Refreshing database with latest status from API."
    handleStatus conn

  -- Query the database by either ID or name.
  case optQueryBy options of
    QueryByStationId stationId     -> queryByStationId   stationId conn
    QueryByStationName stationName -> queryByStationName stationName conn


-- | Query the database for the station with the given ID.
queryByStationId :: (App ~ m, WithLog env Message m, MonadIO m, MonadUnliftIO m)
                 => Int
                 -> Connection
                 -> m ()
queryByStationId stationId conn = do
  log I $ toStrict $ "Querying station ID '" <> (pack . show) stationId <> "'"

  -- Try to lookup station name from the ID.
  name <- liftIO $ queryStationName conn stationId
  -- Maybe tuple of (id, name)
  let station_pair = fmap (stationId,) name

  log I $ toStrict $ "Station: " <> (pack . show) station_pair

  results <- mapM (queryStatus conn "By ID") station_pair

  liftIO $ putStrLn $ unpack $ unlines (fromMaybe [] results)


-- | Query the database for stations with names matching the given pattern.
queryByStationName :: (App ~ m, WithLog env Message m, MonadIO m, MonadUnliftIO m)
                   => MatchMethod String
                   -> Connection
                   -> m ()
queryByStationName stationMatch conn = do
  log I $ toStrict $ "Querying station names like '" <> pack stationName <> "'"

  let transformer = case stationMatch of
        WildcardMatch query -> ("Wildcard", nameTransformer "%" query "%")
        ExactMatch    query -> ("Exact",    nameTransformer ""  query "")
        PrefixMatch   query -> ("Prefix",   nameTransformer ""  query "%")
        SuffixMatch   query -> ("Suffix",   nameTransformer "%" query "")

  results <- liftIO $ queryStationIdLike conn (snd transformer)
  log D $ toStrict $ fst transformer <> ": "    <> (pack . show) results

  resultsText <- mapM (queryStatus conn (unpack $ fst transformer)) results
  liftIO $ putStrLn $ unpack $ unlines (concat resultsText)

  where
    stationName = unMatchMethod stationMatch
    nameTransformer prepend name append = List.intercalate "" [prepend, name, append]

-- | Query the database for the status of the given station tuple.
queryStatus :: (App ~ m, WithLog env Message m, MonadIO m, MonadUnliftIO m)
            => Connection
            -> String -- ^ Header
            -> (Int, String) -- ^ (station_id, station_name)
            -> m [Text]
queryStatus conn header station_tuple = do
  -- Query the current time zone.
  currentTimeZone <- asks envTimeZone  -- Fetch the timeZone from the environment

  -- Query the latest status for the given station tuple.
  resultsText <- liftIO (fmtStationStatus conn currentTimeZone station_tuple)
  pure $ withHeader (pack header) resultsText
