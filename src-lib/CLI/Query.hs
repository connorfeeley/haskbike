-- | CLI interface for querying the database.
module CLI.Query
     ( dispatchQuery
     ) where

import           AppEnv

import           CLI.Database
import           CLI.Options
import           CLI.QueryFormat

import           Colog                         ( log, pattern D, pattern I )

import           Control.Monad.Reader          ( asks, when )

import qualified Data.List                     as List
import           Data.Maybe                    ( fromMaybe )
import           Data.Text.Lazy                ( Text, pack, toStrict, unlines, unpack )

import           Database.BikeShare.Operations

import           Prelude                       hiding ( log, unlines )


-- | Dispatch CLI arguments to the query interface.
dispatchQuery :: QueryOptions
              -> AppM ()
dispatchQuery options = do
  -- Refresh the database if requested.
  when (optRefresh options) $ do
    log D "Refreshing database with latest status from API."
    handleStatus

  -- Query the database by either ID or name.
  case optQueryBy options of
    QueryByStationId stationId     -> queryByStationId   stationId
    QueryByStationName stationName -> queryByStationName stationName


-- | Query the database for the station with the given ID.
queryByStationId :: Int
                 -> AppM ()
queryByStationId stationId = do
  log I $ toStrict $ "Querying station ID '" <> (pack . show) stationId <> "'"

  -- Try to lookup station name from the ID.
  name <- queryStationName stationId
  -- Maybe tuple of (id, name)
  let station_pair = fmap (stationId,) name

  log I $ toStrict $ "Station: " <> (pack . show) station_pair

  results <- mapM (queryStatus "By ID") station_pair

  liftIO $ putStrLn $ unpack $ unlines (fromMaybe [] results)


-- | Query the database for stations with names matching the given pattern.
queryByStationName :: MatchMethod String
                   -> AppM ()
queryByStationName stationMatch = do
  log I $ toStrict $ "Querying station names like '" <> pack stationName <> "'"

  let transformer = case stationMatch of
        WildcardMatch query -> ("Wildcard", nameTransformer "%" query "%")
        ExactMatch    query -> ("Exact",    nameTransformer ""  query "")
        PrefixMatch   query -> ("Prefix",   nameTransformer ""  query "%")
        SuffixMatch   query -> ("Suffix",   nameTransformer "%" query "")

  results <- queryStationIdLike (snd transformer)
  log D $ toStrict $ fst transformer <> ": "    <> (pack . show) results

  resultsText <- mapM (queryStatus (unpack $ fst transformer)) results
  liftIO $ putStrLn $ unpack $ unlines (concat resultsText)

  where
    stationName = unMatchMethod stationMatch
    nameTransformer prepend name append = List.intercalate "" [prepend, name, append]

-- | Query the database for the status of the given station tuple.
queryStatus :: String        -- ^ Header
            -> (Int, String) -- ^ (station_id, station_name)
            -> AppM [Text]
queryStatus header station_tuple = do
  -- Query the current time zone.
  currentTimeZone <- asks envTimeZone  -- Fetch the timeZone from the environment

  -- Query the latest status for the given station tuple.
  resultsText <- fmtStationStatus currentTimeZone station_tuple
  pure $ withHeader (pack header) resultsText
