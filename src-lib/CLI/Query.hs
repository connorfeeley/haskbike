-- | CLI interface for querying the database.
module CLI.Query
     ( dispatchQuery
     , requestStationDataConcurrently
     ) where

import           API.Client
import           API.ClientLifted
import           API.ResponseWrapper
import           API.StationInformation
import           API.StationStatus

import           AppEnv

import           CLI.Options
import           CLI.QueryFormat

import           Colog

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader          ( when )

import qualified Data.List                     as List
import           Data.Maybe                    ( fromMaybe )
import qualified Data.Text                     as T
import           Data.Text.Lazy                ( Text, pack, toStrict, unlines, unpack )

import           Database.BikeShare.Operations

import           Prelude                       hiding ( log, unlines )

import           Servant.Client

import           Text.Pretty.Simple.Extras

import           UnliftIO                      ( MonadIO, MonadUnliftIO, concurrently, liftIO )


-- | Dispatch CLI arguments to the query interface.
dispatchQuery :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
              => QueryOptions
              -> m ()
dispatchQuery options = do
  -- Update database with latest station data.
  when (optRefresh options) refreshStationData

  -- Query the database by either ID or name.
  case optQueryBy options of
    QueryByStationId stationId     -> queryByStationId   stationId
    QueryByStationName stationName -> queryByStationName stationName


-- | Refresh the database with the latest information and status from the API.
refreshStationData :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                   => m ()
refreshStationData = do
  log D "Concurrently fetching station information and status from API."
  (reqInfo, reqStatus) <- requestStationDataConcurrently
  log D "Refreshing database with latest station data."
  case (reqInfo, reqStatus) of
    (Left err, _) -> logException err >> throwM err
    (_, Left err) -> logException err >> throwM err
    (Right info, Right status) -> do
      insInfo   <- insertStationInformation (info ^. respLastUpdated) (info ^. respData)
      insStatus <- insertStationStatus      (status ^. respData)
      log D $ "Inserted " <> (T.pack . show) (length insInfo) <> " information records and " <> (T.pack . show) (length insStatus) <> " status records."


-- | Concurrently request station information and status.
requestStationDataConcurrently :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                               => m (Either ClientError (ResponseWrapper [StationInformation]), Either ClientError (ResponseWrapper [StationStatus]))
requestStationDataConcurrently = concurrently
  (runQueryM stationInformation :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m) => m (Either ClientError (ResponseWrapper [StationInformation])))
  (runQueryM stationStatus      :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m) => m (Either ClientError (ResponseWrapper [StationStatus])))


-- | Query the database for the station with the given ID.
queryByStationId :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                 => Int
                 -> m ()
queryByStationId stationId = do
  log I $ toStrict $ "Querying station ID '" <> (pack . show) stationId <> "'"

  -- Try to lookup station name from the ID.
  name <- queryStationName stationId
  -- Maybe tuple of (id, name)
  let station_pair = fmap (stationId,) name

  log I $ toStrict $ "Station: " <> pShowCompact station_pair

  results <- mapM (queryStatus "By ID") station_pair

  liftIO $ putStrLn $ unpack $ unlines (fromMaybe [] results)


-- | Query the database for stations with names matching the given pattern.
queryByStationName :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                   => MatchMethod String
                   -> m ()
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
queryStatus :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
            => String        -- ^ Header
            -> (Int, String) -- ^ (station_id, station_name)
            -> m [Text]
queryStatus header stationTuple = do
  -- Query the latest status for the given station tuple.
  resultsText <- queryAndFmtStationStatus header stationTuple
  pure $ withHeader (pack header) resultsText

queryAndFmtStationStatus :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                         => String -> (Int, String) -> m [Text]
queryAndFmtStationStatus header (id', name') = do
  currentTimeZone <- getTz  -- Fetch the TimeZone from the environment

  latest <- queryStationStatusLatest id'

  let status = fmap (currentTimeZone, name', ) latest

  pure $
    withHeader (pack header) (formatStationStatusResult status)
