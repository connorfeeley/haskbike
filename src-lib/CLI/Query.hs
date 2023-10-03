-- | CLI interface for querying the database.
module CLI.Query
     ( dispatchQuery
     ) where


import           CLI.Options            ( MatchMethod (..), QueryOptions (..), unMatchMethod )
import           CLI.Utils

import           Colog                  ( Message, WithLog, log, pattern D, pattern I )

import           Control.Lens

import           Data.Int               ( Int32 )
import qualified Data.List              as List
import           Data.Maybe             ( fromMaybe )
import           Data.Text.Lazy         ( Text, intercalate, pack, toStrict, unlines, unpack )
import           Data.Time              ( LocalTime (..), TimeZone, defaultTimeLocale, formatTime, getCurrentTimeZone )

import           Database.Beam.Postgres ( Connection )
import           Database.BikeShare     ( StationStatus, d_status_last_reported, d_status_num_bikes_available,
                                          d_status_num_bikes_disabled, d_status_num_docks_available,
                                          d_status_num_docks_disabled, d_status_station_id,
                                          vehicle_types_available_efit, vehicle_types_available_efit_g5,
                                          vehicle_types_available_iconic )
import           Database.Operations

import           Fmt

import           Prelude                hiding ( log, unlines )

import           ReportTime             ( localToSystem, reportToLocal )

import           System.Console.ANSI

import           UnliftIO               ( MonadIO, MonadUnliftIO, liftIO )


-- | Dispatch CLI arguments to the query interface.
dispatchQuery :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
              => QueryOptions
              -> Connection
              -> m ()
dispatchQuery options conn = do
  case options of
    QueryByStationId stationId     -> queryByStationId   stationId conn
    QueryByStationName stationName -> queryByStationName stationName conn


-- | Query the database for the station with the given ID.
queryByStationId :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
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
queryByStationName :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
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
queryStatus :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                   => Connection
                   -> String -- ^ Header
                   -> (Int, String) -- ^ (station_id, station_name)
                   -> m [Text]
queryStatus conn header station_tuple = do
  -- Query the current time zone.
  currentTimeZone <- liftIO getCurrentTimeZone

  -- Query the latest status for the given station tuple.
  resultsText <- liftIO (fmtStationStatus conn currentTimeZone station_tuple)
  pure $ withHeader (pack header) resultsText

  where
    withHeader :: Text -> [Text] -> [Text]
    withHeader header' results = case results of
      [] -> fmtHeader : ["\t\t" <> indent 6 <> italCode <> " None." <> resetItal ]
      _  -> fmtHeader : results
      where
        fmtHeader = "\t" <> indent 5 <> indent 8 <> " " <> boldCode <> underCode <> header' <> resetUnder <> resetIntens <> " " <> indent 8

fmtStationStatus :: Connection -> TimeZone -> (Int, String) -> IO [Text]
fmtStationStatus conn' currentTimeZone' (id', name') = do
  latest <- queryStationStatusLatest conn' id'
  let status = fmap (currentTimeZone', name', ) latest
  pure $ formatStationStatusResult status

formatStationStatusResult :: Maybe (TimeZone, String, StationStatus) -> [Text]
formatStationStatusResult = maybe ["No status found."] formatStationInfo

formatStationInfo :: (TimeZone, String, StationStatus) -> [Text]
formatStationInfo (timeZone, name, status) =
  let
    bikeAvailability = [ fmtBikeAvailability "Iconic"   (status ^. vehicle_types_available_iconic)
                       -- , fmtBikeAvailability "Boost"    (status ^. vehicle_types_available_boost) -- Not used in Toronto.
                       , fmtBikeAvailability "E-Fit"    (status ^. vehicle_types_available_efit)
                       , fmtBikeAvailability "E-Fit G5" (status ^. vehicle_types_available_efit_g5)]

    pairs = [ ("Docks:\t", status ^. d_status_num_docks_available, status ^. d_status_num_docks_disabled)
            , ("Bikes:\t", status ^. d_status_num_bikes_available, status ^. d_status_num_bikes_disabled)
            ]
    in [formattedName name status, formattedLastReport timeZone $ reportToLocal <$> status ^. d_status_last_reported] ++ map fmtAvailability pairs ++ bikeAvailability

formattedName :: String -> StationStatus -> Text
formattedName name status =
    format "{}[{}{}]{} {}{}{}" boldCode idPrefix (status ^. d_status_station_id) resetIntens underCode name resetUnder
    where idPrefix = resetIntens <> "# " <> boldCode

-- Format the last reported time in the specified time zone (namerly, the system's time zone).
formattedLastReport :: TimeZone -> Maybe LocalTime -> Text
formattedLastReport timeZone status = reportedText
  where
    reportedText = case status of
      Nothing -> boldCode <> colouredText Vivid Red "Never" <> resetIntens
      -- Just t  -> italCode <> showText t <> resetItal
      Just t  -> "[" <> showText t <> "]"
              <> boldCode <> "\t" <> "|" <> "\t" <> resetIntens
              <> italCode <> pack (formatTime' timeZone t) <> resetItal <> " (local)"

showText :: Show a => a -> Text
showText = pack . show

fmtAvailability :: (Text, Int32, Int32) -> Text
fmtAvailability (name, avail, disabled)
  = colouredText Dull Yellow name
 <> colouredText Vivid Green  (fmt $ padLeftF 2 ' ' avail)    <> " available" <> tab
 <> boldCode <> "|" <> tab <> resetIntens
 <> colouredText Vivid Red    (fmt $ padLeftF 2 ' ' disabled) <> " disabled"
 where tab = "\t" :: Text

fmtBikeAvailability :: Text -> Int32 -> Text
fmtBikeAvailability name count
  = tab
 <> colouredText Dull Yellow (fmt $ padLeftF 9 ' ' name) <> ": "
 <> colouredText Vivid Green  (fmt $ padLeftF 2 ' ' count) <> tab <> boldCode <> "|" <> resetIntens
 where tab = "\t" :: Text

colouredText :: ColorIntensity -> Color -> Text -> Text
colouredText intensity colour text = format "{}{}{}" (setSGRCode [SetColor Foreground intensity colour]) text (setSGRCode [])

boldCode, resetIntens, italCode, resetItal, underCode, resetUnder :: Text
boldCode    = pack $ setSGRCode [ SetConsoleIntensity  BoldIntensity   ]
resetIntens = pack $ setSGRCode [ SetConsoleIntensity  NormalIntensity ]
italCode    = pack $ setSGRCode [ SetItalicized        True            ]
resetItal   = pack $ setSGRCode [ SetItalicized        False           ]
underCode   = pack $ setSGRCode [ SetUnderlining       SingleUnderline ]
resetUnder  = pack $ setSGRCode [ SetUnderlining       NoUnderline     ]
