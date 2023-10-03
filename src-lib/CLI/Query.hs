{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | CLI interface for querying the database.
module CLI.Query
     ( dispatchQuery
     ) where


import           CLI.Options            ( QueryOptions (..) )

import           Colog                  ( Message, WithLog, log, pattern D, pattern I )

import           Control.Lens

import           Data.Int               ( Int32 )
import           Data.List              ( intercalate )
import           Data.Text.Lazy         ( Text, pack, toStrict, unlines, unpack )
import           Data.Time              ( LocalTime (..), TimeZone, defaultTimeLocale, formatTime, getCurrentTimeZone )

import           Database.Beam.Postgres ( Connection )
import           Database.BikeShare     ( StationStatus, d_status_last_reported, d_status_num_bikes_available,
                                          d_status_num_bikes_disabled, d_status_num_docks_available,
                                          d_status_num_docks_disabled, d_status_station_id )
import           Database.Operations
import           Database.Utils         ( pPrintCompact )

import           Fmt

import           Prelude                hiding ( log, unlines )

import           ReportTime             ( localToSystem, reportToLocal )

import           System.Console.ANSI

import           UnliftIO               ( MonadIO, MonadUnliftIO, liftIO )

dispatchQuery :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
              => QueryOptions
              -> Connection
              -> m ()
dispatchQuery options conn = do
  case options of
    QueryByStationId stationId     -> queryByStationId   stationId conn
    QueryByStationName stationName -> queryByStationName stationName conn


queryByStationId :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                 => Int
                 -> Connection
                 -> m ()
queryByStationId stationId conn = do
  log I $ toStrict $ "Querying station ID '" <> (pack . show) stationId <> "'"
  name <- liftIO $ queryStationName conn stationId
  log I $ toStrict $ "Station : " <> (pack . show) name
  pPrintCompact name

queryByStationName :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                   => String
                   -> Connection
                   -> m ()
queryByStationName stationName conn = do
  currentTimeZone <- liftIO getCurrentTimeZone

  log I $ toStrict $ "Querying station names like '" <> pack stationName <> "'"
  resultsAnywhere  <- liftIO $ queryStationIdLike conn (nameTransformer "%" stationName "%")
  resultsBegins    <- liftIO $ queryStationIdLike conn (nameTransformer ""  stationName "%")
  resultsEnds      <- liftIO $ queryStationIdLike conn (nameTransformer "%" stationName "")

  log D $ toStrict $ "Wildcard: "    <> (pack . show) resultsAnywhere
  log D $ toStrict $ "Begins with: " <> (pack . show) resultsBegins
  log D $ toStrict $ "Ends with: "   <> (pack . show) resultsEnds

  cliOut $ "" : "Matched anywhere: " : formatStationResults resultsAnywhere
  cliOut $ "" : "Begins with: "      : formatStationResults resultsBegins
  cliOut $ "" : "Ends with: "        : formatStationResults resultsEnds

  liftIO $ mapM_ (showStationStatus conn currentTimeZone) resultsAnywhere

  where
    nameTransformer prepend name append = intercalate "" [prepend, name, append]

    formatStationResults :: [(Int, String)] -> [Text]
    formatStationResults results = case results of
      []        -> ["No results found."]
      _results' -> map formatStationResult results

    formatStationResult :: (Int, String) -> Text
    formatStationResult (sId, sName) = pack $ show sId <> ": " <> sName


    cliOut :: MonadIO m => [Text] -> m ()
    cliOut = mapM_ (liftIO . putStrLn . unpack)

    showStationStatus :: Connection -> TimeZone -> (Int, String) -> IO ()
    showStationStatus conn' currentTimeZone' (id', name') = do
      latest <- queryStationStatusLatest conn' id'
      let status = fmap (currentTimeZone', name', ) latest
      putStrLn (unpack $ unlines $ formatStationStatusResult status)


formatStationStatusResult :: Maybe (TimeZone, String, StationStatus) -> [Text]
formatStationStatusResult = maybe ["No status found."] formatStationInfo

formatStationInfo :: (TimeZone, String, StationStatus) -> [Text]
formatStationInfo (timeZone, name, status) =
    let pairs = [("Bikes:\t", status ^. d_status_num_bikes_available, status ^. d_status_num_bikes_disabled),
                 ("Docks:\t", status ^. d_status_num_docks_available, status ^. d_status_num_docks_disabled)]
    in [formattedName name status, formattedLastReport timeZone $ reportToLocal <$> status ^. d_status_last_reported] ++ map fmtAvailability pairs

formattedName :: String -> StationStatus -> Text
formattedName name status =
    format "{}[{}]{} {}{}{}" boldCode (status ^. d_status_station_id) resetIntens underCode name resetUnder

-- Format the last reported time in the specified time zone (namerly, the system's time zone).
formattedLastReport :: TimeZone -> Maybe LocalTime -> Text
formattedLastReport timeZone status = reportedText
  where
    reportedText = case status of
      Nothing -> boldCode <> colouredText' Red "Never" <> resetIntens
      -- Just t  -> italCode <> showText t <> resetItal
      Just t  -> "[" <> showText t <> "]"
              <> boldCode <> "\t" <> "|" <> "\t" <> resetIntens
              <> italCode <> pack (formatTime' t) <> resetItal <> " (local)"
    timeFormat = "%A, %b %e, %T" -- DayOfWeek Month Day Hour:Minute:Second
    formatTime' t = formatTime defaultTimeLocale timeFormat $ localToSystem timeZone t

showText :: Show a => a -> Text
showText = pack . show

fmtAvailability :: (Text, Int32, Int32) -> Text
fmtAvailability (name, avail, disable)
  = colouredText' Green name
 <> colouredText Green avail <> " available" <> tab
 <> boldCode <> "|" <> tab <> resetIntens
 <> colouredText Red disable <> " disabled"
 where tab = "\t" :: Text

colouredText :: Show a => Color -> a -> Text
colouredText colour = colouredText' colour . pack . show

colouredText' :: Color -> Text -> Text
colouredText' colour text = format "{}{}{}" (setSGRCode [SetColor Foreground Vivid colour]) text (setSGRCode [])

boldCode, resetIntens, italCode, resetItal, underCode, resetUnder :: Text
boldCode    = pack $ setSGRCode [ SetConsoleIntensity  BoldIntensity   ]
resetIntens = pack $ setSGRCode [ SetConsoleIntensity  NormalIntensity ]
italCode    = pack $ setSGRCode [ SetItalicized        True            ]
resetItal   = pack $ setSGRCode [ SetItalicized        False           ]
underCode   = pack $ setSGRCode [ SetUnderlining       SingleUnderline ]
resetUnder  = pack $ setSGRCode [ SetUnderlining       NoUnderline     ]
