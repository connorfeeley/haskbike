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
import qualified Data.List              as List
import           Data.Text.Lazy         ( Text, intercalate, pack, toStrict, unlines, unpack )
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
  resultsEnds      <- liftIO $ queryStationIdLike conn (nameTransformer "%" stationName "")
  resultsBegins    <- liftIO $ queryStationIdLike conn (nameTransformer ""  stationName "%")

  log D $ toStrict $ "Wildcard: "    <> (pack . show) resultsAnywhere
  log D $ toStrict $ "Ends with: "   <> (pack . show) resultsEnds
  log D $ toStrict $ "Begins with: " <> (pack . show) resultsBegins

  anywhereText  <- liftIO $ mapM (fmtStationStatus conn currentTimeZone) resultsAnywhere
  endsText      <- liftIO $ mapM (fmtStationStatus conn currentTimeZone) resultsEnds
  beginsText    <- liftIO $ mapM (fmtStationStatus conn currentTimeZone) resultsBegins

  -- Print formatted results to the console.
  liftIO $ do
    putStrLn $ unpack $ unlines $ withHeader "Wildcard"  (concat anywhereText)
    putStrLn $ unpack $ unlines $ withHeader "  Ends  "  (concat endsText)
    putStrLn $ unpack $ unlines $ withHeader " Begins "  (concat beginsText)

  where
    withHeader :: Text -> [Text] -> [Text]
    withHeader header results = case results of
      [] -> fmtHeader : ["\t\t" <> indent 6 <> italCode <> " None." <> resetItal ]
      _  -> fmtHeader : results
      where
        fmtHeader = "\t" <> indent 5 <> indent 8 <> " " <> boldCode <> underCode <> header <> resetUnder <> resetIntens <> " " <> indent 8
        indent (n :: Int) = pack (unwords (replicate n ""))
        boxLine n = intercalate "" (replicate n boxDrawingCharacter)
        boxDrawingCharacter = "\x2500" :: Text -- Unicode U+2500 (Box Drawing Character)
    nameTransformer prepend name append = List.intercalate "" [prepend, name, append]

    fmtStationStatus :: Connection -> TimeZone -> (Int, String) -> IO [Text]
    fmtStationStatus conn' currentTimeZone' (id', name') = do
      latest <- queryStationStatusLatest conn' id'
      let status = fmap (currentTimeZone', name', ) latest
      pure $ formatStationStatusResult status

formatStationStatusResult :: Maybe (TimeZone, String, StationStatus) -> [Text]
formatStationStatusResult = maybe ["No status found."] formatStationInfo

formatStationInfo :: (TimeZone, String, StationStatus) -> [Text]
formatStationInfo (timeZone, name, status) =
    let pairs = [("Bikes:\t", status ^. d_status_num_bikes_available, status ^. d_status_num_bikes_disabled),
                 ("Docks:\t", status ^. d_status_num_docks_available, status ^. d_status_num_docks_disabled)]
    in [formattedName name status, formattedLastReport timeZone $ reportToLocal <$> status ^. d_status_last_reported] ++ map fmtAvailability pairs

formattedName :: String -> StationStatus -> Text
formattedName name status =
    format "{}[{}{}]{} {}{}{}" boldCode idPrefix (status ^. d_status_station_id) resetIntens underCode name resetUnder
    where idPrefix = resetIntens <> "# " <> boldCode

-- Format the last reported time in the specified time zone (namerly, the system's time zone).
formattedLastReport :: TimeZone -> Maybe LocalTime -> Text
formattedLastReport timeZone status = reportedText
  where
    reportedText = case status of
      Nothing -> boldCode <> colouredText' Vivid Red "Never" <> resetIntens
      -- Just t  -> italCode <> showText t <> resetItal
      Just t  -> "[" <> showText t <> "]"
              <> boldCode <> "\t" <> "|" <> "\t" <> resetIntens
              <> italCode <> pack (formatTime' t) <> resetItal <> " (local)"
    timeFormat = "%A, %b %e, %T" -- DayOfWeek Month Day Hour:Minute:Second
    formatTime' t = formatTime defaultTimeLocale timeFormat $ localToSystem timeZone t

showText :: Show a => a -> Text
showText = pack . show

fmtAvailability :: (Text, Int32, Int32) -> Text
fmtAvailability (name, avail, disabled)
  = colouredText' Dull Yellow name
 <> colouredText' Vivid Green  (fmt $ padLeftF 2 ' ' avail)    <> " available" <> tab
 <> boldCode <> "|" <> tab <> resetIntens
 <> colouredText' Vivid Red    (fmt $ padLeftF 2 ' ' disabled) <> " disabled"
 where tab = "\t" :: Text

colouredText :: Show a => ColorIntensity -> Color -> a -> Text
colouredText intensity colour = colouredText' intensity colour . pack . show

colouredText' :: ColorIntensity -> Color -> Text -> Text
colouredText' intensity colour text = format "{}{}{}" (setSGRCode [SetColor Foreground intensity colour]) text (setSGRCode [])

boldCode, resetIntens, italCode, resetItal, underCode, resetUnder :: Text
boldCode    = pack $ setSGRCode [ SetConsoleIntensity  BoldIntensity   ]
resetIntens = pack $ setSGRCode [ SetConsoleIntensity  NormalIntensity ]
italCode    = pack $ setSGRCode [ SetItalicized        True            ]
resetItal   = pack $ setSGRCode [ SetItalicized        False           ]
underCode   = pack $ setSGRCode [ SetUnderlining       SingleUnderline ]
resetUnder  = pack $ setSGRCode [ SetUnderlining       NoUnderline     ]
