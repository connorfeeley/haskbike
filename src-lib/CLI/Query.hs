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
import qualified Data.Text              as Text

import           Database.Beam.Postgres ( Connection )
import           Database.BikeShare     ( StationStatus, d_status_last_reported, d_status_num_bikes_available,
                                          d_status_num_bikes_disabled, d_status_num_docks_available,
                                          d_status_num_docks_disabled, d_status_station_id )
import           Database.Operations
import           Database.Utils         ( pPrintCompact )

import           Fmt

import           Prelude                hiding ( log )

import           ReportTime             ( localToPosix, localToSystem, reportToLocal )

import           System.Console.ANSI

import           UnliftIO               ( MonadIO, MonadUnliftIO, liftIO )
import Data.Time (formatTime, defaultTimeLocale, LocalTime (..))

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
  log I $ "Querying station ID '" <> (Text.pack . show) stationId <> "'"
  name <- liftIO $ queryStationName conn stationId
  log I $ "Station : " <> (Text.pack . show) name
  pPrintCompact name

queryByStationName :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                   => String
                   -> Connection
                   -> m ()
queryByStationName stationName conn = do
  log I $ "Querying station names like '" <> Text.pack stationName <> "'"
  resultsAnywhere  <- liftIO $ queryStationIdLike conn (nameTransformer "%" stationName "%")
  resultsBegins    <- liftIO $ queryStationIdLike conn (nameTransformer ""  stationName "%")
  resultsEnds      <- liftIO $ queryStationIdLike conn (nameTransformer "%" stationName "")

  log D $ "Wildcard: "    <> (Text.pack . show) resultsAnywhere
  log D $ "Begins with: " <> (Text.pack . show) resultsBegins
  log D $ "Ends with: "   <> (Text.pack . show) resultsEnds

  cliOut $ "" : "Matched anywhere: " : formatStationResults resultsAnywhere
  cliOut $ "" : "Begins with: "      : formatStationResults resultsBegins
  cliOut $ "" : "Ends with: "        : formatStationResults resultsEnds

  liftIO $ mapM_ (showStationStatus conn) resultsAnywhere

  where
    nameTransformer prepend name append = intercalate "" [prepend, name, append]

    formatStationResults :: [(Int, String)] -> [Text.Text]
    formatStationResults results = case results of
      []       -> ["No results found."]
      results' -> map formatStationResult results

    formatStationResult :: (Int, String) -> Text.Text
    formatStationResult (sId, sName) = Text.pack $ show sId <> ": " <> sName


    cliOut :: MonadIO m => [Text.Text] -> m ()
    cliOut = mapM_ (liftIO . putStrLn . Text.unpack)

    showStationStatus :: Connection -> (Int, String) -> IO ()
    showStationStatus conn' (id', name') = do
      latest <- queryStationStatusLatest conn' id'
      let status = fmap (name', ) latest
      putStrLn (Text.unpack $ Text.unlines $ formatStationStatusResult status)


formatStationStatusResult :: Maybe (String, StationStatus) -> [Text.Text]
formatStationStatusResult = maybe ["No status found."] formatStationInfo

formatStationInfo :: (String, StationStatus) -> [Text.Text]
formatStationInfo (name, status) =
    let pairs = [("Bikes:\t", status ^. d_status_num_bikes_available, status ^. d_status_num_bikes_disabled),
                 ("Docks:\t", status ^. d_status_num_docks_available, status ^. d_status_num_docks_disabled)]
    in [formattedName name status, formattedLastReport status] ++ map fmtAvailability pairs

formattedName :: String -> StationStatus -> Text.Text
formattedName name status =
    format "{}[{}]{} {}{}{}" boldCode (status ^. d_status_station_id) resetIntens underCode name resetUnder

formattedLastReport :: StationStatus -> Text.Text
formattedLastReport status = reportedText
  where
    reportedText = case status ^. d_status_last_reported of
      Nothing -> boldCode <> colouredText' Red "Never" <> resetIntens
      -- Just t  -> italCode <> showText t <> resetItal
      Just t  -> "[" <> showText t <> "]"
              <> boldCode <> "\t" <> "|" <> "\t" <> resetIntens
              <> italCode <> Text.pack (formatTime' t) <> resetItal
    timeFormat = "%A, %b %e, %T" -- DayOfWeek Month Day Hour:Minute:Second
    formatTime' t = formatTime defaultTimeLocale timeFormat $ reportToLocal t

showText :: Show a => a -> Text.Text
showText = Text.pack . show

fmtAvailability :: (Text.Text, Int32, Int32) -> Text.Text
fmtAvailability (name, avail, disable)
  = colouredText' Green name
 <> colouredText Green avail <> " available" <> tab
 <> boldCode <> "|" <> tab <> resetIntens
 <> colouredText Red disable <> " disabled"
 where tab = "\t" :: Text.Text

colouredText :: Show a => Color -> a -> Text.Text
colouredText colour = colouredText' colour . Text.pack . show

colouredText' :: Color -> Text.Text -> Text.Text
colouredText' colour text = format "{}{}{}" (setSGRCode [SetColor Foreground Vivid colour]) text (setSGRCode [])

boldCode, resetIntens, italCode, resetItal, underCode, resetUnder :: Text.Text
boldCode    = Text.pack $ setSGRCode [ SetConsoleIntensity  BoldIntensity   ]
resetIntens = Text.pack $ setSGRCode [ SetConsoleIntensity  NormalIntensity ]
italCode    = Text.pack $ setSGRCode [ SetItalicized        True            ]
resetItal   = Text.pack $ setSGRCode [ SetItalicized        False           ]
underCode   = Text.pack $ setSGRCode [ SetUnderlining       SingleUnderline ]
resetUnder  = Text.pack $ setSGRCode [ SetUnderlining       NoUnderline     ]
