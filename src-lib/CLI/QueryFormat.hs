-- | This module defines the formatters for the command line interface.

module CLI.QueryFormat where

import           AppEnv

import           CLI.Utils

import           Control.Lens

import           Data.Int                      ( Int32 )
import           Data.Text.Lazy                ( Text, chunksOf, intercalate, pack, reverse, toStrict, unlines, unpack,
                                                 unwords )
import           Data.Time                     ( LocalTime (..), TimeZone )

import           Database.BikeShare            ( StationStatus, d_status_is_charging_station, d_status_last_reported,
                                                 d_status_num_bikes_available, d_status_num_bikes_disabled,
                                                 d_status_num_docks_available, d_status_num_docks_disabled,
                                                 d_status_station_id, vehicle_types_available_efit,
                                                 vehicle_types_available_efit_g5, vehicle_types_available_iconic )
import           Database.BikeShare.Operations

import           Fmt

import           Prelude                       hiding ( log, reverse, unlines, unwords )

import           ReportTime                    ( reportToLocal )

import           System.Console.ANSI

import           UnliftIO                      ( liftIO )


-- | Helper function to show a value as 'Text'.
showText :: Show a => a -> Text
showText = pack . show

-- | Format text with ANSI colour codes.
colouredText :: ColorIntensity -> Color -> Text -> Text
colouredText intensity colour text = format "{}{}{}" (setSGRCode [SetColor Foreground intensity colour]) text (setSGRCode [])

-- | Helper functions for Formatting text with ANSI colour codes.
boldCode, resetIntens, italCode, resetItal, underCode, resetUnder :: Text
boldCode    = pack $ setSGRCode [ SetConsoleIntensity  BoldIntensity   ]
resetIntens = pack $ setSGRCode [ SetConsoleIntensity  NormalIntensity ]
italCode    = pack $ setSGRCode [ SetItalicized        True            ]
resetItal   = pack $ setSGRCode [ SetItalicized        False           ]
underCode   = pack $ setSGRCode [ SetUnderlining       SingleUnderline ]
resetUnder  = pack $ setSGRCode [ SetUnderlining       NoUnderline     ]

-- | Lookup local time zone.
fmtStationStatus :: TimeZone -> (Int, String) -> App [Text]
fmtStationStatus currentTimeZone' (id', name') = do
  latest <- queryStationStatusLatest <$> withConn <*> pure id' >>= liftIO
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
    in [ formattedName name status
       , formattedLastReport timeZone $ reportToLocal <$> status ^. d_status_last_reported
       , "Charger:\t" <> formattedBool (status ^. d_status_is_charging_station)
       ] ++ map fmtAvailability pairs ++ bikeAvailability

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

formattedBool :: Bool -> Text
formattedBool b = if b
  then boldCode <> colouredText Vivid Green "Yes" <> resetIntens
  else colouredText Dull White   "No"

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

withHeader :: Text -> [Text] -> [Text]
withHeader header' results = case results of
  [] -> fmtHeader : ["\t\t" <> indent 6 <> italCode <> " None." <> resetItal ]
  _  -> fmtHeader : results
  where
    fmtHeader = "\t" <> indent 5 <> indent 8 <> " " <> boldCode <> underCode <> header' <> resetUnder <> resetIntens <> " " <> indent 8

-- | Print a list of 'Text' lines to the console.
cliOut :: [Text] -> IO ()
cliOut = putStrLn . unpack . unlines

-- | Format a number with thousands separators.
prettyNum :: Integer -> Text
prettyNum = unwords . chunksOf 3 . reverse . pack . show
