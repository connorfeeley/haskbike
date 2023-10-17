-- | This module defines the formatters for the command line interface.

module CLI.QueryFormat where

import           CLI.Utils

import           Control.Lens

import           Data.Int            ( Int32 )
import           Data.Text.Lazy      ( Text, chunksOf, pack, reverse, unlines, unpack, unwords )
import           Data.Time           ( TimeZone, UTCTime )

import           Database.BikeShare

import           Fmt

import           Prelude             hiding ( log, reverse, unlines, unwords )

import           System.Console.ANSI



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


formatStationStatusResult :: Maybe (TimeZone, String, StationStatus) -> [Text]
formatStationStatusResult = maybe ["No status found."] formatStationInfo

formatStationInfo :: (TimeZone, String, StationStatus) -> [Text]
formatStationInfo (timeZone, name, status) =
  let
    bikeAvailability = [ fmtBikeAvailability "Iconic"   (status ^. vehicleTypesAvailableIconic)
                       -- , fmtBikeAvailability "Boost"    (status ^. vehicle_types_available_boost) -- Not used in Toronto.
                       , fmtBikeAvailability "E-Fit"    (status ^. vehicleTypesAvailableEfit)
                       , fmtBikeAvailability "E-Fit G5" (status ^. vehicleTypesAvailableEfitG5)]

    pairs = [ ("Docks:\t", status ^. statusNumDocksAvailable, fromIntegral $ status ^. statusNumDocksDisabled)
            , ("Bikes:\t", status ^. statusNumBikesAvailable, fromIntegral $ status ^. statusNumBikesDisabled)
            ]
    in [ formattedName name status
       , formattedLastReport timeZone $ status ^. statusLastReported
       , "Charger:\t" <> formattedBool (status ^. statusIsChargingStation)
       ] ++ map fmtAvailability pairs ++ bikeAvailability

formattedName :: String -> StationStatus -> Text
formattedName name status =
    format "{}[{}{}]{} {}{}{}" boldCode idPrefix (status ^. statusStationId . unInformationStationId) resetIntens underCode name resetUnder
    where idPrefix = resetIntens <> "# " <> boldCode

-- Format the last reported time in the specified time zone (namerly, the system's time zone).
formattedLastReport :: TimeZone -> UTCTime -> Text
formattedLastReport _timeZone status = reportedText
  where
    reportedText = "[" <> showText status <> "]"
                <> boldCode <> "\t" <> "|" <> "\t" <> resetIntens
                <> italCode <> pack (formatTime' status) <> resetItal <> " (local)"

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
