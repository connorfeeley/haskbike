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

import           ReportTime             ( localToPosix, localToSystem )

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
formatStationStatusResult = maybe ["No status found."] $ \s ->
                [ format "{}[{}]{} {}{}{}"
                  boldCode (s ^. _2 . d_status_station_id) resetIntens
                  underCode (s ^. _1) resetUnder
                , "Last reported: " <> maybe "Never" (Text.pack . show) (s ^. _2 . d_status_last_reported)
                ] <> fmtVehiclesAvailable (s ^. _2)


fmtVehiclesAvailable :: StationStatus -> [Text.Text]
fmtVehiclesAvailable s =
  [ Text.pack ("Bikes: " <> (setSGRCode [SetColor Foreground Vivid Green] <> show  (s ^. d_status_num_bikes_available)  <> setSGRCode [])) <> " available"
 <> Text.pack (setSGRCode [SetConsoleIntensity BoldIntensity] <> " | " <> setSGRCode [])
 <> Text.pack (setSGRCode [SetColor Foreground Vivid Red] <> show  (s ^. d_status_num_bikes_disabled) <> setSGRCode [] <> " disabled")

  , Text.pack ("Docks: " <> (setSGRCode [SetColor Foreground Vivid Green] <> show  (s ^. d_status_num_docks_available)  <> setSGRCode [])) <> " available"
 <> Text.pack (setSGRCode [SetConsoleIntensity BoldIntensity] <> " | " <> setSGRCode [])
 <> Text.pack (setSGRCode [SetColor Foreground Vivid Red] <> show  (s ^. d_status_num_docks_disabled) <> setSGRCode [] <> " disabled")
  ]

showText :: Show a => a -> Text.Text
showText = Text.pack . show

showTextColour :: Show a => Color -> a -> Text.Text
showTextColour colour text = format "{}{}{}" (setSGRCode [SetColor Foreground Vivid colour]) (show text) (setSGRCode [])

boldCode :: String
boldCode = setSGRCode [ SetConsoleIntensity BoldIntensity ]
resetIntens :: String
resetIntens = setSGRCode [ SetConsoleIntensity NormalIntensity ]

underCode :: String
underCode = setSGRCode [ SetUnderlining SingleUnderline ]
resetUnder :: String
resetUnder = setSGRCode [ SetUnderlining NoUnderline ]
