{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
CLI interface for querying the database.
-}
module CLI.Query
     ( dispatchQuery
     ) where

import           API.Client
import           API.ResponseWrapper
import           API.Types

import           CLI.Options            ( QueryOptions (..) )

import           Colog                  ( Message, Msg (msgText), WithLog, cmap, log, logException, pattern D,
                                          pattern E, pattern I, withLog )

import           Control.Concurrent     ( threadDelay )
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad          ( void )
import           Control.Monad.Cont     ( forever )

import           Data.List              ( intercalate )
import qualified Data.Text              as Text

import           Database.Beam.Postgres ( Connection )
import           Database.BikeShare     ( d_status_last_reported, d_status_station_id, fromBeamStationStatusToJSON )
import           Database.Operations
import           Database.Utils         ( pPrintCompact )

import           Fmt

import           Prelude                hiding ( log )

import           ReportTime             ( localToPosix, localToSystem )

import           Text.Pretty.Simple     ( pPrintString )

import           UnliftIO               ( MonadIO, MonadUnliftIO, liftIO )
import           UnliftIO.Async         ( concurrently_ )

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

  latest <- liftIO $ queryStationStatusLatest conn (head resultsAnywhere ^. _1)
  liftIO $ putStrLn ("Latest status:" :: String)
  liftIO $ putStrLn (Text.unpack $ formatStationStatusResult $ fromBeamStationStatusToJSON <$> latest)

  where
    nameTransformer prepend name append = intercalate "" [prepend, name, append]

    formatStationResults :: [(Int, String)] -> [Text.Text]
    formatStationResults results = case results of
      []       -> ["No results found."]
      results' -> map formatStationResult results

    formatStationResult :: (Int, String) -> Text.Text
    formatStationResult (sId, sName) = Text.pack $ show sId <> ": " <> sName

    formatStationStatusResult :: Maybe StationStatus -> Text.Text
    formatStationStatusResult status = case status of
      Nothing -> Text.pack "No status found."
      Just status' -> Text.pack $ fmt $ nameF "station" $ blockListF
        [ "Station ID: " <> (Text.pack . show $ status' ^. status_station_id)
        -- FIXME: this is hideous.
        -- FIXME: just dump the database type directly.
        , Text.pack (fmt $ blockListF $ map formatStationStatusAvailableResult (status' ^. status_vehicle_types_available))]

    -- FIXME: this too is hideous.
    formatStationStatusAvailableResult :: VehicleType -> Text.Text
    formatStationStatusAvailableResult available =
      Text.pack $ fmt $ blockListF (vehicle_type_id available, type_count available)

    cliOut :: MonadIO m => [Text.Text] -> m ()
    cliOut = mapM_ (liftIO . putStrLn . Text.unpack)
