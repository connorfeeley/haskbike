{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeApplications          #-}

-- Signatures of beam-related functions are incredibly verbose, so let's settle for partial type signatures.
-- Sometimes it is straight up impossible to write the types down because of ambiguous types.
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | This module contains the operations that can be performed on the database.

module Database.BikeShare.Operations
     ( module Database.BikeShare.Operations.Dockings
     , insertStationInformation
     , insertStationStatus
     , printDisabledDocks
     , queryDisabledDocks
     , queryRowCount
     , queryStationId
     , queryStationIdLike
     , queryStationInformation
     , queryStationInformationByIds
     , queryStationName
     , queryStationStatus
     , queryStationStatusBetween
     , queryStationStatusFields
     , queryStationStatusLatest
     , querySystemStatusAtRange
     , queryTableSize
     ) where

import qualified API.Types                                as AT

import           AppEnv

import           Colog                                    ( logException )

import           Control.Lens                             hiding ( reuse, (<.) )
import           Control.Monad.Catch

import           Data.Int                                 ( Int32 )
import           Data.Maybe                               ( mapMaybe )
import qualified Data.Text                                as Text
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.BikeShare.Expressions
import           Database.BikeShare.Operations.Dockings
import           Database.PostgreSQL.Simple               ( Only (..), query_ )

import           Formatting

import           GHC.Exts                                 ( fromString )

import           Prelude                                  hiding ( log )

-- | Query database for disabled docks, returning tuples of (name, num_docks_disabled).
queryDisabledDocks :: AppM [(Text.Text, Int32)] -- ^ List of tuples of (name, num_docks_disabled).
queryDisabledDocks = withPostgres $ runSelectReturningList $ select disabledDocksExpr

-- | Helper function to print disabled docks.
printDisabledDocks :: AppM ()
printDisabledDocks = queryDisabledDocks >>= pPrintCompact

-- | Query database for station status.
queryStationStatus :: Maybe Integer                              -- ^ Limit number of rows returned.
                   -> AppM [(StationInformation, StationStatus)] -- ^ List of tuples of (station information, station status).
queryStationStatus limit =
  withPostgres $ runSelectReturningList $ select $
  queryStationStatusExpr limit

queryStationStatusFields =
  withPostgres $ runSelectReturningList $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- all_ (bikeshareDb ^. bikeshareStationStatus)
  guard_ (_statusStationId status `references_` info)
  pure ( info   ^. infoName
       , status ^. statusNumBikesAvailable
       , status ^. statusNumBikesDisabled
       , status ^. statusNumDocksAvailable
       , status ^. statusNumDocksDisabled
       )


{- | Query database for all station information. -}
queryStationInformation :: AppM [StationInformation] -- ^ List of station information.
queryStationInformation =
  withPostgres $ runSelectReturningList $ select $ all_ (bikeshareDb ^. bikeshareStationInformation)

{- | Query database for station information corresponding to a list of station IDs. -}
queryStationInformationByIds :: [Int]                   -- ^ List of station IDs to query.
                             -> AppM [StationInformation] -- ^ List of station information.
queryStationInformationByIds ids =
  withPostgres $ runSelectReturningList $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_infoStationId info `in_` ids')
  pure info
  where
    ids' = fromIntegral <$> ids

-- | Insert station information into the database.
insertStationInformation :: [AT.StationInformation]  -- ^ List of 'StationInformation' from the API response.
                         -> AppM [StationInformation] -- ^ List of 'StationInformation' that where inserted.
insertStationInformation stations =
  withPostgres $ runInsertReturningList $ insertStationInformationExpr stations

{- |
Insert station statuses into the database.
-}
insertStationStatus :: [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response.
                    -> AppM [StationStatus]
insertStationStatus apiStatus
  | null apiStatus = pure []
  | otherwise = do
      result <- try $ withPostgres $ runInsertReturningList $
          insertOnConflict (bikeshareDb ^. bikeshareStationStatus)
          (insertExpressions (mapMaybe fromJSONToBeamStationStatus apiStatus)
         ) anyConflict onConflictDoNothing

      case result of
        Left (e :: SqlError) ->
          logException e >>
          throwM e >>
          pure []
        Right inserted       -> pure inserted

{- |
Query the statuses for a station between two times.
-}
queryStationStatusBetween :: Int                  -- ^ Station ID.
                          -> UTCTime              -- ^ Start time.
                          -> UTCTime              -- ^ End time.
                          -> AppM [StationStatus] -- ^ List of 'StationStatus' for the given station between the given times.
queryStationStatusBetween stationId startTime endTime =
  withPostgres $ runSelectReturningList $ select $
  statusBetweenExpr (fromIntegral stationId) startTime endTime

{- |
Query the station name given a station ID.
-}
queryStationName :: Int                 -- ^ Station ID.
                 -> AppM (Maybe String) -- ^ Station name assosicated with the given station ID.
queryStationName stationId = do
  info <- withPostgres $ runSelectReturningOne $ select $ infoByIdExpr [fromIntegral stationId]

  let station_name = info ^. _Just . infoName

  pure $ Just $ Text.unpack station_name


{- |
Query the station ID for given a station name, using SQL `=` semantics.

== __Examples__
Get ID for "King St W / Joe Shuster Way":

>>> queryStationId "King St W / Joe Shuster Way"
Just 7148

Get ID for "Wellesley Station Green P":

>>> queryStationId "Wellesley Station Green P"
Just 7001
-}
queryStationId :: String           -- ^ Station ID.
               -> AppM (Maybe Int) -- ^ Station ID assosicated with the given station name, if found.
queryStationId stationName = do
  info <- withPostgres $ runSelectReturningOne $ select $ queryStationIdExpr stationName

  pure $ fromIntegral <$> info ^? _Just . infoStationId


{- | Query possible station IDs matching a given station name, using SQL `LIKE` semantics.

== __Examples__

Search for station names ending with "Green P":

>>> queryStationIdLike "%Green P"
[ (7001,"Wellesley Station Green P")
, (7050,"Richmond St E / Jarvis St Green P")
, (7112,"Liberty St / Fraser Ave Green P")
, (7789,"75 Holly St - Green P") ]

Search for station names containing "Joe Shuster":

>>> queryStationIdLike "%Joe Shuster%"
[(7148,"King St W / Joe Shuster Way")]

__Return:__ Tuples of (station ID, station name) matching the searched name, using SQL `LIKE` semantics.
-}
queryStationIdLike :: String               -- ^ Station ID.
                   -> AppM [(Int, String)]  -- ^ Tuples of (station ID, name) for stations that matched the query.
queryStationIdLike stationName = do
  info <- withPostgres $ runSelectReturningList $ select $ queryStationIdLikeExpr stationName

  -- Return tuples of (station_id, station_name)
  pure $ map (\si -> ( si ^. infoStationId & fromIntegral
                     , si ^. infoName & Text.unpack
                     )) info

-- | Query the latest status for a station.
queryStationStatusLatest :: Int                       -- ^ Station ID.
                         -> AppM (Maybe StationStatus) -- ^ Latest 'StationStatus' for the given station.
queryStationStatusLatest station_id = withPostgres $ runSelectReturningOne $ select $ limit_ 1 $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_infoStationId info ==. val_ ( fromIntegral station_id))
  status <- orderBy_ (desc_ . _statusLastReported)
            (all_ (bikeshareDb ^. bikeshareStationStatus))
  guard_ (_statusStationId status `references_` info)
  pure status

-- | Count the number of rows in a given table.
queryRowCount :: (Beamable table, Database Postgres db)
              => Getting (DatabaseEntity Postgres db (TableEntity table)) (DatabaseSettings be BikeshareDb) (DatabaseEntity Postgres db (TableEntity table))
              -- ^ Lens to the table in the database.
              -> AppM (Maybe Int32)     -- ^ Count of rows in the specified table.
queryRowCount table = withPostgres $ runSelectReturningOne $ select $
  aggregate_ (\_ -> as_ @Int32 countAll_) (all_ (bikeshareDb ^. table))

-- | Function to query the size of a table.
queryTableSize :: String                -- ^ Name of the table.
               -> AppM (Maybe String)    -- ^ Size of the table.
queryTableSize tableName = do
  conn <- withConn
  [Only size] <- liftIO $ query_ conn $ fromString ("SELECT pg_size_pretty(pg_total_relation_size('" ++ tableName ++ "'))")
  return size

querySystemStatusAtRange :: UTCTime -> UTCTime -> Int
                        -> AppM [(UTCTime, Integer, Integer, Integer, Integer, Integer, Integer, Integer)]
querySystemStatusAtRange earliestTime latestTime intervalMins = do
  -- Execute query expression, returning 'Just (tup)' if one row was returned; otherwise 'Nothing'.
  statusAtTime <-
    withPostgres $ runSelectReturningList $ selectWith $
    querySystemStatusAtRangeExpr earliestTime latestTime intervalMins

  (pure . map statusFieldsToInt) statusAtTime
  where
    -- Convert fields from 'Int32' to 'Int'.
    statusFieldsToInt =
      (\statusAtTime -> (               statusAtTime ^. _1
                        , fromIntegral (statusAtTime ^. _2)
                        , fromIntegral (statusAtTime ^. _3)
                        , fromIntegral (statusAtTime ^. _4)
                        , fromIntegral (statusAtTime ^. _5)
                        , fromIntegral (statusAtTime ^. _6)
                        , fromIntegral (statusAtTime ^. _7)
                        , fromIntegral (statusAtTime ^. _8)
      ))
