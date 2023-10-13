{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}

-- Signatures of beam-related functions are incredibly verbose, so let's settle for partial type signatures.
-- Sometimes it is straight up impossible to write the types down because of ambiguous types.
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | This module contains the operations that can be performed on the database.

module Database.BikeShare.Operations
     ( module Database.BikeShare.Operations.Dockings
     , FilterStatusResult (..)
     , InsertStatusResult (..)
     , filter_newer
     , filter_unchanged
     , getRowsToDeactivate
     , insertStationInformation
     , insertStationStatus
     , insert_deactivated
     , insert_inserted
     , printDisabledDocks
     , queryAllStationsStatusBeforeTime
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
     , queryTableSize
     , separateNewerStatusRecords
     ) where

import           API.Types                                ( _status_last_reported, _status_station_id,
                                                            status_station_id )
import qualified API.Types                                as AT

import           AppEnv

import           Colog                                    ( log, pattern W )

import           Control.Lens                             hiding ( reuse, (<.) )
import           Control.Monad.RWS                        ( modify )

import           Data.Int                                 ( Int32 )
import qualified Data.Map                                 as Map
import qualified Data.Text                                as Text

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres                   as Pg
import qualified Database.Beam.Postgres.Full              as Pg
import           Database.BikeShare
import           Database.BikeShare.Expressions
import           Database.BikeShare.Operations.Dockings
import           Database.PostgreSQL.Simple               ( Only (..), query_ )

import           Fmt

import           Formatting

import           GHC.Exts                                 ( fromString )

import           Prelude                                  hiding ( log )


data FilterStatusResult where
  FilterStatusResult :: { _filter_newer         :: [AT.StationStatus] -- ^ List of 'AT.StationStatus' that were updated.
                        , _filter_unchanged     :: [AT.StationStatus] -- ^ List of 'AT.StationStatus' that were not updated.
                        } -> FilterStatusResult
  deriving (Show)
makeLenses ''FilterStatusResult

{- | Data type representing the result of inserting updated station statuses.

When inserting to an empty database, all statuses are inserted.
When inserting to a non-empty database with the full 'AT.StationStatusResponse' data,
'_insert_deactivated' and '_insert_inserted' will be the same length.
-}
data InsertStatusResult where
  InsertStatusResult :: { _insert_deactivated  :: [StationStatus] -- ^ List of 'StationStatus' that were updated.
                        , _insert_inserted     :: [StationStatus] -- ^ List of station statuses that were inserted.
                        } -> InsertStatusResult
  deriving (Show)
makeLenses ''InsertStatusResult

-- | Query database for disabled docks, returning tuples of (name, num_docks_disabled).
queryDisabledDocks :: App [(Text.Text, Int32)] -- ^ List of tuples of (name, num_docks_disabled).
queryDisabledDocks = do
  withPostgres $ runSelectReturningList $ select disabledDocksExpr

-- | Helper function to print disabled docks.
printDisabledDocks :: App ()
printDisabledDocks = queryDisabledDocks >>= pPrintCompact

-- | Query database for station status.
queryStationStatus :: Maybe Integer                            -- ^ Limit number of rows returned.
                   -> App [(StationInformation, StationStatus)] -- ^ List of tuples of (station information, station status).
queryStationStatus limit =
  withPostgres $ runSelectReturningList $ select $
  queryStationStatusExpr limit

queryStationStatusFields =
  withPostgres $ runSelectReturningList $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- all_ (bikeshareDb ^. bikeshareStationStatus)
  guard_ (_d_status_info_id status `references_` info)
  pure ( info^.info_name
       , status^.d_status_num_bikes_available
       , status^.d_status_num_bikes_disabled
       , status^.d_status_num_docks_available
       , status^.d_status_num_docks_disabled
       )


{- | Query database for all station information. -}
queryStationInformation :: App [StationInformation] -- ^ List of station information.
queryStationInformation =
  withPostgres $ runSelectReturningList $ select $ all_ (bikeshareDb ^. bikeshareStationInformation)

{- | Query database for station information corresponding to a list of station IDs. -}
queryStationInformationByIds :: [Int]                   -- ^ List of station IDs to query.
                             -> App [StationInformation] -- ^ List of station information.
queryStationInformationByIds ids =
  withPostgres $ runSelectReturningList $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_info_station_id info `in_` ids')
  pure info
  where
    ids' = fromIntegral <$> ids

-- | Insert station information into the database.
insertStationInformation :: [AT.StationInformation]  -- ^ List of 'StationInformation' from the API response.
                         -> App [StationInformation] -- ^ List of 'StationInformation' that where inserted.
insertStationInformation stations =
  withPostgres $ runInsertReturningList $ insertStationInformationExpr stations

-- | Find the corresponding active rows in the database corresponding to each element of a list of *newer* 'AT.StationStatus' records.
getRowsToDeactivate :: [AT.StationStatus]  -- ^ List of 'AT.StationStatus' from the API response.
                    -> App [StationStatus] -- ^ List of 'StationStatus' rows that would need to be deactivated, if the statuses from the API were inserted.
getRowsToDeactivate api_status
  | null api_status = return []
  | otherwise = do
    -- Select using common table expressions (selectWith).
    withPostgres $ runSelectReturningList $ selectWith $ do
      -- Common table expression for 'StationStatus'.
      common_status <- selecting $ do
        info <- infoByIdExpr (map (fromIntegral . _status_station_id) api_status)
        status <- orderBy_ (asc_ . _d_status_id) $
          all_ (bikeshareDb ^. bikeshareStationStatus)
        guard_ (_d_status_info_id status `references_` info)
        pure status

      -- Select from station status.
      pure $ do
        -- Transform each 'AT.StationStatus' into corresponding rows, containing '_status_station_id' and '_status_last_reported'.
        api_values <- values_ $ map (\s -> ( as_ @Int32 ( fromIntegral $ _status_station_id s)
                                           , cast_ (val_ $ _status_last_reported s) (maybeType reportTimeType))
                                    ) api_status
        -- Select from station status where the last reported time is older than in the API response.
        status <- Database.Beam.reuse common_status
        guard_ (_d_status_info_id       status ==. StationInformationId (fst api_values) &&. -- Station ID
                _d_status_active        status ==. val_ True                             &&. -- Status record is active
                _d_status_last_reported status <.  snd api_values)    -- Last reported time is older than in the API response
        pure status

{- |
Separate API 'AT.StationStatus' into two lists:
- one containing statuses that are newer than in the database statuses
- one containing statuses that are the same as in the database statuses
-}
separateNewerStatusRecords :: [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response.
                           -> App FilterStatusResult
separateNewerStatusRecords api_status = do
  -- Find rows that would need to be deactivated if the statuses from the API were inserted.
  statuses_would_deactivate <- getRowsToDeactivate api_status

  -- Map of all API statuses, keyed on station ID.
  let api_status_map                = Map.fromList $ map (\ss -> (               ss ^. status_station_id,   ss)) api_status

  -- Map of rows that would be deactivated, keyed on station ID.
  let statuses_would_deactivate_map = Map.fromList $ map (\ss -> (fromIntegral $ ss ^. d_status_station_id, ss)) statuses_would_deactivate

  -- Map of intersection of both maps (station ID key appears in both Maps).
  let api_status_newer    = Map.intersection api_status_map statuses_would_deactivate_map
  -- Map of difference of both maps (station ID key appears in API map but not in the statuses to deactivate Map).
  let api_status_unchanged = Map.difference  api_status_map statuses_would_deactivate_map

  pure $ FilterStatusResult { _filter_newer     = map snd $ Map.toAscList api_status_newer
                            , _filter_unchanged = map snd $ Map.toAscList api_status_unchanged
                            }


{- |
Insert updated station status into the database.
-}
insertStationStatus :: [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response.
                    -> App InsertStatusResult
insertStationStatus apiStatus
  | null apiStatus = do
      log W "API status empty when trying to insert into DB."
      pure $ InsertStatusResult [] []
  | otherwise = do
    -- New plan:
    -- 1. Upsert the new statuses using runInsertReturningList and onConflictUpdateInstead.
    -- 2. If there is an ACTIVE row with a matching station ID, then:
    --   a. Upsert the new row, changing only the active field to FALSE.
    --   b. runInsertReturningList returns the rows that were inserted AND updated.
    --   c. Filter the rows that were updated, and return them.
    --   d. Also return the rows that were inserted since we need to return the entire set of new (inserted on first try),
    --      along with updated (upserted to be unactivated, then inserted with new status data) rows.
    -- 3. For rows that were updated:
    --   a. Perform same upsert as in step 1. Since there will be no conflict on insertion, this inserts the new (active) statuses.
    --   b. Return the rows that were inserted. None should be updated, since that would have been done in step 2.
    -- 4. Combine newly-inserted rows from step 2 and updated rows from step 3 into an InsertStatusResult, which is returned.

      -- TODO: step 1.

      -- TODO: step 2.

      -- TODO: step 3.

      -- TODO: step 4.
      pure $ InsertStatusResult [] []
  where
    -- Status from API transformed into Beam expressions.
    statusExprs = map fromJSONToBeamStationStatus apiStatus


{- |
Query the statuses for a station between two times.
-}
queryStationStatusBetween :: Int                 -- ^ Station ID.
                          -> ReportTime          -- ^ Start time.
                          -> ReportTime          -- ^ End time.
                          -> App [StationStatus] -- ^ List of 'StationStatus' for the given station between the given times.
queryStationStatusBetween station_id start_time end_time =
  withPostgres $ runSelectReturningList $ select $
  statusBetweenExpr (fromIntegral station_id) start_time end_time

{- |
Query the station name given a station ID.
-}
queryStationName :: Int               -- ^ Station ID.
                 -> App (Maybe String) -- ^ Station name assosicated with the given station ID.
queryStationName station_id = do
  info <- withPostgres $ runSelectReturningOne $ select $ infoByIdExpr [fromIntegral station_id]

  let station_name = info ^. _Just . info_name

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
               -> App (Maybe Int)  -- ^ Station ID assosicated with the given station name, if found.
queryStationId station_name = do
  info <- withPostgres $ runSelectReturningOne $ select $ queryStationIdExpr station_name

  pure $ fromIntegral <$> info ^? _Just . info_station_id


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
                   -> App [(Int, String)]  -- ^ Tuples of (station ID, name) for stations that matched the query.
queryStationIdLike station_name = do
  info <- withPostgres $ runSelectReturningList $ select $ queryStationIdLikeExpr station_name

  -- Return tuples of (station_id, station_name)
  pure $ map (\si -> ( si ^. info_station_id & fromIntegral
                     , si ^. info_name & Text.unpack
                     )) info

-- | Query the latest status for a station.
queryStationStatusLatest :: Int                       -- ^ Station ID.
                         -> App (Maybe StationStatus) -- ^ Latest 'StationStatus' for the given station.
queryStationStatusLatest station_id = withPostgres $ runSelectReturningOne $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_info_station_id info ==. val_ ( fromIntegral station_id))
  status <- orderBy_ (asc_ . _d_status_last_reported)
              (all_ (bikeshareDb ^. bikeshareStationStatus))
  guard_ (_d_status_info_id status `references_` info &&.
         _d_status_active status ==. val_ True)
  pure status

-- | Count the number of rows in a given table.
queryRowCount :: (Beamable table, Database Postgres db)
              => Getting (DatabaseEntity Postgres db (TableEntity table)) (DatabaseSettings be BikeshareDb) (DatabaseEntity Postgres db (TableEntity table))
              -- ^ Lens to the table in the database.
              -> App (Maybe Int32)     -- ^ Count of rows in the specified table.
queryRowCount table = withPostgres $ runSelectReturningOne $ select $
  aggregate_ (\_ -> as_ @Int32 countAll_) (all_ (bikeshareDb ^. table))

-- | Function to query the size of a table.
queryTableSize :: String                -- ^ Name of the table.
               -> App (Maybe String)    -- ^ Size of the table.
queryTableSize tableName = do
  conn <- withConn
  [Only size] <- liftIO $ query_ conn $ fromString ("SELECT pg_size_pretty(pg_total_relation_size('" ++ tableName ++ "'))")
  return size


-- | Query the latest statuses for all stations before a given time.
queryAllStationsStatusBeforeTime :: ReportTime        -- ^ Latest time to return records for.
                                 -> App [StationStatus] -- ^ Latest 'StationStatus' for each station before given time.
queryAllStationsStatusBeforeTime latestTime = withPostgres $ do
  runSelectReturningList $ selectWith $ do
    queryAllStationsStatusBeforeTimeExpr latestTime
