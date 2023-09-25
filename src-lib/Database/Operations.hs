{-# LANGUAGE CPP                 #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module contains the operations that can be performed on the database.

module Database.Operations
     ( InsertStatusResult (..)
     , getRowsToDeactivate
     , insertStationInformation
     , insertUpdatedStationStatus
     , printDisabledDocks
     , queryDisabledDocks
     , queryStationInformation
     , queryStationInformationByIds
     , queryStationName
     , queryStationStatus
     , queryStationStatusBetween
     , queryStationStatusFields
       -- types
     , FilterStatusResult (..)
       -- lenses
     , filter_newer
     , filter_unchanged
     , insert_deactivated
     , insert_inserted
       -- functions
     , separateNewerStatusRecords
     ) where

import           API.Types                                ( _status_last_reported, _status_station_id,
                                                            status_station_id )
import qualified API.Types                                as AT

import           Control.Lens                             hiding ( (<.) )

import           Data.Int                                 ( Int32 )
import qualified Data.Map                                 as Map
import qualified Data.Text                                as Text

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.Utils


-- | Enable SQL debug output if DEBUG flag is set.
runBeamPostgres' :: Connection  -- ^ Connection to the database.
                 -> Pg a        -- ^ @MonadBeam@ in which we can run Postgres commands.
                 -> IO a
runBeamPostgres' =
#ifdef DEBUG
  runBeamPostgresDebug'
#else
  runBeamPostgres
#endif


-- | @runBeamPostgresDebug@ prefilled with @pPrintCompact@.
runBeamPostgresDebug' :: Connection     -- ^ Connection to the database.
                      -> Pg a           -- ^ @MonadBeam@ in which we can run Postgres commands.
                      -> IO a
runBeamPostgresDebug' = runBeamPostgresDebug pPrintCompact


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
queryDisabledDocks :: Connection              -- ^ Connection to the database.
                   -> IO [(Text.Text, Int32)] -- ^ List of tuples of (name, num_docks_disabled).
queryDisabledDocks conn =
  runBeamPostgres' conn $ runSelectReturningList $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- all_ (bikeshareDb ^. bikeshareStationStatus)
  guard_ (_d_status_info_id status `references_` info &&. status^.d_status_num_docks_disabled >. 0)
  pure ( info^.info_name
       , status^.d_status_num_docks_disabled
       )

-- | Helper function to print disabled docks.
printDisabledDocks :: Connection -- ^ Connection to the database.
                   -> IO ()
printDisabledDocks conn = queryDisabledDocks conn >>= pPrintCompact

-- | Query database for station status.
queryStationStatus :: Connection                               -- ^ Connection to the database.
                   -> IO [(StationInformation, StationStatus)] -- ^ List of tuples of (station information, station status).
queryStationStatus conn = do
  runBeamPostgres' conn $ runSelectReturningList $ select $ do
    info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
    status <- all_ (bikeshareDb ^. bikeshareStationStatus)
    guard_ (_d_status_info_id status `references_` info)
    -- station_status <- leftJoin_ (all_(bikeshareDb ^. bikeshareStationStatus))
    --   (\station_status -> _station_id station_status `references_` station_information)
    -- guard_ (isJust_ station_status)
    pure (info, status)

-- | Query database for station status, returning the number of bikes and docks available and disabled.
queryStationStatusFields :: Connection                                   -- ^ Connection to the database.
                         -> IO [(Text.Text, Int32, Int32, Int32, Int32)] -- ^ List of tuples of (name, num_bikes_available, num_bikes_disabled, num_docks_available, num_docks_disabled).
queryStationStatusFields conn =
  runBeamPostgres' conn $ runSelectReturningList $ select $ do
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
queryStationInformation:: Connection              -- ^ Connection to the database.
                       -> IO [StationInformation] -- ^ List of station information.
queryStationInformation conn =
  runBeamPostgres' conn $ runSelectReturningList $ select $ do
  all_ (bikeshareDb ^. bikeshareStationInformation)

{- | Query database for station information corresponding to a list of station IDs. -}
queryStationInformationByIds :: Connection              -- ^ Connection to the database.
                             -> [Int]                   -- ^ List of station IDs to query.
                             -> IO [StationInformation] -- ^ List of station information.
queryStationInformationByIds conn ids =
  runBeamPostgres' conn $ runSelectReturningList $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_info_station_id info `in_` ids')
  pure info
  where
    ids' = fromIntegral <$> ids

-- | Insert station information into the database.
insertStationInformation :: Connection              -- ^ Connection to the database.
                         -> [AT.StationInformation] -- ^ List of 'StationInformation' from the API response.
                         -> IO [StationInformation] -- ^ List of 'StationInformation' that where inserted.
insertStationInformation conn stations = do
  runBeamPostgres' conn $ runInsertReturningList $
    insert (bikeshareDb ^. bikeshareStationInformation) $
    insertExpressions $ map fromJSONToBeamStationInformation stations


{- |
Find the corresponding active rows in the database corresponding to each element of a list of *newer* 'AT.StationStatus' records.
-}
getRowsToDeactivate :: Connection         -- ^ Connection to the database.
                    -> [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response.
                    -> IO [StationStatus] -- ^ List of 'StationStatus' rows that would need to be deactivated, if the statuses from the API were inserted.
getRowsToDeactivate conn api_status = do
  -- Select using common table expressions (selectWith).
  runBeamPostgres' conn $ runSelectReturningList $ selectWith $ do
    -- Common table expression for 'StationInformation'.
    common_info <- selecting $
        filter_ (\info -> _info_station_id info `in_` map (fromIntegral . _status_station_id) api_status)
        (all_ (bikeshareDb ^. bikeshareStationInformation))

    -- Common table expression for 'StationStatus'.
    common_status <- selecting $ do
      info <- Database.Beam.reuse common_info
      status <- orderBy_ (desc_ . _d_status_id) $
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
      guard_ (_d_status_station_id    status ==. fst api_values &&. -- Station ID
              _d_status_active        status ==. val_ True      &&. -- Status record is active
              _d_status_last_reported status <.  snd api_values)    -- Last reported time is older than in the API response
      pure status

{- |
Separate API 'AT.StationStatus' into two lists:
- one containing statuses that are newer than in the database statuses
- one containing statuses that are the same as in the database statuses
-}
separateNewerStatusRecords :: Connection         -- ^ Connection to the database.
                           -> [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response.
                           -> IO FilterStatusResult
separateNewerStatusRecords conn api_status = do
  -- Find rows that would need to be deactivated if the statuses from the API were inserted.
  statuses_would_deactivate <- getRowsToDeactivate conn api_status

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
insertUpdatedStationStatus :: Connection         -- ^ Connection to the database.
                           -> [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response.
                           -> IO InsertStatusResult
insertUpdatedStationStatus conn api_status = do
  -- Get information for the stations that are in the status response.
  info_ids <- map _info_station_id <$> queryStationInformationByIds conn status_ids

  -- Filter out statuses that don't have corresponding information in the database.
  let status = filter (\ss -> fromIntegral (_status_station_id ss) `elem` info_ids) api_status

  -- Determine which rows will be deactivated.
  statuses_to_deactivate <- case length api_status of
    0 -> pure []                             -- No need to query if there are no statuses to update.
    _ -> getRowsToDeactivate conn api_status -- Get rows to deactivate.
  let status_ids' =  map _d_status_id statuses_to_deactivate

  -- Deactivate status rows which we have new data for.
  updated_statuses <- case length statuses_to_deactivate of
    0 -> pure [] -- Can't update if there are no statuses to update (SQL restriction).
    _ ->         -- Set returned station statuses as inactive.
      runBeamPostgres' conn $ runUpdateReturningList $
        update (bikeshareDb ^. bikeshareStationStatus)
        (\c -> _d_status_active c <-. val_ False)
        (\c -> _d_status_id c `in_` map val_ status_ids')

  -- Insert new status rows.
  inserted_statuses <- case length status of
    0 -> pure [] -- No need to insert if there are no statuses to insert.
    _ ->         -- Insert rows for each 'AT.StationStatus' that has newer data.
      runBeamPostgres' conn $ runInsertReturningList $
        insert (bikeshareDb ^. bikeshareStationStatus) $
        insertExpressions $ map fromJSONToBeamStationStatus status

  pure $ InsertStatusResult { _insert_deactivated = updated_statuses
                            , _insert_inserted    = inserted_statuses
                            }

  where
    status_ids :: [Int]
    status_ids = fromIntegral <$> api_status ^.. traverse . status_station_id


{- |
Query the statuses for a station between two times.
-}
queryStationStatusBetween :: Connection         -- ^ Connection to the database.
                          -> Int                -- ^ Station ID.
                          -> ReportTime         -- ^ Start time.
                          -> ReportTime         -- ^ End time.
                          -> IO [StationStatus] -- ^ List of 'StationStatus' for the given station between the given times.
queryStationStatusBetween conn station_id start_time end_time = do
  runBeamPostgres' conn $ runSelectReturningList $ select $ do
    info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
    status <- orderBy_ (asc_ . _d_status_last_reported)
                (all_ (bikeshareDb ^. bikeshareStationStatus))
    guard_ (_d_status_info_id status `references_` info &&.
            _info_station_id info ==. val_ (fromIntegral station_id) &&.
            _d_status_last_reported status >=. val_ (Just start_time) &&.
            _d_status_last_reported status <=. val_ (Just end_time))
    pure status

{- |
Query the station name given a station ID.
-}
queryStationName :: Connection        -- ^ Connection to the database.
                 -> Int               -- ^ Station ID.
                 -> IO (Maybe String) -- ^ Station name assosicated with the given station ID.
queryStationName conn station_id = do
  info <- runBeamPostgres' conn $ runSelectReturningOne $ select $ do
    info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
    guard_ (_info_station_id info ==. val_ (fromIntegral station_id))
    pure info

  let station_name = info ^. _Just . info_name

  pure $ Just $ Text.unpack station_name
