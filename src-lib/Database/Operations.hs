{-# LANGUAGE CPP              #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains the operations that can be performed on the database.

module Database.Operations
  ( queryDisabledDocks
  , printDisabledDocks
  , queryStationStatus
  , queryStationStatusFields
  , queryStationInformation
  , insertStationInformation
  -- , _insertStationStatus -- NOTE: use insertUpdatedStationStatus instead
  , queryUpdatedStatus
  , filterStatusBase
  , filterStatusUpdated
  , filterStatusSame
  , insertUpdatedStationStatus
  ) where

import           API.Types                                (StationInformationResponse (..),
                                                           StationStatusResponse (..),
                                                           _status_last_reported,
                                                           _status_station_id,
                                                           status_station_id)
import qualified API.Types                                as AT
import           Database.BikeShare
import           Database.Utils

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres

import           Control.Lens                             hiding ((<.))
import           Data.Int                                 (Int32)
import qualified Data.Map                                 as Map


-- | Enable SQL debug output if DEBUG flag is set.
runBeamPostgres' :: Connection -> Pg a -> IO a
runBeamPostgres' =
#ifdef DEBUG
  runBeamPostgresDebug pPrintCompact
#else
  runBeamPostgres
#endif

-- | Query database for disabled docks, returning tuples of (name, num_docks_disabled).
queryDisabledDocks conn =
  runBeamPostgres' conn $ runSelectReturningList $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  status <- all_ (bikeshareDb ^. bikeshareStationStatus)
  guard_ (_d_status_info_id status `references_` info &&. status^.d_status_num_docks_disabled >. 0)
  pure ( info^.info_name
       , status^.d_status_num_docks_disabled
       )

-- | Helper function to print disabled docks.
printDisabledDocks :: IO ()
printDisabledDocks = (connectDb >>= queryDisabledDocks) >>= pPrintCompact

-- | Query database for station status.
queryStationStatus :: Connection -> IO [(StationInformation, StationStatus)]
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

-- | Query database for disabled docks, returning tuples of (name, num_docks_disabled).
queryStationInformation :: Connection -> [Int] -> IO [StationInformation]
queryStationInformation conn ids =
  runBeamPostgres' conn $ runSelectReturningList $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_info_station_id info `in_` ids')
  pure info
  where
    ids' = fromIntegral <$> ids

-- | Insert station information into the database.
insertStationInformation :: Connection -> [AT.StationInformation] -> IO [StationInformation]
insertStationInformation conn stations = do
  runBeamPostgres' conn $ runInsertReturningList $
    insert (bikeshareDb ^. bikeshareStationInformation) $
    insertExpressions $ map fromJSONToBeamStationInformation stations



-- | Insert station status into the database.
_insertStationStatus :: Connection -> [AT.StationStatus] -> IO [StationStatus]
_insertStationStatus conn status = do
  -- Get information for the stations that are in the status response.
  info_ids <- map _info_station_id <$> queryStationInformation conn status_ids
  let filtered_status = filter (\ss -> fromIntegral (_status_station_id ss) `elem` info_ids) status
  runBeamPostgres' conn $ runInsertReturningList $
    insert (bikeshareDb ^. bikeshareStationStatus) $
    insertExpressions $ map fromJSONToBeamStationStatus filtered_status
  where
    status_ids :: [Int]
    status_ids = fromIntegral <$> status ^.. traverse . status_station_id


-- | Query database to determine which stations have reported since being inserted.
queryUpdatedStatus :: Connection -> [AT.StationStatus] -> IO [StationStatus]
queryUpdatedStatus conn api_status = do
  -- Select using common table expressions (selectWith).
  runBeamPostgres' conn $ runSelectReturningList $ selectWith $ do
    -- CTE for station information.
    common_info <- selecting $
        filter_ (\info -> _info_station_id info `in_` map (fromIntegral . _status_station_id) api_status)
        (all_ (bikeshareDb ^. bikeshareStationInformation))

    -- CTE for station status.
    common_status <- selecting $ do
      info <- Database.Beam.reuse common_info
      status <- orderBy_ (desc_ . _d_status_id) $
        all_ (bikeshareDb ^. bikeshareStationStatus)
      guard_ (_d_status_info_id status `references_` info)
      pure status

    -- Select from station status.
    pure $ do
      -- Construct rows containing station ID and last reported time, corresponding to the API response parameter.
      api_values <- values_ $ map (\s -> ( as_ @Int32 ( fromIntegral $ _status_station_id s)
                                         , cast_ (val_ $ _status_last_reported s) (maybeType reportTimeType))
                                  ) api_status
      -- Select from station status where the last reported time is older than in the API response.
      status <- Database.Beam.reuse common_status
      guard_ (_d_status_station_id    status ==. fst api_values &&.
              _d_status_active        status ==. val_ True      &&.
              _d_status_last_reported status <.  snd api_values)
      pure status

{- |
Query database for updated statuses and return a tuple of maps representing the API statuses that have reported:
First element:  map of API statuses that have reported since being inserted.
Second element: map of API statuses that have not reported since being inserted.
-}
filterStatusBase :: Connection -> [AT.StationStatus] -> IO (Map.Map Int AT.StationStatus, Map.Map Int AT.StationStatus)
filterStatusBase conn api_status = do
  -- Query database for updated statuses
  db_status_updated <- queryUpdatedStatus conn api_status

  -- Construct map of all API statuses
  let api_status'        = Map.fromList $ map (\ss -> (               ss ^. status_station_id,   ss)) api_status
  -- Construct map of updated statuses from the database
  let db_updated_status' = Map.fromList $ map (\ss -> (fromIntegral $ ss ^. d_status_station_id, ss)) db_status_updated

  -- Construct map of intersection of both maps; only elements with keys in both are preserved.
  let api_status_updated = Map.intersection api_status' db_updated_status'
  let api_status_same    = Map.difference   api_status' db_updated_status'

  pure (api_status_updated, api_status_same)

-- | Query database for updated statuses and return a filtered list of API StationStatus which have reported since being inserted.
filterStatusUpdated :: Connection -> [AT.StationStatus] -> IO [AT.StationStatus]
filterStatusUpdated conn api_status = do
  updated_map <- fst <$> filterStatusBase conn api_status

  let updated_list = map snd $ Map.toAscList updated_map

  pure updated_list

-- | Query database for updated statuses and return a filtered list of API StationStatus which have not reported since being inserted.
filterStatusSame :: Connection -> [AT.StationStatus] -> IO [AT.StationStatus]
filterStatusSame conn api_status = do
  same_map <- snd <$> filterStatusBase conn api_status

  let same_list = map snd $ Map.toAscList same_map

  pure same_list


-- | Insert updated station status into the database.
insertUpdatedStationStatus :: Connection -> [AT.StationStatus] -> IO ([StationStatus], [StationStatus])
insertUpdatedStationStatus conn status = do
  -- Query database for updated statuses
  db_status_updated <- case length status of
    0 -> pure []
    _ -> queryUpdatedStatus conn status
  let status_ids' =  map _d_status_id db_status_updated

  updated <- case length db_status_updated of
    -- Can't update if there are no statuses to update (SQL restriction).
    0 -> pure []
    -- Set returned station statuses as inactive.
    _ -> runBeamPostgres' conn $ runUpdateReturningList $
      update (bikeshareDb ^. bikeshareStationStatus)
             (\c -> _d_status_active c <-. val_ False)
             (\c -> _d_status_id c `in_` map val_ status_ids')

  -- Get information for the stations that are in the status response.
  info_ids <- map _info_station_id <$> queryStationInformation conn status_ids
  let filtered_status = filter (\ss -> fromIntegral (_status_station_id ss) `elem` info_ids) status
  inserted <- case length updated of
    0 -> pure []
    _ -> runBeamPostgres' conn $ runInsertReturningList $
      insert (bikeshareDb ^. bikeshareStationStatus) $
      insertExpressions $ map fromJSONToBeamStationStatus filtered_status

  pure (updated, inserted)

  where
    status_ids :: [Int]
    status_ids = fromIntegral <$> status ^.. traverse . status_station_id
