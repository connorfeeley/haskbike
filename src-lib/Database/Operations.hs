{-# LANGUAGE CPP                 #-}

{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module contains the operations that can be performed on the database.

module Database.Operations
     ( insertStationInformation
     , printDisabledDocks
     , queryDisabledDocks
     , queryStationInformation
     , queryStationStatus
     , queryStationStatusFields
       -- , _insertStationStatus -- NOTE: use insertUpdatedStationStatus instead
     , FilterStatusResult (..)
     , InsertStatusResult (..)
     , filterStatus
     , filter_same
     , filter_updated
     , insertUpdatedStationStatus
     , insert_inserted
     , insert_updated
     , queryUpdatedStatus
     ) where

import           API.Types                                ( StationInformationResponse (..), StationStatusResponse (..),
                                                            _status_last_reported, _status_station_id,
                                                            status_station_id )
import qualified API.Types                                as AT

import           Control.Lens                             hiding ( (<.) )

import           Data.Int                                 ( Int32 )
import qualified Data.Map                                 as Map

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.Utils


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
printDisabledDocks :: Connection -> IO ()
printDisabledDocks conn = queryDisabledDocks conn >>= pPrintCompact

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
queryUpdatedStatus :: Connection -- ^ Connection to the database.
  -> [AT.StationStatus] -- ^ List of station statuses from the API response.
  -> IO [StationStatus] -- ^ List of station statuses that would need to be updated, if the statuses from the API were inserted.
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

data FilterStatusResult where
  FilterStatusResult :: { _filter_updated :: [AT.StationStatus]
                        , _filter_same    :: [AT.StationStatus]
                        } -> FilterStatusResult
makeLenses ''FilterStatusResult

filterStatus :: Connection -> [AT.StationStatus] -> IO FilterStatusResult
filterStatus conn api_status = do
  -- Query database for updated statuses
  db_status_updated <- queryUpdatedStatus conn api_status

  -- Construct map of all API statuses
  let api_status'        = Map.fromList $ map (\ss -> (               ss ^. status_station_id,   ss)) api_status
  -- Construct map of updated statuses from the database
  let db_updated_status' = Map.fromList $ map (\ss -> (fromIntegral $ ss ^. d_status_station_id, ss)) db_status_updated

  -- Construct map of intersection of both maps; only elements with keys in both are preserved.
  let api_status_same    = Map.intersection api_status' db_updated_status'
  let api_status_updated = Map.difference   api_status' db_updated_status'

  pure $ FilterStatusResult {_filter_updated = map snd $ Map.toAscList api_status_updated, _filter_same = map snd $ Map.toAscList api_status_same}


data InsertStatusResult where
  InsertStatusResult :: { _insert_updated :: [StationStatus]
                        , _insert_inserted :: [StationStatus]
                        } -> InsertStatusResult
makeLenses ''InsertStatusResult

{- |
Insert updated station status into the database.
-}
insertUpdatedStationStatus :: Connection -> [AT.StationStatus] -> IO InsertStatusResult
insertUpdatedStationStatus conn status = do
  -- Query database for updated statuses
  db_status_updated <- case length status of
    0 -> pure [] -- No need to query if there are no statuses to update.
    _ -> queryUpdatedStatus conn status
  let status_ids' =  map _d_status_id db_status_updated

  updated <- case length db_status_updated of
    0 -> pure [] -- Can't update if there are no statuses to update (SQL restriction).
    -- Set returned station statuses as inactive.
    _ -> runBeamPostgres' conn $ runUpdateReturningList $
      update (bikeshareDb ^. bikeshareStationStatus)
             (\c -> _d_status_active c <-. val_ False)
             (\c -> _d_status_id c `in_` map val_ status_ids')

  -- Get information for the stations that are in the status response.
  info_ids <- map _info_station_id <$> queryStationInformation conn status_ids
  let filtered_status = filter (\ss -> fromIntegral (_status_station_id ss) `elem` info_ids) status
  inserted <- case length filtered_status of
    0 -> pure [] -- No need to insert if there are no statuses to insert.
    _ -> runBeamPostgres' conn $ runInsertReturningList $
      insert (bikeshareDb ^. bikeshareStationStatus) $
      insertExpressions $ map fromJSONToBeamStationStatus filtered_status

  pure $ InsertStatusResult {_insert_updated = updated, _insert_inserted = inserted }

  where
    status_ids :: [Int]
    status_ids = fromIntegral <$> status ^.. traverse . status_station_id
