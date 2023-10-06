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
     ( InsertStatusResult (..)
     , cteStationStatus'
     , formatCteStatus
     , getRowsToDeactivate
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
     , queryTableSize
       -- types
     , AvailabilityCountChanged (..)
     , FilterStatusResult (..)
     , StatusQuery (..)
     , StatusThreshold (..)
       -- lenses
     , filter_newer
     , filter_unchanged
     , insert_deactivated
     , insert_inserted
       -- functions
     , runBeamPostgres'
     , runBeamPostgresDebug'
     , separateNewerStatusRecords
     ) where

import           API.Types                                ( _status_last_reported, _status_station_id,
                                                            status_station_id )
import qualified API.Types                                as AT

import           Control.Lens                             hiding ( reuse, (<.) )

import           Data.Int                                 ( Int32 )
import qualified Data.Map                                 as Map
import qualified Data.Text                                as Text

import           Database.Beam
import           Database.Beam.Backend                    ( BeamSqlBackend )
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres                   as Pg
import           Database.BikeShare
import           Database.BikeShare.Expressions
import           Database.BikeShare.Utils
import           Database.PostgreSQL.Simple               ( Only (..), query_ )

import           GHC.Exts                                 ( fromString )


debug :: Bool
debug = False


-- | Enable SQL debug output if DEBUG flag is set.
runBeamPostgres' :: Connection  -- ^ Connection to the database.
                 -> Pg a        -- ^ @MonadBeam@ in which we can run Postgres commands.
                 -> IO a
runBeamPostgres' =
  if debug
  then runBeamPostgresDebug'
  else runBeamPostgres


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
queryDisabledDocks conn = do
  runBeamPostgres' conn $ runSelectReturningList $ select disabledDocksExpr

-- | Helper function to print disabled docks.
printDisabledDocks :: Connection -> IO ()
printDisabledDocks conn = queryDisabledDocks conn >>= pPrintCompact

-- | Query database for station status.
queryStationStatus :: Connection                               -- ^ Connection to the database.
                   -> Maybe Integer                            -- ^ Limit number of rows returned.
                   -> IO [(StationInformation, StationStatus)] -- ^ List of tuples of (station information, station status).
queryStationStatus conn limit =
  runBeamPostgres' conn $ runSelectReturningList $ select $
  queryStationStatusExpr limit

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
  runBeamPostgres' conn $ runSelectReturningList $ select $ all_ (bikeshareDb ^. bikeshareStationInformation)

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
insertStationInformation conn stations =
  runBeamPostgres' conn $ runInsertReturningList $ insertStationInformationExpr stations

-- | Find the corresponding active rows in the database corresponding to each element of a list of *newer* 'AT.StationStatus' records.
getRowsToDeactivate :: Connection         -- ^ Connection to the database.
                    -> [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response.
                    -> IO [StationStatus] -- ^ List of 'StationStatus' rows that would need to be deactivated, if the statuses from the API were inserted.
getRowsToDeactivate conn api_status
  | null api_status = return []
  | otherwise = do
    -- Select using common table expressions (selectWith).
    runBeamPostgres' conn $ runSelectReturningList $ selectWith $ do
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
insertStationStatus :: Connection         -- ^ Connection to the database.
                    -> [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response.
                    -> IO InsertStatusResult
insertStationStatus conn api_status
  | null api_status = return $ InsertStatusResult [] []
  | otherwise = do
    info_ids <- map (fromIntegral . _info_station_id) <$> queryStationInformationByIds conn status_ids

    let status = filter (\ss -> fromIntegral (_status_station_id ss) `elem` info_ids) api_status

    statuses_to_deactivate <- getRowsToDeactivate conn status
    updated_statuses <- deactivateOldStatus conn statuses_to_deactivate
    inserted_statuses <- insertNewStatus conn $
      filter (\ss -> ss ^. status_station_id `elem`
                     map (fromIntegral . _d_status_station_id) updated_statuses
             ) status

    -- Select arbitrary (in this case, last [by ID]) status record for each station ID.
    distinct_by_id <- selectDistinctByStationId conn status

    -- Insert new records corresponding to station IDs which did not already exist in the station_status table.
    new_status <- insertNewStatus conn $
      filter (\ss -> ss ^. status_station_id `notElem`
                     map (fromIntegral . _d_status_station_id) distinct_by_id &&
                     ss ^. status_station_id `elem` info_ids
             ) status


    return $ InsertStatusResult updated_statuses (inserted_statuses ++ new_status)

  where
    status_ids = fromIntegral <$> api_status ^.. traverse . status_station_id

    selectDistinctByStationId conn' status
      | null status = return []
      | otherwise = runBeamPostgres' conn' $ runSelectReturningList $ select $ Pg.pgNubBy_ _d_status_station_id $ orderBy_ (desc_ . _d_status_station_id) $
      all_ (bikeshareDb ^. bikeshareStationStatus)

    deactivateOldStatus conn' status
      | null status = return []
      | otherwise = runBeamPostgres' conn' $ runUpdateReturningList $
          update (bikeshareDb ^. bikeshareStationStatus)
          (\c -> _d_status_active c <-. val_ False)
          (\c -> _d_status_id c `in_` map (val_ . _d_status_id) status)

    insertNewStatus conn' status
      | null status = return []
      | otherwise = runBeamPostgres' conn' $ runInsertReturningList $
          insert (bikeshareDb ^. bikeshareStationStatus) $
          insertExpressions $ map fromJSONToBeamStationStatus status

{- |
Query the statuses for a station between two times.
-}
queryStationStatusBetween :: Connection         -- ^ Connection to the database.
                          -> Int                -- ^ Station ID.
                          -> ReportTime         -- ^ Start time.
                          -> ReportTime         -- ^ End time.
                          -> IO [StationStatus] -- ^ List of 'StationStatus' for the given station between the given times.
queryStationStatusBetween conn station_id start_time end_time =
  runBeamPostgres' conn $ runSelectReturningList $ select $
  statusBetweenExpr (fromIntegral station_id) start_time end_time

{- |
Query the station name given a station ID.
-}
queryStationName :: Connection        -- ^ Connection to the database.
                 -> Int               -- ^ Station ID.
                 -> IO (Maybe String) -- ^ Station name assosicated with the given station ID.
queryStationName conn station_id = do
  info <- runBeamPostgres' conn $ runSelectReturningOne $ select $ infoByIdExpr [fromIntegral station_id]

  let station_name = info ^. _Just . info_name

  pure $ Just $ Text.unpack station_name


{- |
Query the station ID for given a station name, using SQL `=` semantics.

== __Examples__
Get ID for "King St W / Joe Shuster Way":

>>> queryStationId conn "King St W / Joe Shuster Way"
Just 7148

Get ID for "Wellesley Station Green P":

>>> queryStationId conn "Wellesley Station Green P"
Just 7001
-}
queryStationId :: Connection        -- ^ Connection to the database.
                 -> String          -- ^ Station ID.
                 -> IO (Maybe Int)  -- ^ Station ID assosicated with the given station name, if found.
queryStationId conn station_name = do
  info <- runBeamPostgres' conn $ runSelectReturningOne $ select $ queryStationIdExpr station_name

  pure $ fromIntegral <$> info ^? _Just . info_station_id


{- | Query possible station IDs matching a given station name, using SQL `LIKE` semantics.

== __Examples__

Search for station names ending with "Green P":

>>> queryStationIdLike conn "%Green P"
[ (7001,"Wellesley Station Green P")
, (7050,"Richmond St E / Jarvis St Green P")
, (7112,"Liberty St / Fraser Ave Green P")
, (7789,"75 Holly St - Green P") ]

Search for station names containing "Joe Shuster":

>>> queryStationIdLike conn "%Joe Shuster%"
[(7148,"King St W / Joe Shuster Way")]

__Return:__ Tuples of (station ID, station name) matching the searched name, using SQL `LIKE` semantics.
-}
queryStationIdLike :: Connection          -- ^ Connection to the database.
                   -> String              -- ^ Station ID.
                   -> IO [(Int, String)]  -- ^ Tuples of (station ID, name) for stations that matched the query.
queryStationIdLike conn station_name = do
  info <- runBeamPostgres' conn $ runSelectReturningList $ select $ queryStationIdLikeExpr station_name

  -- Return tuples of (station_id, station_name)
  pure $ map (\si -> ( si ^. info_station_id & fromIntegral
                     , si ^. info_name & Text.unpack
                     )) info

-- | Query the latest status for a station.
queryStationStatusLatest :: Connection               -- ^ Connection to the database.
                         -> Int                      -- ^ Station ID.
                         -> IO (Maybe StationStatus) -- ^ Latest 'StationStatus' for the given station.
queryStationStatusLatest conn station_id = runBeamPostgres' conn $ runSelectReturningOne $ select $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_info_station_id info ==. val_ ( fromIntegral station_id))
  status <- orderBy_ (asc_ . _d_status_last_reported)
              (all_ (bikeshareDb ^. bikeshareStationStatus))
  guard_ (_d_status_info_id status `references_` info &&.
         _d_status_active status ==. val_ True)
  pure status

-- | Count the number of rows in a given table.
queryRowCount :: (Beamable table, Database Postgres db)
              => Connection           -- ^ Connection to the database.
              -> Getting (DatabaseEntity Postgres db (TableEntity table)) (DatabaseSettings be BikeshareDb) (DatabaseEntity Postgres db (TableEntity table))
              -- ^ Lens to the table in the database.
              -> IO (Maybe Int32)     -- ^ Count of rows in the specified table.
queryRowCount conn table = runBeamPostgres' conn $ runSelectReturningOne $ select $
  aggregate_ (\_ -> as_ @Int32 countAll_) (all_ (bikeshareDb ^. table))

-- | Function to query the size of a table.
queryTableSize :: Connection            -- ^ Connection to the database.
               -> String                -- ^ Name of the table.
               -> IO (Maybe String)     -- ^ Size of the table.
queryTableSize conn tableName = do
  [Only size] <- query_ conn $ fromString ("SELECT pg_size_pretty(pg_total_relation_size('" ++ tableName ++ "'))")
  return size

{-
In this version, `cteWithDiff` is a list of triples `(StationStatusT Identity, prev_avail, diff)`, where `diff` is iconic availability difference.
`sumIconic` calculates the sum of `diff` over the entire list, and the final result is a list with each row containing the station status, previous iconic availability, and the sum of iconic availability differences.
-}




formatCteStatus :: [(StationStatusT Identity, Int32)] -> IO ()
formatCteStatus bar = pPrintCompact $ map (\(s, l) ->
                                             ( s ^. d_status_id & unSerial
                                             , s ^. d_status_station_id
                                             , s ^. d_status_last_reported
                                             , l
                                             )
                                          ) bar

data StatusQuery = StatusQuery
  { _status_query_station_id :: Int32
  , _status_query_thresholds :: [StatusThreshold] -- Update from single StatusThreshold to list
  } deriving (Show, Eq)

data StatusThreshold =
    OldestID Int32
  | SinceTime ReportTime
  deriving (Show, Eq)

thresholdCondition :: StatusThreshold -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
thresholdCondition (OldestID id_threshold) status =
  status ^. d_status_id >=. val_ (fromIntegral id_threshold)
thresholdCondition (SinceTime time_threshold) status =
  status ^. d_status_last_reported >=. val_ (Just time_threshold)

filterFor_ :: StatusQuery -> StationStatusT (QExpr Postgres s) -> QExpr Postgres s Bool
filterFor_ (StatusQuery stationId thresholds) status =
  let stationCondition = status ^. d_status_station_id ==. val_ (fromIntegral stationId)
      thresholdConditions = map (`thresholdCondition` status) thresholds
  in foldr (&&.) stationCondition thresholdConditions

-- | Data type representing the type of statistic to query.
data AvailabilityCountChanged where
  Undocked      :: AvailabilityCountChanged -- ^ Bike undocked (ride began at this station)
  Docked        :: AvailabilityCountChanged -- ^ Bike docked   (ride ended at this station)
  deriving (Show, Eq)

-- | TODO: parameterize over column.
cteStationStatus' :: Connection -> AvailabilityCountChanged -> StatusQuery -> IO [(StationStatusT Identity, Int32)]
cteStationStatus' conn statisticType conditions =
  runBeamPostgres' conn $ do
    runSelectReturningList $ selectWith $ do
      cte <- selecting $ do
        let statusForStation = filter_ (filterFor_ conditions)
                                       (all_ (bikeshareDb ^. bikeshareStationStatus))
          in withWindow_ (\row -> frame_ (partitionBy_ (row ^. d_status_station_id)) (orderPartitionBy_ (asc_ $ row ^. d_status_id)) noBounds_)
                         (\row w -> (row, lagWithDefault_ (row ^. vehicle_types_available_iconic) (val_ 1) (row ^. vehicle_types_available_iconic) `over_` w))
                         statusForStation
      dockings <- selecting $ do
        -- Only rows where the availability increased.
        let increments = filter_ (\(s, prevAvail) -> s ^. vehicle_types_available_iconic `deltaOp_` prevAvail)
                         (reuse cte)
              -- Delta between current and previous iconic availability.
              in withWindow_ (\(row, _prev) -> frame_ (partitionBy_ (row ^. d_status_station_id)) noOrder_ noBounds_)
                             (\(row, prev) _w -> (row, row ^. vehicle_types_available_iconic - prev))
                             increments

      pure $ do
        counts <- reuse cte -- [(status, previous availability)]

        -- Rows where the delta was either:
        -- - positive ('deltaOp_': '(>.)')
        -- - negative ('deltaOp_': '(<.)')
        -- ... depending on the statisticType paramater.
        changed <- filter_ (\(_s, delta) -> delta `deltaOp_` 0)
                           (reuse dockings)

        guard_ ((counts ^. _1 . d_status_id) ==. (changed ^. _1 . d_status_id))

        pure changed
  where
    infixl 4 `deltaOp_` -- same as '(<.)' and '(>.)'.
    deltaOp_ :: (BeamSqlBackend be) => QGenExpr context be s a -> QGenExpr context be s a -> QGenExpr context be s Bool
    deltaOp_ = case statisticType of
      Undocked -> (<.)
      Docked   -> (>.)

-- conn <- mkDbParams "haskbike" >>= uncurry5 connectDbName
-- res <- cteStationStatus conn 7148 1890764
-- res <- cteStationStatus <$> (mkDbParams "haskbike" >>= uncurry5 connectDbName) <*> pure 7148 <*> pure 1890764 >>= liftIO
-- res ^.. traverse . _4
-- listFrequency s = map (\x -> ([head x], length x)) . List.group . List.sort $ s
