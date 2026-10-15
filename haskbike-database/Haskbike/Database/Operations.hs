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

module Haskbike.Database.Operations
     ( module Haskbike.Database.Operations.Dockings
     , module Haskbike.Database.Operations.StatusChanges
     , insertStationInformation
     , insertStationInformation'
     , insertStationInformation'Pg
     , insertStationInformationHistoric
     , insertStationInformationHistoricPg
     , insertStationStatus
     , insertStationStatusAsOf
     , insertStationStatusAsOfPg
     , insertSystemInformation
     , insertSystemInformationPg
     , lockTableAndSleep
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
     , sleepDatabaseFor
     ) where

import           Control.Lens                                hiding ( reuse, (<.) )
import           Control.Monad.Catch                         ( MonadCatch, MonadThrow )

import           Data.Int                                    ( Int32 )
import           Data.List                                   ( maximumBy, nubBy, partition )
import qualified Data.Map                                    as Map
import           Data.Maybe                                  ( catMaybes, fromMaybe, mapMaybe )
import           Data.Ord                                    ( comparing )
import           Data.Pool                                   ( withResource )
import qualified Data.Text                                   as Text
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple                  ( Only (..), execute_, query_ )
import           Database.PostgreSQL.Simple.Transaction      ( withTransaction )

import           GHC.Exts                                    ( fromString )
import           GHC.Int                                     ( Int64 )

import qualified Haskbike.API.StationInformation             as AT
import qualified Haskbike.API.StationStatus                  as AT
import qualified Haskbike.API.SystemInformation              as AT
import           Haskbike.AppEnv
import           Haskbike.Database.BikeShare
import           Haskbike.Database.Expressions
import           Haskbike.Database.Operations.Dockings
import           Haskbike.Database.Operations.StatusChanges
import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationLookup
import           Haskbike.Database.Tables.StationStatus
import           Haskbike.Database.Tables.SystemInformation

import           Text.Pretty.Simple.Extras

import           UnliftIO                                    ( MonadUnliftIO )


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
  guard_ ((_unInformationStationId . _statusInfoId . _statusCommon) status ==. _infoStationId info)
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

-- | Insert new station information into the database.
insertStationInformation :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                         => [(UTCTime, AT.StationInformation)]          -- ^ List of 'StationInformation' from the API response.
                         -> m [StationInformationT Identity] -- ^ List of 'StationInformation' that where inserted.
insertStationInformation stations = do
  (_, _, inserted) <- insertStationInformation' stations
  pure inserted

insertStationInformation' :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                          => [(UTCTime, AT.StationInformation)]
                          -- ^ List of 'StationInformation' from the API response.
                          -> m ([StationInformation], [StationInformation], [StationInformation])
                          -- ^ List of 'StationInformation' that were: (active, updated, inserted).
insertStationInformation' =
  -- Use a transaction to ensure that the database is not left in an inconsistent state.
  withPostgresTransaction . insertStationInformation'Pg

-- | 'Pg' core of 'insertStationInformation''. Demotes the currently active row for any station
-- whose API payload is meaningfully different, then inserts the newer/new payloads as the new
-- active rows. Runs outside a transaction of its own so callers (such as
-- 'insertStationInformationHistoricPg') can compose it into a larger transaction.
insertStationInformation'Pg :: [(UTCTime, AT.StationInformation)]
                            -- ^ List of 'StationInformation' from the API response.
                            -> Pg ([StationInformation], [StationInformation], [StationInformation])
                            -- ^ List of 'StationInformation' that were: (active, updated, inserted).
insertStationInformation'Pg stations = do
    -- Retrieve all active stations
    info <- runSelectReturningList $ select $
      filter_ (\inf -> _infoActive inf ==. val_ True)
      (all_ (bikeshareDb ^. bikeshareStationInformation))

    -- Pairs of (StationInformation, (UTCTime, AT.StationInformation)) where the API data is meaningfully different from the database contents.
    let newerInfo = infoHasNewer (apiInfoMap stations) (infoMap info)
    -- Station information from the API which is not already present in the database.
    let newInfo = infoNewStation (apiInfoMap stations) (infoMap info)

    -- Update the station information that is already in the database to set active = False.
    updated <- runUpdateReturningList $ update (_bikeshareStationInformation bikeshareDb)
      (\inf -> _infoActive inf <-. val_ False)
      (\inf -> _infoId inf `in_` idsToUpdate newerInfo)

    -- Insert only the stations that are not already in the database.
    inserted <- runInsertReturningList $ do
      insertStationInformationExpr (map snd newerInfo ++ newInfo)
    pure (info, updated, inserted)
   where
     infoMap    = Map.fromList . map (\inf -> ((fromIntegral . _infoStationId) inf, inf))
     apiInfoMap = Map.fromList . map (\inf -> ((AT.infoStationId . snd) inf, inf))
     infoHasNewer apiInfo info = catMaybes . Map.elems $ Map.intersectionWith stationInfoChanged apiInfo info
     infoNewStation apiInfo info = Map.elems (Map.difference apiInfo info)
     idsToUpdate = map (fromIntegral . _infoId . fst)

-- | Transactional wrapper for 'insertStationInformationHistoricPg'.
insertStationInformationHistoric :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                                 => [(UTCTime, AT.StationInformation)]
                                 -- ^ List of 'StationInformation' from the API response, potentially out of chronological order.
                                 -> m ([StationInformation], [StationInformation])
                                 -- ^ Rows inserted: (via the normal/newer path, directly as inactive history).
insertStationInformationHistoric =
  withPostgresTransaction . insertStationInformationHistoricPg

{- | Insert historic (out-of-chronological-order) station information payloads without demoting
the currently active row for any station.

Used by the replay path: a replayed payload can be chronologically older than data already in
the database, so it must never win the "active" row for its station. A payload is /newer/ for its
station when its reported time is later than that station's currently active row's reported time,
or when the station has no active row yet; such payloads go through the normal
'insertStationInformation'Pg' path, which /does/ demote as usual. Payloads that are not newer are
inserted directly as inactive history rows; this is a no-op
(@ON CONFLICT (station_id, reported) DO NOTHING@) if that exact (station, reported) pair has
already been inserted.
-}
insertStationInformationHistoricPg :: [(UTCTime, AT.StationInformation)]
                                   -- ^ List of 'StationInformation' from the API response, potentially out of chronological order.
                                   -> Pg ([StationInformation], [StationInformation])
                                   -- ^ Rows inserted: (via the normal/newer path, directly as inactive history).
insertStationInformationHistoricPg stations = do
  activeInfo <- runSelectReturningList $ select $
    filter_ (\inf -> _infoActive inf ==. val_ True)
    (all_ (bikeshareDb ^. bikeshareStationInformation))

  let activeReportedByStation = Map.fromList (map (\inf -> ((fromIntegral . _infoStationId) inf, _infoReported inf)) activeInfo)
      isNewerThanActive (reported, apiInfo) = maybe True (reported >) (Map.lookup (AT.infoStationId apiInfo) activeReportedByStation)
      (newer, older) = partition isNewerThanActive stations

  (_, _, insertedNewer) <-
    if null newer
    then pure ([], [], [])
    else insertStationInformation'Pg newer

  insertedOlder <-
    if null older
    then pure []
    else runInsertReturningList $
         insertOnConflict (bikeshareDb ^. bikeshareStationInformation)
         (insertExpressions (map (\(reported, apiInfo) -> fromJSONToBeamStationInformation reported apiInfo & infoActive .~ val_ False) older))
         (conflictingFields primaryKey) onConflictDoNothing

  pure (insertedNewer, insertedOlder)

stationInfoChanged :: (UTCTime, AT.StationInformation) -> StationInformation -> Maybe (StationInformation, (UTCTime, AT.StationInformation))
stationInfoChanged apiInfo info
  | stationInfoMostlyEq apiInfo info = Nothing
  | otherwise = Just (info, apiInfo)

stationInfoMostlyEq (_reported, apiInfo) dbInfo =
  isEq AT.infoName a b
  && isEq AT.infoPhysicalConfiguration a b
  && isEq AT.infoLat a b
  && isEq AT.infoLon a b
  && isEq AT.infoAltitude a b
  && isEq AT.infoAddress a b
  && isEq AT.infoCapacity a b
  && isEq AT.infoIsChargingStation a b
  && isEq AT.infoRentalMethods a b
  && isEq AT.infoIsValetStation a b
  && isEq AT.infoIsVirtualStation a b
  && isEq AT.infoGroups a b
  && isEq AT.infoObcn a b
  && isEq AT.infoNearbyDistance a b
  -- TODO: Bluetooth ID is changing randomly for ~150 stations.
  && isEq AT.infoBluetoothId a b
  && isEq AT.infoRideCodeSupport a b
  && isEq AT.infoRentalUris a b
  where a = apiInfo
        b = fromBeamStationInformationToJSON dbInfo
        isEq f a' b' = f a' == f b'


{- |
Insert station statuses into the database.
-}
insertStationStatus :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                    => [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response.
                    -> m [StationStatus]
insertStationStatus apiStatus =
  withPostgresTransaction $ do
    info <- runSelectReturningList $ select $
      filter_ (\inf -> _infoStationId inf `in_` map (val_ . fromIntegral . AT._statusStationId) apiStatus
                   &&. _infoActive inf ==. val_ True
              )
      (all_ (bikeshareDb ^. bikeshareStationInformation))

    let infoMap        :: Map.Map Int StationInformation        = Map.fromList
                                                                (map (\inf -> ((fromIntegral . _infoStationId) inf, inf)) info)
    let statusWithInfo :: [(StationInformation, AT.StationStatus)] = mapMaybe (lookupInfoId infoMap) apiStatus

    status <- runInsertReturningList $
              insertOnConflict (bikeshareDb ^. bikeshareStationStatus)
              (insertExpressions
               (mapMaybe (\(inf, sta) -> fromJSONToBeamStationStatus (StationInformationId (_infoStationId inf) (_infoReported inf)) sta) statusWithInfo)
              ) (conflictingFields primaryKey) onConflictDoNothing

    _statusLookup <- runInsertReturningList $
      insertOnConflict (bikeshareDb ^. bikeshareStationLookup)
      (insertExpressions $
       map (\ss -> StationLookup (val_ $ StationStatusId (ss ^. statusInfoId) (ss ^. statusLastReported)))
       (uniqueStatus status)
      ) (conflictingFields (_unInformationStationId . _unStatusStationId . _stnLookup)) onConflictUpdateAll

    pure status
  where
    uniqueStatus = nubBy (\s1 s2 -> (_unInformationStationId . _statusInfoId . _statusCommon) s1 == (_unInformationStationId . _statusInfoId . _statusCommon) s2)
    lookupInfoId infoMap status =
      case Map.lookup (AT._statusStationId status) infoMap of
        Nothing  -> Nothing
        Just iId -> Just (iId, status)


-- | Transactional wrapper for 'insertStationStatusAsOfPg'.
insertStationStatusAsOf :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                        => [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response, potentially out of chronological order.
                        -> m [StationStatus]
insertStationStatusAsOf =
  withPostgresTransaction . insertStationStatusAsOfPg

{- | Insert station statuses linked to the station_information row that was current /as of/
each status's own @last_reported@ time, and advance 'station_lookup' only when doing so moves
it forward.

Used by the replay path: a replayed status can be chronologically older than the
currently active station_information row, so linking it to that active row (as
'insertStationStatus' does) would attribute it to the wrong station shape. Instead, for each
status this looks up every station_information row on record for that station (active or not,
unlike 'insertStationStatus') and picks the one with the latest @reported@ that is still @<=@ the
status's @last_reported@ - the row that was actually in effect at that moment. A status with no
such candidate (no info row exists at or before its @last_reported@) is dropped rather than
inserted. 'station_lookup' - which points at each station's most recently seen status - is only
advanced when the incoming status's @last_reported@ is later than the one already stored, so a
historic insert can never regress the "latest status" pointer.
-}
insertStationStatusAsOfPg :: [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response, potentially out of chronological order.
                          -> Pg [StationStatus]
insertStationStatusAsOfPg apiStatus = do
    -- Retrieve every station_information row (active or not) for the stations in this batch.
    info <- runSelectReturningList $ select $
      filter_ (\inf -> _infoStationId inf `in_` map (val_ . fromIntegral . AT._statusStationId) apiStatus)
      (all_ (bikeshareDb ^. bikeshareStationInformation))

    let infoByStation  :: Map.Map Int [StationInformation] = Map.fromListWith (++)
                                                              (map (\inf -> ((fromIntegral . _infoStationId) inf, [inf])) info)
    let statusWithInfo :: [(StationInformation, AT.StationStatus)] = mapMaybe (asOfInfoFor infoByStation) apiStatus

    status <- runInsertReturningList $
              insertOnConflict (bikeshareDb ^. bikeshareStationStatus)
              (insertExpressions
               (mapMaybe (\(inf, sta) -> fromJSONToBeamStationStatus (StationInformationId (_infoStationId inf) (_infoReported inf)) sta) statusWithInfo)
              ) (conflictingFields primaryKey) onConflictDoNothing

    _statusLookup <- runInsertReturningList $
      insertOnConflict (bikeshareDb ^. bikeshareStationLookup)
      (insertExpressions $
       map (\ss -> StationLookup (val_ $ StationStatusId (ss ^. statusInfoId) (ss ^. statusLastReported)))
       (uniqueStatus status)
      ) (conflictingFields (_unInformationStationId . _unStatusStationId . _stnLookup))
        -- Only advance the lookup pointer forward: an out-of-order (historic) status must never
        -- overwrite a lookup entry that already points at a later last_reported.
        (onConflictUpdateSetWhere
          (\current excluded ->
             ((_unInformationReported . _unStatusStationId . _stnLookup) current <-. (_unInformationReported . _unStatusStationId . _stnLookup) excluded)
             <> ((_unStatusLastReported . _stnLookup) current <-. (_unStatusLastReported . _stnLookup) excluded))
          (\current excluded -> current_ ((_unStatusLastReported . _stnLookup) current) <. (_unStatusLastReported . _stnLookup) excluded))

    pure status
  where
    uniqueStatus = nubBy (\s1 s2 -> (_unInformationStationId . _statusInfoId . _statusCommon) s1 == (_unInformationStationId . _statusInfoId . _statusCommon) s2)
    -- Pick the station_information row that was current as of a status's last_reported: the
    -- latest-reported row that is not later than the status itself. Drops statuses that have no
    -- last_reported, or no station_information row on record at or before that time.
    asOfInfoFor infoByStation status = do
      lastReported <- AT._statusLastReported status
      candidates   <- Map.lookup (AT._statusStationId status) infoByStation
      case filter (\inf -> _infoReported inf <= lastReported) candidates of
        []       -> Nothing
        eligible -> Just (maximumBy (comparing _infoReported) eligible, status)


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
queryStationName :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                 => Int                 -- ^ Station ID.
                 -> m (Maybe String) -- ^ Station name assosicated with the given station ID.
queryStationName stationId = do
  info <- withPostgres $ runSelectReturningOne $ selectWith $ infoByIdE [fromIntegral stationId]

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
queryStationIdLike :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                   => String               -- ^ Station ID.
                   -> m [(Int, String)]  -- ^ Tuples of (station ID, name) for stations that matched the query.
queryStationIdLike stationName = do
  info <- withPostgres $ runSelectReturningList $ select $ queryStationIdLikeExpr stationName

  -- Return tuples of (station_id, station_name)
  pure $ map (\si -> ( si ^. infoStationId & fromIntegral
                     , si ^. infoName & Text.unpack
                     )) info

-- | Query the latest status for a station.
queryStationStatusLatest :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
                         => Int                       -- ^ Station ID.
                         -> m (Maybe StationStatus) -- ^ Latest 'StationStatus' for the given station.
queryStationStatusLatest station_id = withPostgres $ runSelectReturningOne $ select $ limit_ 1 $ do
  info   <- all_ (bikeshareDb ^. bikeshareStationInformation)
  guard_ (_infoStationId info ==. val_ ( fromIntegral station_id))
  status <- orderBy_ (desc_ . _statusLastReported . _statusCommon)
            (all_ (bikeshareDb ^. bikeshareStationStatus))
  guard_ ((_unInformationStationId . _statusInfoId . _statusCommon) status ==. _infoStationId info)
  pure status

-- | Count the number of rows in a given table.
queryRowCount :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, Beamable table, Database Postgres db)
              => Getting (DatabaseEntity Postgres db (TableEntity table)) (DatabaseSettings Postgres BikeshareDb) (DatabaseEntity Postgres db (TableEntity table))
              -- ^ Lens to the table in the database.
              -> m Int     -- ^ Count of rows in the specified table.
queryRowCount table = fromIntegral . fromMaybe 0 <$> withPostgres (runSelectReturningOne $ select $
    aggregate_ (\_ -> as_ @Int32 countAll_)
    (all_ (bikeshareDb ^. table)))

-- | Function to query the size of a table.
queryTableSize :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m, MonadFail m)
               => String                -- ^ Name of the table.
               -> m (Maybe String)    -- ^ Size of the table.
queryTableSize tableName = do
  pool <- withConnPool
  [Only size] <- liftIO (withResource pool (\conn -> query_ conn $ fromString ("SELECT pg_size_pretty(pg_total_relation_size('" ++ tableName ++ "'))")))
  return size

-- | Per-row return for 'querySystemStatusAtRange'. The trailing 8-tuple carries per-vehicle-type
-- sums in the order (Boost, Iconic, EFit, EFitG5, CHLOE, Cosmo, Astro, Metro).
querySystemStatusAtRange :: (HasEnv env m, MonadIO m, MonadCatch m)
                         => UTCTime -> UTCTime -> Integer
                         -> m [( UTCTime
                               , Integer, Integer, Integer, Integer
                               , (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
                               )]
querySystemStatusAtRange earliestTime latestTime intervalMins = do
  -- Execute query expression, returning 'Just (tup)' if one row was returned; otherwise 'Nothing'.
  statusAtTime <-
    withPostgres $ runSelectReturningList $ selectWith $
    querySystemStatusAtRangeExpr earliestTime latestTime intervalMins

  (pure . map statusFieldsToInt) statusAtTime
  where
    -- Convert fields from 'Int32' to 'Integer'.
    statusFieldsToInt statusAtTime =
      let bikeTypes = statusAtTime ^. _6 in
      ( statusAtTime ^. _1
      , fromIntegral (statusAtTime ^. _2)
      , fromIntegral (statusAtTime ^. _3)
      , fromIntegral (statusAtTime ^. _4)
      , fromIntegral (statusAtTime ^. _5)
      , ( fromIntegral (bikeTypes ^. _1)
        , fromIntegral (bikeTypes ^. _2)
        , fromIntegral (bikeTypes ^. _3)
        , fromIntegral (bikeTypes ^. _4)
        , fromIntegral (bikeTypes ^. _5)
        , fromIntegral (bikeTypes ^. _6)
        , fromIntegral (bikeTypes ^. _7)
        , fromIntegral (bikeTypes ^. _8)
        )
      )

-- | Insert system information into the database.
insertSystemInformation :: UTCTime
                        -- ^ Time at which the system information was reported.
                        -> AT.SystemInformation
                        -- ^ List of 'SystemInformation' from the API response.
                        -> AppM ([SystemInformation], [SystemInformationCount])
                        -- ^ List of 'SystemInformation' that where inserted.
insertSystemInformation reported inf = do
  insertedInfo <- withPostgres $ runInsertReturningList $
    insert (bikeshareDb ^. bikeshareSystemInformation)
    (insertExpressions [fromJSONToBeamSystemInformation reported inf])
  insertedInfoCount <- withPostgres $ runInsertReturningList $
    insert (bikeshareDb ^. bikeshareSystemInformationCount)
    (insertExpressions [fromJSONToBeamSystemInformationCount reported inf])
  pure (insertedInfo, insertedInfoCount)

{- | 'Pg' core combining the two inserts 'insertSystemInformation' performs (system_information,
then system_information_count) into one action, for callers - such as the replay path - that need
to run it as part of a larger transaction rather than its own standalone transaction. The returned
count is the sum of rows inserted across /both/ tables; this is deliberately wider than the
'Haskbike.API.APIEntity.APIPersistable' @insertAPI@ instance for @SystemInformationCountT@, whose
count covers only the count-table insert.
-}
insertSystemInformationPg :: UTCTime
                          -- ^ Time at which the system information was reported.
                          -> AT.SystemInformation
                          -- ^ 'SystemInformation' from the API response.
                          -> Pg Int
                          -- ^ Count of rows inserted across both tables.
insertSystemInformationPg reported inf = do
  insertedInfo <- runInsertReturningList $
    insert (bikeshareDb ^. bikeshareSystemInformation)
    (insertExpressions [fromJSONToBeamSystemInformation reported inf])
  insertedInfoCount <- runInsertReturningList $
    insert (bikeshareDb ^. bikeshareSystemInformationCount)
    (insertExpressions [fromJSONToBeamSystemInformationCount reported inf])
  pure (length insertedInfo + length insertedInfoCount)

-- | Sleep the database for a given number of seconds.
sleepDatabaseFor :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => Int -> m ()
sleepDatabaseFor seconds = do
  pool <- getDBConnectionPool
  _ :: [Only ()] <- liftIO $ withResource pool $ \conn ->
    withTransaction conn $
        query_ conn (fromString ("SELECT pg_sleep(" ++ show seconds ++ ")"))
  pure ()


-- | Lock a table and then for a given number of seconds.
lockTableAndSleep :: (HasEnv env m, MonadIO m, MonadCatch m, MonadUnliftIO m) => String -> Int -> m ()
lockTableAndSleep tableName seconds = do
  pool <- getDBConnectionPool
  _ :: () <- liftIO $ withResource pool $ \conn ->
    withTransaction conn $ do
        _ :: Int64 <- execute_ conn (fromString ("LOCK TABLE " <> tableName <> " IN ACCESS EXCLUSIVE MODE"))
        _ :: [Only ()] <- query_ conn (fromString ("SELECT pg_sleep(" ++ show seconds ++ ")"))
        pure ()
  pure ()
