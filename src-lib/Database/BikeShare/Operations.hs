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
     , module Database.BikeShare.Operations.StatusChanges
     , insertStationInformation
     , insertStationInformation'
     , insertStationStatus
     , insertSystemInformation
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

import qualified API.StationInformation                       as AT
import qualified API.StationStatus                            as AT
import qualified API.SystemInformation                        as AT

import           AppEnv

import           Control.Lens                                 hiding ( reuse, (<.) )

import           Data.Int                                     ( Int32 )
import           Data.List                                    ( nubBy )
import qualified Data.Map                                     as Map
import           Data.Maybe                                   ( catMaybes, mapMaybe )
import           Data.Pool                                    ( withResource )
import qualified Data.Text                                    as Text
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.BikeShare.Expressions
import           Database.BikeShare.Operations.Dockings
import           Database.BikeShare.Operations.StatusChanges
import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationLookup
import           Database.BikeShare.Tables.StationStatus
import           Database.BikeShare.Tables.SystemInformation
import           Database.PostgreSQL.Simple                   ( Only (..), query_ )

import           GHC.Exts                                     ( fromString )

import           Text.Pretty.Simple.Extras

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
insertStationInformation :: UTCTime
                         -> [AT.StationInformation]             -- ^ List of 'StationInformation' from the API response.
                         -> AppM [StationInformationT Identity] -- ^ List of 'StationInformation' that where inserted.
insertStationInformation reported stations = do
  (_, _, inserted) <- insertStationInformation' reported stations
  pure inserted

insertStationInformation' :: UTCTime
                         -> [AT.StationInformation]
                         -- ^ List of 'StationInformation' from the API response.
                         -> AppM ([StationInformation], [StationInformation], [StationInformation])
                         -- ^ List of 'StationInformation' that were: (active, updated, inserted).
insertStationInformation' reported stations =
  -- Use a transaction to ensure that the database is not left in an inconsistent state.
  withPostgresTransaction $ do
    -- Retrieve all active stations
    info <- runSelectReturningList $ select $
      filter_ (\inf -> _infoActive inf ==. val_ True)
      (all_ (bikeshareDb ^. bikeshareStationInformation))

    -- Pairs of (StationInformation, AT.StationInformation) where the API data is meaningfully different from the database contents.
    let newerInfo = infoHasNewer (apiInfoMap stations) (infoMap info)
    -- Station information from the API which is not already present in the database.
    let newInfo = infoNewStation (apiInfoMap stations) (infoMap info)

    -- Update the station information that is already in the database to set active = False.
    updated <- runUpdateReturningList $ update (_bikeshareStationInformation bikeshareDb)
      (\inf -> _infoActive inf <-. val_ False)
      (\inf -> _infoId inf `in_` idsToUpdate newerInfo)

    -- Insert only the stations that are not already in the database.
    inserted <- runInsertReturningList $ do
      insertStationInformationExpr reported (map snd newerInfo ++ newInfo)
    pure (info, updated, inserted)
  where
    infoMap    = Map.fromList . map (\inf -> ((fromIntegral . _infoStationId) inf, inf))
    apiInfoMap = Map.fromList . map (\inf -> (AT.infoStationId inf, inf))
    infoHasNewer apiInfo info = catMaybes . Map.elems $ Map.intersectionWith stationInfoChanged apiInfo info
    infoNewStation apiInfo info = Map.elems (Map.difference apiInfo info)
    idsToUpdate = map (fromIntegral . _infoId . fst)

stationInfoChanged :: AT.StationInformation -> StationInformation -> Maybe (StationInformation, AT.StationInformation)
stationInfoChanged apiInfo info
  | stationInfoMostlyEq apiInfo info = Nothing
  | otherwise = Just (info, apiInfo)

stationInfoMostlyEq apiInfo dbInfo =
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
insertStationStatus :: [AT.StationStatus] -- ^ List of 'AT.StationStatus' from the API response.
                    -> AppM [StationStatus]
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

    -- Insert only changed station statuses into the database.
    _stationStatusChanged <- insertChangedStationStatus

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
  info <- withPostgres $ runSelectReturningOne $ selectWith $ infoByIdExpr [fromIntegral stationId]

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
  status <- orderBy_ (desc_ . _statusLastReported . _statusCommon)
            (all_ (bikeshareDb ^. bikeshareStationStatus))
  guard_ ((_unInformationStationId . _statusInfoId . _statusCommon) status ==. _infoStationId info)
  pure status

-- | Count the number of rows in a given table.
queryRowCount :: (Beamable table, Database Postgres db)
              => Getting (DatabaseEntity Postgres db (TableEntity table)) (DatabaseSettings Postgres BikeshareDb) (DatabaseEntity Postgres db (TableEntity table))
              -- ^ Lens to the table in the database.
              -> AppM (Maybe Int32)     -- ^ Count of rows in the specified table.
queryRowCount table = withPostgres $ runSelectReturningOne $ select $
  aggregate_ (\_ -> as_ @Int32 countAll_) (all_ (bikeshareDb ^. table))

-- | Function to query the size of a table.
queryTableSize :: String                -- ^ Name of the table.
               -> AppM (Maybe String)    -- ^ Size of the table.
queryTableSize tableName = do
  pool <- withConnPool
  [Only size] <- liftIO (withResource pool (\conn -> query_ conn $ fromString ("SELECT pg_size_pretty(pg_total_relation_size('" ++ tableName ++ "'))")))
  return size

querySystemStatusAtRange :: UTCTime -> UTCTime -> Integer
                         -> AppM [(UTCTime, Integer, Integer, Integer, Integer, Integer, Integer, Integer)]
querySystemStatusAtRange earliestTime latestTime intervalMins = do
  -- Execute query expression, returning 'Just (tup)' if one row was returned; otherwise 'Nothing'.
  statusAtTime <-
    withPostgres $ runSelectReturningList $ selectWith $
    querySystemStatusAtRangeExpr earliestTime latestTime intervalMins

  (pure . map statusFieldsToInt) statusAtTime
  where
    -- Convert fields from 'Int32' to 'Int'.
    statusFieldsToInt statusAtTime =
      ( statusAtTime ^. _1
      , fromIntegral (statusAtTime ^. _2)
      , fromIntegral (statusAtTime ^. _3)
      , fromIntegral (statusAtTime ^. _4)
      , fromIntegral (statusAtTime ^. _5)
      , fromIntegral (statusAtTime ^. _6)
      , fromIntegral (statusAtTime ^. _7)
      , fromIntegral (statusAtTime ^. _8)
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
