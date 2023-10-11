-- | Test the database.

{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module TestDatabase
     ( unit_getRowsToDeactivate
     , unit_insertStationApi
     , unit_insertStationInformation
     , unit_insertStationInformationApi
     , unit_insertStationStatus
     , unit_insertStationStatusApi
     , unit_queryDockingUndockingCount
     , unit_queryStationByIdAndName
     , unit_queryStationStatus
     , unit_queryStationStatusBetween
     , unit_separateNewerStatusRecords
     , unit_separateNewerStatusRecordsInsert
     , unit_separateNewerStatusRecordsInsertTwice
     ) where

import           API.ResponseWrapper           ( response_data )
import           API.Types                     ( StationInformationResponse, StationStatusResponse, _unInfoStations,
                                                 status_station_id, unInfoStations, unStatusStations )
import qualified API.Types                     as AT

import           AppEnv

import           Control.Lens

import           Data.Aeson                    ( FromJSON, eitherDecode )
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor                  ( void )
import           Data.Int                      ( Int32 )

import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.BikeShare.Operations
import           Database.BikeShare.Utils

import           Fmt

import           ReportTime

import           Test.Tasty.HUnit

setupTestDatabase :: IO Connection
setupTestDatabase = connectTestDatabase >>= dropTables >>= migrateDatabase

connectTestDatabase :: IO Connection
connectTestDatabase = connectDbName dbnameTest "" "" "" ""


-- | Helper function to decode a JSON file.
decodeFile :: FromJSON a => FilePath -- ^ Path to the JSON file.
           -> IO (Either String a)   -- ^ Decoded value.
decodeFile file = eitherDecode <$> BL.readFile file

{- | Read a file as JSON and decode it into a data type.

The file is located at the given 'FilePath'. If the decoding is successful,
the decoded value is returned. If there is an error decoding the JSON, an
assertion failure with the error message is thrown.
-}
getDecodedFile :: FromJSON a => FilePath -- ^ Path to the JSON file.
                             -> IO a     -- ^ Decoded value.
getDecodedFile filePath = either (assertFailure . ("Error decoding JSON: " ++)) return =<< decodeFile filePath

-- | Helper function to decode a 'StationInformationResponse' from a JSON file.
getDecodedFileInformation :: FromJSON StationInformationResponse
                          => FilePath                       -- ^ Path to the JSON file.
                          -> IO StationInformationResponse  -- ^ Decoded 'StationInformationReponse'.
getDecodedFileInformation = getDecodedFile

-- | Helper function to decode a 'StationStatusResponse' from a JSON file.
getDecodedFileStatus :: FromJSON StationStatusResponse
                     => FilePath                  -- ^ Path to the JSON file.
                     -> IO StationStatusResponse  -- ^ Decoded 'StationStatusReponse'.
getDecodedFileStatus = getDecodedFile


-- | Initialize empty database from the test station information response and all 22 station status responses.
initDBWithAllTestData :: Connection -- ^ Database connection
                      -> IO ()
initDBWithAllTestData conn = do
  info <- getDecodedFileInformation  "docs/json/2.3/station_information-1.json"
  void $ runWithApp dbnameTest $ insertStationInformation $ info   ^. response_data . unInfoStations

  -- Insert test station status data 1-22.
  mapM_ (\i -> do
            statusResponse <- getDecodedFileStatus $ "docs/json/2.3/station_status-"+|i|+".json"
            void $ runWithApp dbnameTest $ insertStationStatus $ statusResponse ^. response_data . unStatusStations
        ) [(1 :: Int) .. (22 :: Int)]


-- | HUnit test for inserting station information.
unit_insertStationInformation :: IO ()
unit_insertStationInformation = do
  -- Connect to the database.
  conn <- setupTestDatabase

  stationInformationResponse <- getDecodedFileInformation "test/json/station_information.json"

  -- Insert test data.
  inserted_info <- runWithApp dbnameTest $ insertStationInformation $ _unInfoStations $ stationInformationResponse ^. response_data

  assertEqual "Inserted station information" 6 (length inserted_info)


-- | HUnit test for inserting station status.
unit_insertStationStatus :: IO ()
unit_insertStationStatus = do
  -- Connect to the database.
  conn <- setupTestDatabase

  info    <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"
  status  <- getDecodedFileStatus      "test/json/station_status.json"

  -- Insert test data.
  inserted_info   <- runWithApp dbnameTest $ insertStationInformation $ info   ^. response_data . unInfoStations
  inserted_status <- runWithApp dbnameTest $ insertStationStatus $ status ^. response_data . unStatusStations

  assertEqual "Inserted station information" 704 (length inserted_info)
  assertEqual "Inserted station status"        8 (length $ inserted_status ^. insert_inserted)


-- | HUnit test for querying station status.
unit_queryStationStatus :: IO ()
unit_queryStationStatus = do
  -- Connect to the database.
  conn <- setupTestDatabase

  info    <- getDecodedFileInformation  "test/json/station_information.json"
  status  <- getDecodedFileStatus       "test/json/station_status.json"

  -- Insert test data.
  inserted_info   <- runWithApp dbnameTest $ insertStationInformation $ info   ^. response_data . unInfoStations
  inserted_status <- runWithApp dbnameTest $ insertStationStatus $ status ^. response_data . unStatusStations

  assertEqual "Inserted station information" 6 (length inserted_info)
  assertEqual "Inserted station status"      5 (length $ inserted_status ^. insert_inserted)

  -- Query station status.
  liftIO $ assertEqual "Query status (limit: 1000)" 5 . length =<< runWithApp dbnameTest (queryStationStatus (Just 1000))
  liftIO $ assertEqual "Query status (limit: none)" 5 . length =<< runWithApp dbnameTest (queryStationStatus Nothing)


-- | HUnit test for inserting station information, with data from the actual API.
unit_insertStationInformationApi :: IO ()
unit_insertStationInformationApi = do
  -- Connect to the database.
  conn <- setupTestDatabase

  info    <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"

  -- Insert test data.
  void $ runWithApp dbnameTest $ insertStationInformation $ info ^. response_data . unInfoStations


-- | HUnit test for inserting station status, with data from the actual API.
unit_insertStationStatusApi :: IO ()
unit_insertStationStatusApi = do
  -- Connect to the database.
  conn <- setupTestDatabase

  status  <- getDecodedFileStatus "docs/json/2.3/station_status-1.json"

  -- Should fail because station information has not been inserted.
  inserted_status <- runWithApp dbnameTest $ insertStationStatus $ status ^. response_data . unStatusStations

  assertEqual "Inserted station status" [] $ inserted_status ^. insert_inserted
  assertEqual "Updated station status"  [] $ inserted_status ^. insert_deactivated

-- | HUnit test for inserting station information and status, with data from the actual API.
unit_insertStationApi :: IO ()
unit_insertStationApi = do
  -- Connect to the database.
  conn <- setupTestDatabase

  info    <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"
  status  <- getDecodedFileStatus      "docs/json/2.3/station_status-1.json"

  -- Insert test data.
  inserted_info   <- runWithApp dbnameTest $ insertStationInformation $ info   ^. response_data . unInfoStations
  inserted_status <- runWithApp dbnameTest $ insertStationStatus $ status ^. response_data . unStatusStations

  assertEqual "Inserted station information" 704 (length inserted_info)
  assertEqual "Inserted station status"      704 (length $ inserted_status ^. insert_inserted)


{- | HUnit test for querying which station status have reported.

Between /station_status-1/ and /station_status-2/, station 7000 reported new data but 7001 did not:
+---------+--------------------+--------------------+-----+
| Station | last\_reported (1) | last\_reported (2) |   Δ |
+=========+====================+====================+=====+
|    7000 |         1694798090 |         1694798350 | 260 |
+---------+--------------------+--------------------+-----+
|    7001 |         1694798218 |         1694798218 |   0 |
+---------+--------------------+--------------------+-----+
-}
unit_getRowsToDeactivate :: IO ()
unit_getRowsToDeactivate = do
  conn <- setupTestDatabase

  -- Separate API status records into those that are newer than in the database entry and those that are unchanged.
  rows_to_deactivate <- doGetRowsToDeactivate conn

  assertEqual "Expected number of rows that would be deactivated" 302 (length rows_to_deactivate)

  -- Ensure station 7000 (has newer data between (1) and (2)) is in the list of rows that would be deactivated.
  assertBool "Station 7000 would be deactivated"     (7000 `elem`     map _d_status_station_id rows_to_deactivate)
  -- Ensure station 7001 (did not update between (1) and (2)) is not in the list of rows that would be deactivated.
  assertBool "Station 7001 would not be deactivated" (7001 `notElem`  map _d_status_station_id rows_to_deactivate)


-- | Query updated station status and return a list of database statuses.
doGetRowsToDeactivate :: Connection -> IO [StationStatusT Identity]
doGetRowsToDeactivate conn = do
  info      <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"
  status_1  <- getDecodedFileStatus      "docs/json/2.3/station_status-1.json"
  status_2  <- getDecodedFileStatus      "docs/json/2.3/station_status-2.json"

  -- Insert test data.
  void $ runWithApp dbnameTest $ insertStationInformation $ info   ^. response_data . unInfoStations
  void $ runWithApp dbnameTest $ insertStationStatus $ status_1 ^. response_data . unStatusStations

  -- Return stations that have reported since being inserted.
  runWithApp dbnameTest $ getRowsToDeactivate $ status_2 ^. response_data . unStatusStations


{- | HUnit test for querying which station status have reported.

Between /station_status-1/ and /station_status-2/, station 7000 reported new data but 7001 did not:
+---------+--------------------+--------------------+-----+
| Station | last\_reported (1) | last\_reported (2) |   Δ |
+=========+====================+====================+=====+
|    7000 |         1694798090 |         1694798350 | 260 |
+---------+--------------------+--------------------+-----+
|    7001 |         1694798218 |         1694798218 |   0 |
+---------+--------------------+--------------------+-----+
-}
unit_separateNewerStatusRecords :: IO ()
unit_separateNewerStatusRecords = do
  conn <- setupTestDatabase

  -- Separate API status records into those that are newer than in the database entry and those that are unchanged.
  api_update_plan <- doSeparateNewerStatusRecords conn

  assertEqual "API status records newer than database entry"      302 (length $ api_update_plan ^. filter_newer)
  assertEqual "API status recrods unchanged from database entry"  407 (length $ api_update_plan ^. filter_unchanged)

  -- Station 7000 should be in the list of API records that would trigger a database update, but not in the list of unchanged records.
  assertBool "Station 7000 record is newer"                      (has (traverse . status_station_id . only 7000) (api_update_plan ^. filter_newer))
  assertBool "Station 7000 record is not in unchanged list" (not (has (traverse . status_station_id . only 7000) (api_update_plan ^. filter_unchanged)))

  -- Station 7001 should be in the list of API records that would /not/ trigger a database update, but not in the list of newer records.
  assertBool "Station 7001 record is unchanged"                  (has (traverse . status_station_id . only 7001) (api_update_plan ^. filter_unchanged))
  assertBool "Station 7001 record is not in newer list"     (not (has (traverse . status_station_id . only 7001) (api_update_plan ^. filter_newer)))


-- | Query updated station status and return a list of API statuses.
doSeparateNewerStatusRecords :: Connection -> IO FilterStatusResult
doSeparateNewerStatusRecords conn = do
  info      <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"
  status_1  <- getDecodedFileStatus      "docs/json/2.3/station_status-1.json"
  status_2  <- getDecodedFileStatus      "docs/json/2.3/station_status-2.json"

  -- Insert test data.
  void $ runWithApp dbnameTest $ insertStationInformation $ info   ^. response_data . unInfoStations
  void $ runWithApp dbnameTest $ insertStationStatus $ status_1 ^. response_data . unStatusStations

  -- Return maps of updated and same API statuses
  runWithApp dbnameTest $ separateNewerStatusRecords $ status_2 ^. response_data . unStatusStations


-- | HUnit test to assert that changed station status are inserted.
unit_separateNewerStatusRecordsInsert :: IO ()
unit_separateNewerStatusRecordsInsert = do
  conn <- setupTestDatabase

  {-
  - Insert information and status data (1)
  - Insert/update status data (2)
  - Check if inserting status data (2) would result in updates
  - Returns (updated, same)
  -}
  updated <- doSeparateNewerStatusRecordsInsertOnce conn

  -- Assert that the same number of status rows are inserted as were updated.
  assertEqual "Deactivated status rows"     302 (length $ updated ^. insert_deactivated)
  assertEqual "Inserted status rows"        302 (length $ updated ^. insert_inserted)
  assertEqual "Same number inserted as updated" (length $ updated ^. insert_inserted) (length $ updated ^. insert_deactivated)

-- | Insert station statuses (1) into a database, then (2).
doSeparateNewerStatusRecordsInsertOnce :: Connection        -- ^ Database connection
                                   -> IO InsertStatusResult -- ^ Result of inserting updated station statuses.
doSeparateNewerStatusRecordsInsertOnce conn = do
  info      <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"
  status_1  <- getDecodedFileStatus      "docs/json/2.3/station_status-1.json"
  status_2  <- getDecodedFileStatus      "docs/json/2.3/station_status-2.json"

  -- Insert test data.
  void $ runWithApp dbnameTest $ insertStationInformation $ info   ^. response_data . unInfoStations
  void $ runWithApp dbnameTest $ insertStationStatus $ status_1 ^. response_data . unStatusStations

  -- Find statuses that need to be updated (second round of data vs. first).
  updated <- runWithApp dbnameTest $ separateNewerStatusRecords $ status_2 ^. response_data . unStatusStations

  -- Insert second round of test data (some of which have reported since the first round was inserted).
  runWithApp dbnameTest $ insertStationStatus $ updated ^. filter_newer


-- FIXME: test fails
-- | HUnit test to assert that reinserting already-deactivated rows is a no-op.
unit_separateNewerStatusRecordsInsertTwice :: IO ()
unit_separateNewerStatusRecordsInsertTwice = do
  conn <- setupTestDatabase

  {-
  - Insert information and status data (1)
  - Insert/update status data (2)
  - Check if inserting status data (2) would result in updates
  - Returns (updated, same)
  -}
  updated <- doSeparateNewerStatusRecordsInsertTwice conn

  -- Assert that the no status rows are inserted or deactivated on the second iteration.
  assertEqual "Deactivated status rows"       0 (length $ updated ^. insert_deactivated)
  assertEqual "Inserted status rows"          0 (length $ updated ^. insert_inserted)
  assertEqual "Same number inserted as updated" (length $ updated ^. insert_inserted) (length $ updated ^. insert_deactivated)


-- FIXME: test fails
-- | Insert station statuses (1) into a database, then (2), then (2) again.
doSeparateNewerStatusRecordsInsertTwice :: Connection        -- ^ Database connection
                                        -> IO InsertStatusResult -- ^ Result of inserting updated station statuses.
doSeparateNewerStatusRecordsInsertTwice conn = do
  info      <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"
  status_1  <- getDecodedFileStatus "docs/json/2.3/station_status-1.json"
  status_2  <- getDecodedFileStatus "docs/json/2.3/station_status-2.json"

  -- Insert first round of test data.
  void $ runWithApp dbnameTest $ insertStationInformation $ info   ^. response_data . unInfoStations
  void $ runWithApp dbnameTest $ insertStationStatus $ status_1 ^. response_data . unStatusStations


  -- Find status records that need to be updated (second round of data vs. first).
  updated_1 <- runWithApp dbnameTest $ separateNewerStatusRecords $ status_2 ^. response_data . unStatusStations

  -- Insert second round of test data (some of which have reported since the first round was inserted).
  void $ runWithApp dbnameTest $ insertStationStatus $ updated_1 ^. filter_newer


  -- Find status records that need to be updated (second round of data vs. second).
  updated_2 <- runWithApp dbnameTest $ separateNewerStatusRecords $ status_2 ^. response_data . unStatusStations

  -- Insert second round of test data once again (nothing should have changed).
  runWithApp dbnameTest $ insertStationStatus $ updated_2 ^. filter_newer


-- | HUnit test to validate that a station ID can be looked up by its name, and vice-versa.
unit_queryStationByIdAndName :: IO ()
unit_queryStationByIdAndName = do
  conn <- setupTestDatabase
  info <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"
  void $ runWithApp dbnameTest $ insertStationInformation $ info ^. response_data . unInfoStations

  assertEqual "Station ID for 'King St W / Joe Shuster Way'" (Just 7148)  =<< runWithApp dbnameTest (queryStationId "King St W / Joe Shuster Way")
  assertEqual "Station ID for 'Wellesley Station Green P'" (Just 7001)    =<< runWithApp dbnameTest (queryStationId "Wellesley Station Green P")
  assertEqual "Stations with name ending in 'Green P'"
    [ (7001,"Wellesley Station Green P")
    , (7050,"Richmond St E / Jarvis St Green P")
    , (7112,"Liberty St / Fraser Ave Green P")
    , (7789,"75 Holly St - Green P")
    ] =<< (runWithApp dbnameTest $ queryStationIdLike "%Green P")


-- | HUnit test to query all status records for a station between two times.
unit_queryStationStatusBetween :: IO ()
unit_queryStationStatusBetween = do
  conn <- setupTestDatabase
  initDBWithAllTestData conn

  -- First status for #7001 was inserted at 2023-09-15 17:16:58; last status at 2023-09-15 17:35:00.
  statusBetweenAll <- runWithApp dbnameTest $ queryStationStatusBetween 7001
    (ReportTime $ read "2023-09-15 17:16:58")
    (ReportTime $ read "2023-09-15 17:35:00")
  assertEqual "Expected number of status records for #7001 between two valid times" 4 (length statusBetweenAll)

  -- Query for status records for #7001 between two times, where the start and end time match the first status report.
  statusBetweenFirst <- runWithApp dbnameTest $ queryStationStatusBetween 7001
    (ReportTime $ read "2000-09-15 17:16:58") -- Moment the first status was reported.
    (ReportTime $ read "2023-09-15 17:16:58") -- Moment the first status was reported.
  assertEqual "Expected number of status records for #7001 for first status reported" 1 (length statusBetweenFirst)

  -- Query for status records for #7001 between two times, where the end time is before the first status was reported.
  statusBetweenTooEarly <- runWithApp dbnameTest $ queryStationStatusBetween 7001
    (ReportTime $ read "2000-01-01 00:00:00") -- Arbitrary date
    (ReportTime $ read "2023-09-15 17:16:57") -- One second before first status reported.
  assertEqual "Expected number of status records for #7001 before first status reported" 0 (length statusBetweenTooEarly)

  {-
  Query for status records for #7001 between two times, where the earliest time is *after* the first status was reported,
  and the end time is *before* the first status was reported.

  NOTE: as an example, uses both 'ReportTime $ ...' and 'reportTime ...' to construct a 'ReportTime' value.
  -}
  statusBetweenBackwards <- runWithApp dbnameTest $ queryStationStatusBetween 7001
    (ReportTime $ read "2023-09-15 17:16:59")                     -- One second after first status reported.
    (reportTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00))  -- Arbitrary date
  assertEqual "Expected number of status records for #7001 with backwards time parameters" 0 (length statusBetweenBackwards)


-- | HUnit test to query all status records for a station between two times.
unit_queryDockingUndockingCount :: IO ()
unit_queryDockingUndockingCount = do
  conn <- setupTestDatabase
  initDBWithAllTestData conn

  -- Test dataset has 5 rows for station 7000, 3 rows for 7006, 5 for 7012, and 5 rows for 7148:
  -- |   id | station | reported               | iconic |
  -- |------+---------+------------------------+--------|
  -- |    1 |    7000 | 2023-09-15 17:14:50-04 |      0 |
  -- |  705 |    7000 | 2023-09-15 17:19:10-04 |      0 |
  -- | 1309 |    7000 | 2023-09-15 17:23:29-04 |      5 |
  -- | 1849 |    7000 | 2023-09-15 17:32:06-04 |     26 |
  -- | 2746 |    7000 | 2023-09-15 17:36:27-04 |     29 |
  -- |------+---------+------------------------+--------|
  -- |    7 |    7006 | 2023-09-15 17:17:15-04 |     13 |
  -- | 1164 |    7006 | 2023-09-15 17:21:36-04 |     10 |
  -- | 1855 |    7006 | 2023-09-15 17:34:31-04 |      7 |
  -- |------+---------+------------------------+--------|
  -- |   12 |    7012 | 2023-09-15 17:14:33-04 |     10 |
  -- |  710 |    7012 | 2023-09-15 17:18:52-04 |     11 |
  -- | 1317 |    7012 | 2023-09-15 17:23:09-04 |     13 |
  -- | 1860 |    7012 | 2023-09-15 17:31:47-04 |     12 |
  -- | 2749 |    7012 | 2023-09-15 17:36:06-04 |     12 |
  -- |------+---------+------------------------+--------|
  -- |  136 |    7148 | 2023-09-15 17:14:12-04 |     17 |
  -- |  757 |    7148 | 2023-09-15 17:18:40-04 |     17 |
  -- | 1411 |    7148 | 2023-09-15 17:23:08-04 |     17 |
  -- | 1983 |    7148 | 2023-09-15 17:32:07-04 |     19 |
  -- | 2849 |    7148 | 2023-09-15 17:36:37-04 |     19 |


  --                   station  thresholds     dockings undockings
  checkConditions conn 7000     thresholds7000 29         0
  checkConditions conn 7006     thresholds7006  0       (-6)
  checkConditions conn 7012     thresholds7012  3       (-1)
  checkConditions conn 7148     thresholds7148  2         0
  where
    thresholds7000  = [ OldestID    1, NewestID 2746, LatestTime (ReportTime $ read "2023-09-15 17:36:27.0") ]
    thresholds7006  = [ OldestID    7, NewestID 1855, LatestTime (ReportTime $ read "2023-09-15 17:34:31.0") ]
    thresholds7012  = [ OldestID   12, NewestID 2749, LatestTime (ReportTime $ read "2023-09-15 17:36:06.0") ]
    thresholds7148  = [ OldestID  136, NewestID 2849, LatestTime (ReportTime $ read "2023-09-15 17:36:37.0") ]

checkConditions :: Connection -> Int32 -> [StatusThreshold] -> Int -> Int -> IO ()
checkConditions conn stationId thresholds expectDockings expectUndockings = do
  eventCountsForStation <- findInList stationId <$> queryDockingEventsCount conn (StatusVariationQuery stationId Docking AT.Iconic thresholds)
  assertEqual ("Expected number of undockings at station " ++ show stationId) (Just expectUndockings)  (undockings <$> eventCountsForStation)
  assertEqual ("Expected number of dockings at station "   ++ show stationId) (Just expectDockings)    (dockings   <$> eventCountsForStation)

findInList :: Int32 -> [DockingEventsCount] -> Maybe DockingEventsCount
findInList key tuples = tuples ^? folded . filtered (\k -> station k ^. info_station_id == key)
