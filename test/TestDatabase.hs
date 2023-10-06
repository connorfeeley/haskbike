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
     , unit_queryStationByIdAndName
     , unit_queryStationStatus
     , unit_queryStationStatusBetween
     , unit_separateNewerStatusRecords
     , unit_separateNewerStatusRecordsInsert
     , unit_separateNewerStatusRecordsInsertTwice
     ) where

import           API.ResponseWrapper           ( response_data )
import           API.Types                     ( StationInformationResponse, StationStatusResponse, _info_stations,
                                                 info_stations, status_station_id, status_stations )

import           Control.Lens

import           Data.Aeson                    ( FromJSON, eitherDecode )
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor                  ( void )

import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.BikeShare.Operations
import           Database.BikeShare.Utils

import           Fmt

import           ReportTime

import           Test.Tasty.HUnit


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
  void $ insertStationInformation   conn $ info   ^. response_data . info_stations

  -- Insert test station status data 1-22.
  mapM_ (\i -> do
            statusResponse <- getDecodedFileStatus $ "docs/json/2.3/station_status-"+|i|+".json"
            void $ insertStationStatus conn $ statusResponse ^. response_data . status_stations
        ) [(1 :: Int) .. (22 :: Int)]


-- | HUnit test for inserting station information.
unit_insertStationInformation :: IO ()
unit_insertStationInformation = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  stationInformationResponse <- getDecodedFileInformation "test/json/station_information.json"

  -- Insert test data.
  inserted_info <- insertStationInformation conn $ _info_stations $ stationInformationResponse ^. response_data

  assertEqual "Inserted station information" 6 (length inserted_info)


-- | HUnit test for inserting station status.
unit_insertStationStatus :: IO ()
unit_insertStationStatus = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  info    <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"
  status  <- getDecodedFileStatus      "test/json/station_status.json"

  -- Insert test data.
  inserted_info   <- insertStationInformation   conn $ info   ^. response_data . info_stations
  inserted_status <- insertStationStatus conn $ status ^. response_data . status_stations

  assertEqual "Inserted station information" 704 (length inserted_info)
  assertEqual "Inserted station status"        8 (length $ inserted_status ^. insert_inserted)


-- | HUnit test for querying station status.
unit_queryStationStatus :: IO ()
unit_queryStationStatus = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  info    <- getDecodedFileInformation  "test/json/station_information.json"
  status  <- getDecodedFileStatus       "test/json/station_status.json"

  -- Insert test data.
  inserted_info   <- insertStationInformation   conn $ info   ^. response_data . info_stations
  inserted_status <- insertStationStatus conn $ status ^. response_data . status_stations

  assertEqual "Inserted station information" 6 (length inserted_info)
  assertEqual "Inserted station status"      5 (length $ inserted_status ^. insert_inserted)

  -- Query station status.
  assertEqual "Query status (limit: 1000)" 5 . length =<< queryStationStatus  conn (Just 1000)
  assertEqual "Query status (limit: none)" 5 . length =<< queryStationStatus  conn Nothing


-- | HUnit test for inserting station information, with data from the actual API.
unit_insertStationInformationApi :: IO ()
unit_insertStationInformationApi = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  info    <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"

  -- Insert test data.
  void $ insertStationInformation conn $ info ^. response_data . info_stations


-- | HUnit test for inserting station status, with data from the actual API.
unit_insertStationStatusApi :: IO ()
unit_insertStationStatusApi = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  status  <- getDecodedFileStatus "docs/json/2.3/station_status-1.json"

  -- Should fail because station information has not been inserted.
  inserted_status <- insertStationStatus conn $ status ^. response_data . status_stations

  assertEqual "Inserted station status" [] $ inserted_status ^. insert_inserted
  assertEqual "Updated station status"  [] $ inserted_status ^. insert_deactivated

-- | HUnit test for inserting station information and status, with data from the actual API.
unit_insertStationApi :: IO ()
unit_insertStationApi = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  info    <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"
  status  <- getDecodedFileStatus      "docs/json/2.3/station_status-1.json"

  -- Insert test data.
  inserted_info   <- insertStationInformation   conn $ info   ^. response_data . info_stations
  inserted_status <- insertStationStatus conn $ status ^. response_data . status_stations

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
  conn <- setupDatabaseName dbnameTest

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
  void $ insertStationInformation   conn $ info   ^. response_data . info_stations
  void $ insertStationStatus conn $ status_1 ^. response_data . status_stations

  -- Return stations that have reported since being inserted.
  getRowsToDeactivate conn $ status_2 ^. response_data . status_stations


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
  conn <- setupDatabaseName dbnameTest

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
  void $ insertStationInformation   conn $ info   ^. response_data . info_stations
  void $ insertStationStatus conn $ status_1 ^. response_data . status_stations

  -- Return maps of updated and same API statuses
  separateNewerStatusRecords conn $ status_2 ^. response_data . status_stations


-- | HUnit test to assert that changed station status are inserted.
unit_separateNewerStatusRecordsInsert :: IO ()
unit_separateNewerStatusRecordsInsert = do
  conn <- setupDatabaseName dbnameTest

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
  void $ insertStationInformation   conn $ info   ^. response_data . info_stations
  void $ insertStationStatus conn $ status_1 ^. response_data . status_stations

  -- Find statuses that need to be updated (second round of data vs. first).
  updated <- separateNewerStatusRecords conn $ status_2 ^. response_data . status_stations

  -- Insert second round of test data (some of which have reported since the first round was inserted).
  insertStationStatus conn $ updated ^. filter_newer


-- FIXME: test fails
-- | HUnit test to assert that reinserting already-deactivated rows is a no-op.
unit_separateNewerStatusRecordsInsertTwice :: IO ()
unit_separateNewerStatusRecordsInsertTwice = do
  conn <- setupDatabaseName dbnameTest

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
  void $ insertStationInformation   conn $ info   ^. response_data . info_stations
  void $ insertStationStatus conn $ status_1 ^. response_data . status_stations


  -- Find status records that need to be updated (second round of data vs. first).
  updated_1 <- separateNewerStatusRecords conn $ status_2 ^. response_data . status_stations

  -- Insert second round of test data (some of which have reported since the first round was inserted).
  void $ insertStationStatus conn $ updated_1 ^. filter_newer


  -- Find status records that need to be updated (second round of data vs. second).
  updated_2 <- separateNewerStatusRecords conn $ status_2 ^. response_data . status_stations

  -- Insert second round of test data once again (nothing should have changed).
  insertStationStatus conn $ updated_2 ^. filter_newer


-- | HUnit test to validate that a station ID can be looked up by its name, and vice-versa.
unit_queryStationByIdAndName :: IO ()
unit_queryStationByIdAndName = do
  conn <- setupDatabaseName dbnameTest
  info <- getDecodedFileInformation "docs/json/2.3/station_information-1.json"
  void $ insertStationInformation conn $ info ^. response_data . info_stations

  assertEqual "Station ID for 'King St W / Joe Shuster Way'" (Just 7148) =<< queryStationId conn "King St W / Joe Shuster Way"
  assertEqual "Station ID for 'Wellesley Station Green P'" (Just 7001) =<< queryStationId conn "Wellesley Station Green P"
  assertEqual "Stations with name ending in 'Green P'"
    [ (7001,"Wellesley Station Green P")
    , (7050,"Richmond St E / Jarvis St Green P")
    , (7112,"Liberty St / Fraser Ave Green P")
    , (7789,"75 Holly St - Green P")
    ] =<< queryStationIdLike conn "%Green P"


-- | HUnit test to query all status records for a station between two times.
unit_queryStationStatusBetween :: IO ()
unit_queryStationStatusBetween = do
  conn <- setupDatabaseName dbnameTest
  initDBWithAllTestData conn

  -- First status for #7001 was inserted at 2023-09-15 17:16:58; last status at 2023-09-15 17:35:00.
  statusBetweenAll <- queryStationStatusBetween conn 7001
    (ReportTime $ read "2023-09-15 17:16:58")
    (ReportTime $ read "2023-09-15 17:35:00")
  assertEqual "Expected number of status records for #7001 between two valid times" 4 (length statusBetweenAll)

  -- Query for status records for #7001 between two times, where the start and end time match the first status report.
  statusBetweenFirst <- queryStationStatusBetween conn 7001
    (ReportTime $ read "2000-09-15 17:16:58") -- Moment the first status was reported.
    (ReportTime $ read "2023-09-15 17:16:58") -- Moment the first status was reported.
  assertEqual "Expected number of status records for #7001 for first status reported" 1 (length statusBetweenFirst)

  -- Query for status records for #7001 between two times, where the end time is before the first status was reported.
  statusBetweenTooEarly <- queryStationStatusBetween conn 7001
    (ReportTime $ read "2000-01-01 00:00:00") -- Arbitrary date
    (ReportTime $ read "2023-09-15 17:16:57") -- One second before first status reported.
  assertEqual "Expected number of status records for #7001 before first status reported" 0 (length statusBetweenTooEarly)

  {-
  Query for status records for #7001 between two times, where the earliest time is *after* the first status was reported,
  and the end time is *before* the first status was reported.

  NOTE: as an example, uses both 'ReportTime $ ...' and 'reportTime ...' to construct a 'ReportTime' value.
  -}
  statusBetweenBackwards <- queryStationStatusBetween conn 7001
    (ReportTime $ read "2023-09-15 17:16:59")                     -- One second after first status reported.
    (reportTime (fromGregorian 2000 01 01) (TimeOfDay 00 00 00))  -- Arbitrary date
  assertEqual "Expected number of status records for #7001 with backwards time parameters" 0 (length statusBetweenBackwards)

