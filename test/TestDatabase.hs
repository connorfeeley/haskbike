-- | Test the database.

{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module TestDatabase
     ( unit_getRowsToDeactivate
     , unit_insertStationApi
     , unit_insertStationInformation
     , unit_insertStationInformationApi
     , unit_insertStationStatus
     , unit_insertStationStatusApi
     , unit_queryStationStatus
     , unit_queryStationStatusBetween
     , unit_separateNewerStatusRecords
     , unit_separateNewerStatusRecordsInsert
     , unit_separateNewerStatusRecordsInsertTwice
     ) where

import           API.ResponseWrapper    ( response_data )
import           API.Types              ( StationInformationResponse, StationStatusResponse, _info_stations,
                                          info_stations, status_station_id, status_stations )

import           Control.Lens

import           Data.Aeson             ( FromJSON, eitherDecode )
import qualified Data.ByteString.Lazy   as BL
import           Data.Functor           ( void )

import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.Operations
import           Database.Utils

import           Fmt

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

-- | HUnit test for inserting station information.
unit_insertStationInformation :: IO ()
unit_insertStationInformation = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  stationInformationResponse <- getDecodedFile "test/json/station_information.json"
                              :: IO StationInformationResponse

  -- Insert test data.
  inserted_info <- insertStationInformation conn $ _info_stations $ stationInformationResponse ^. response_data

  assertEqual "Inserted station information" 6 (length inserted_info)


-- | HUnit test for inserting station status.
unit_insertStationStatus :: IO ()
unit_insertStationStatus = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  info    <- getDecodedFile "docs/json/2.3/station_information-1.json"
          :: IO StationInformationResponse
  status  <- getDecodedFile "test/json/station_status.json"
          :: IO StationStatusResponse

  -- Insert test data.
  inserted_info   <- insertStationInformation   conn $ info   ^. response_data . info_stations
  inserted_status <- insertUpdatedStationStatus conn $ status ^. response_data . status_stations

  assertEqual "Inserted station information" 704 (length inserted_info)
  assertEqual "Inserted station status"        8 (length $ inserted_status ^. insert_inserted)


-- | HUnit test for querying station status.
unit_queryStationStatus :: IO ()
unit_queryStationStatus = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  info    <- getDecodedFile "test/json/station_information.json"
          :: IO StationInformationResponse
  status  <- getDecodedFile "test/json/station_status.json"
          :: IO StationStatusResponse

  -- Insert test data.
  inserted_info   <- insertStationInformation   conn $ info   ^. response_data . info_stations
  inserted_status <- insertUpdatedStationStatus conn $ status ^. response_data . status_stations

  assertEqual "Inserted station information" 6 (length inserted_info)
  assertEqual "Inserted station information" 5 (length $ inserted_status ^. insert_inserted)

  -- Query station status.
  void $ queryStationStatus conn


-- | HUnit test for inserting station information, with data from the actual API.
unit_insertStationInformationApi :: IO ()
unit_insertStationInformationApi = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  info    <- getDecodedFile "docs/json/2.3/station_information-1.json"
          :: IO StationInformationResponse

  -- Insert test data.
  void $ insertStationInformation conn $ info ^. response_data . info_stations


-- | HUnit test for inserting station status, with data from the actual API.
unit_insertStationStatusApi :: IO ()
unit_insertStationStatusApi = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  status  <- getDecodedFile "docs/json/2.3/station_status-1.json"
          :: IO StationStatusResponse

  -- Insert test data.
  void $ insertUpdatedStationStatus conn $ status ^. response_data . status_stations

  -- Should fail because station information has not been inserted.
  inserted_info <- insertUpdatedStationStatus conn $ status ^. response_data . status_stations

  assertEqual "Inserted station status" [] $ inserted_info ^. insert_inserted
  assertEqual "Updated station status"  [] $ inserted_info ^. insert_deactivated

-- | HUnit test for inserting station information and status, with data from the actual API.
unit_insertStationApi :: IO ()
unit_insertStationApi = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  info    <- getDecodedFile "docs/json/2.3/station_information-1.json"
          :: IO StationInformationResponse
  status  <- getDecodedFile "docs/json/2.3/station_status-1.json"
          :: IO StationStatusResponse

  -- Insert test data.
  inserted_info   <- insertStationInformation   conn $ info   ^. response_data . info_stations
  inserted_status <- insertUpdatedStationStatus conn $ status ^. response_data . status_stations

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
  info      <- getDecodedFile "docs/json/2.3/station_information-1.json"
            :: IO StationInformationResponse
  status_1  <- getDecodedFile "docs/json/2.3/station_status-1.json"
            :: IO StationStatusResponse
  status_2  <- getDecodedFile "docs/json/2.3/station_status-2.json"
            :: IO StationStatusResponse

  -- Insert test data.
  void $ insertStationInformation   conn $ info   ^. response_data . info_stations
  void $ insertUpdatedStationStatus conn $ status_1 ^. response_data . status_stations

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
  info      <- getDecodedFile "docs/json/2.3/station_information-1.json"
            :: IO StationInformationResponse
  status_1  <- getDecodedFile "docs/json/2.3/station_status-1.json"
            :: IO StationStatusResponse
  status_2  <- getDecodedFile "docs/json/2.3/station_status-2.json"
            :: IO StationStatusResponse

  -- Insert test data.
  void $ insertStationInformation   conn $ info   ^. response_data . info_stations
  void $ insertUpdatedStationStatus conn $ status_1 ^. response_data . status_stations

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
  info      <- getDecodedFile "docs/json/2.3/station_information-1.json"
            :: IO StationInformationResponse
  status_1  <- getDecodedFile "docs/json/2.3/station_status-1.json"
            :: IO StationStatusResponse
  status_2  <- getDecodedFile "docs/json/2.3/station_status-2.json"
            :: IO StationStatusResponse

  -- Insert test data.
  void $ insertStationInformation   conn $ info   ^. response_data . info_stations
  void $ insertUpdatedStationStatus conn $ status_1 ^. response_data . status_stations

  -- Find statuses that need to be updated (second round of data vs. first).
  updated <- separateNewerStatusRecords conn $ status_2 ^. response_data . status_stations

  -- Insert second round of test data (some of which have reported since the first round was inserted).
  insertUpdatedStationStatus conn $ updated ^. filter_newer


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
  info      <- getDecodedFile "docs/json/2.3/station_information-1.json"
            :: IO StationInformationResponse
  status_1  <- getDecodedFile "docs/json/2.3/station_status-1.json"
            :: IO StationStatusResponse
  status_2  <- getDecodedFile "docs/json/2.3/station_status-2.json"
            :: IO StationStatusResponse

  -- Insert first round of test data.
  void $ insertStationInformation   conn $ info   ^. response_data . info_stations
  void $ insertUpdatedStationStatus conn $ status_1 ^. response_data . status_stations


  -- Find status records that need to be updated (second round of data vs. first).
  updated_1 <- separateNewerStatusRecords conn $ status_2 ^. response_data . status_stations

  -- Insert second round of test data (some of which have reported since the first round was inserted).
  void $ insertUpdatedStationStatus conn $ updated_1 ^. filter_newer


  -- Find status records that need to be updated (second round of data vs. second).
  updated_2 <- separateNewerStatusRecords conn $ status_2 ^. response_data . status_stations

  -- Insert second round of test data once again (nothing should have changed).
  insertUpdatedStationStatus conn $ updated_2 ^. filter_newer

-- | HUnit test to query all status records for a station between two times.
unit_queryStationStatusBetween :: IO ()
unit_queryStationStatusBetween = do
  conn <- setupDatabaseName dbnameTest

  _statusBetween <- doQueryStationStatusBetween conn

  pure ()

-- | Query all status records for a station between two times.
doQueryStationStatusBetween :: Connection         -- ^ Database connection
                            -> IO [StationStatus] -- ^ Result of querying station status between two times.
doQueryStationStatusBetween conn = do
  -- Insert station information.
  info      <- getDecodedFile "docs/json/2.3/station_information-1.json"
            :: IO StationInformationResponse
  status      <- getDecodedFile "docs/json/2.3/station_status-1.json"
            :: IO StationStatusResponse
  void $ insertStationInformation   conn $ info   ^. response_data . info_stations
  void $ insertUpdatedStationStatus        conn $ status   ^. response_data . status_stations

  -- Insert test station status data 1-22.
  mapM_ (\i -> do
            statusResponse <- getDecodedFile $ "docs/json/2.3/station_status-"+|i|+".json"
            updated <- separateNewerStatusRecords conn $ statusResponse ^. response_data . status_stations
            inserted <- insertUpdatedStationStatus conn $ updated ^. filter_newer
            putStrLn $ "Updated: "+| length (updated ^.filter_newer) |+ " Inserted " +| length (inserted ^. insert_inserted) |+" status " +|i|+ ""
        ) [(1 :: Int) .. (22 :: Int)]

  -- Query database for station status between two times.
  statusBetween <- queryStationStatusBetween conn 7001 1694798218 1694799300

  pure statusBetween
