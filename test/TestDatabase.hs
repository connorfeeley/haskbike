-- | Test the database.

{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
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
     , unit_separateNewerStatusRecords
     , unit_separateNewerStatusRecordsInsert
     ) where

import           API.ResponseWrapper    ( ResponseWrapper (..), response_data )
import           API.Types              ( _info_stations, info_stations, status_station_id, status_stations )

import           Control.Lens

import           Data.Aeson             ( FromJSON, eitherDecode )
import qualified Data.ByteString.Lazy   as BL
import           Data.Functor           ( void )

import           Database.Beam.Postgres
import           Database.BikeShare
import           Database.Operations
import           Database.Utils

import           Test.Tasty.HUnit


-- | Helper function to decode a JSON file.
decodeFile :: FromJSON a => FilePath -> IO (Either String (ResponseWrapper a))
decodeFile file = eitherDecode <$> BL.readFile file


-- | HUnit test for inserting station information.
unit_insertStationInformation :: IO ()
unit_insertStationInformation = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  stationInformationResponse <- decodeFile "test/json/station_information.json"

  -- Insert test data.
  case stationInformationResponse of
    Left  err      -> assertFailure $ "Error decoding station information JSON: " ++  err
    Right stations -> do
      inserted_info <- insertStationInformation conn $ _info_stations $ stations ^. response_data
      assertEqual "Inserted station information" 6 (length inserted_info)


-- | HUnit test for inserting station status.
unit_insertStationStatus :: IO ()
unit_insertStationStatus = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  stationInformationResponse  <- decodeFile "docs/json/2.3/station_information-1.json"
  stationStatusResponse       <- decodeFile "test/json/station_status.json"

  -- Insert test data.
  case (stationInformationResponse, stationStatusResponse) of
    (Right info , Right status  ) -> do
      inserted_info   <- insertStationInformation   conn $  info   ^. response_data . info_stations
      inserted_status <- insertUpdatedStationStatus conn $ status ^. response_data . status_stations

      assertEqual "Inserted station information" 704 (length inserted_info)
      assertEqual "Inserted station status"        8 (length $ inserted_status ^. insert_inserted)
    ( _        , _            ) -> assertFailure "Error loading test data"


-- | HUnit test for querying station status.
unit_queryStationStatus :: IO ()
unit_queryStationStatus = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  stationInformationResponse  <- decodeFile "test/json/station_information.json"
  stationStatusResponse       <- decodeFile "test/json/station_status.json"

  -- Insert test data.
  case (stationInformationResponse, stationStatusResponse) of
    (Right info , Right status  ) -> do
      inserted_info   <- insertStationInformation   conn $ info   ^. response_data . info_stations
      inserted_status <- insertUpdatedStationStatus conn $ status ^. response_data . status_stations
      assertEqual "Inserted station information" 6 (length inserted_info)
      assertEqual "Inserted station information" 5 (length $ inserted_status ^. insert_inserted)
    ( _        , _            ) -> assertFailure "Error loading test data"

  -- Query station status.
  void $ queryStationStatus conn -- >>= pPrintCompact


-- | HUnit test for inserting station information, with data from the actual API.
unit_insertStationInformationApi :: IO ()
unit_insertStationInformationApi = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  -- Query API for station information.
  -- stationInformationResponse <- runQueryWithEnv stationInformation
  stationInformationResponse <- decodeFile "docs/json/2.3/station_information-1.json"

  case stationInformationResponse of
    (Left err)   -> assertFailure $ "Error loading test data: " ++ show err
    (Right info) -> do
      void $ insertStationInformation conn $ info ^. response_data . info_stations


-- | HUnit test for inserting station status, with data from the actual API.
unit_insertStationStatusApi :: IO ()
unit_insertStationStatusApi = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  -- Query API for station status.
  -- stationStatusResponse <- runQueryWithEnv stationStatus
  stationStatusResponse <- decodeFile "docs/json/2.3/station_status-1.json"

  case stationStatusResponse of
    (Left err    )  -> assertFailure $ "Error querying API: " ++ show err
    (Right status)  -> do
      -- Should fail because station information has not been inserted.
      inserted_info <- insertUpdatedStationStatus conn $ status ^. response_data . status_stations

      assertEqual "Inserted station information" [] $ inserted_info ^. insert_inserted
      assertEqual "Updated station information"  [] $ inserted_info ^. insert_deactivated

-- | HUnit test for inserting station information and status, with data from the actual API.
unit_insertStationApi :: IO ()
unit_insertStationApi = do
  -- Connect to the database.
  conn <- setupDatabaseName dbnameTest

  -- Query API.
  -- stationInformationResponse  <- runQueryWithEnv stationInformation
  -- stationStatusResponse       <- runQueryWithEnv stationStatus
  stationInformationResponse  <- decodeFile "docs/json/2.3/station_information-1.json"
  stationStatusResponse       <- decodeFile "docs/json/2.3/station_status-1.json"

  case (stationInformationResponse, stationStatusResponse) of
    (Left err_info, Left err_status)  -> assertFailure $ "Error querying API: " ++ show err_info ++ " " ++ show err_status
    (Left err_info, _              )  -> assertFailure $ "Error querying API: " ++ show err_info
    (_            , Left err_status)  -> assertFailure $ "Error querying API: " ++ show err_status
    (Right info   , Right status   )  -> do
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
  stationInformationResponse    <- decodeFile "docs/json/2.3/station_information-1.json"
  stationStatusResponse         <- decodeFile "docs/json/2.3/station_status-1.json"

  -- updatedStationStatusResponse       <- runQueryWithEnv stationStatus
  updatedStationStatusResponse <- decodeFile "docs/json/2.3/station_status-2.json"

  case (stationInformationResponse, stationStatusResponse) of
    (Right info, Right status) -> do
      -- Insert test data.
      void $ insertStationInformation   conn $ info   ^. response_data . info_stations
      void $ insertUpdatedStationStatus conn $ status ^. response_data . status_stations
    ( _        , _           ) -> assertFailure "Error loading test data"

  case updatedStationStatusResponse of
    Left   err          -> assertFailure $ "Error decoding station status JSON: " ++ err
    (Right api_status)  -> do
      -- Return stations that have reported since being inserted.
      getRowsToDeactivate conn $ api_status ^. response_data . status_stations


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
  stationInformationResponse    <- decodeFile "docs/json/2.3/station_information-1.json"
  stationStatusResponse         <- decodeFile "docs/json/2.3/station_status-1.json"

  -- updatedStationStatusResponse       <- runQueryWithEnv stationStatus
  updatedStationStatusResponse <- decodeFile "docs/json/2.3/station_status-2.json"

  case (stationInformationResponse, stationStatusResponse) of
    (Right info, Right status) -> do
      -- Insert test data.
      void $ insertStationInformation   conn $ info   ^. response_data . info_stations
      void $ insertUpdatedStationStatus conn $ status ^. response_data . status_stations
    ( _        , _           ) -> assertFailure "Error loading test data"

  case updatedStationStatusResponse of
    Left   err          -> assertFailure $ "Error decoding station status JSON: " ++ err
    (Right api_status)  -> do
      -- Return maps of updated and same API statuses
      separateNewerStatusRecords conn $ api_status ^. response_data . status_stations


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
  updated <- doQueryUpdatedStatusInsert conn

  -- Assert no stations are updated.
  -- assertEqual "Updated stations" 0 (length $ updated ^. insert_updated)
  -- assertEqual "Inserted stations" 402 (length $ updated ^. insert_inserted)
  pure ()

-- | Query updated station status and return a list of API statuses.
doQueryUpdatedStatusInsert :: Connection -> IO (InsertStatusResult, InsertStatusResult)
doQueryUpdatedStatusInsert conn = do
  stationInformationResponse    <- decodeFile "docs/json/2.3/station_information-1.json"
  stationStatusResponse         <- decodeFile "docs/json/2.3/station_status-1.json"

  -- updatedStationStatusResponse       <- runQueryWithEnv stationStatus
  updatedStationStatusResponse <- decodeFile "docs/json/2.3/station_status-2.json"

  case (stationInformationResponse, stationStatusResponse) of
    (Right info, Right status) -> do
      -- Insert first round of test data.
      void $ insertStationInformation   conn $ info   ^. response_data . info_stations
      void $ insertUpdatedStationStatus conn $ status ^. response_data . status_stations
    ( _        , _           ) -> assertFailure "Error loading test data"

  case updatedStationStatusResponse of
    Left   err          -> assertFailure $ "Error decoding station status JSON: " ++ err
    (Right api_status)  -> do
      -- Find statuses that need to be updated (second round of data vs. first).
      updated <- separateNewerStatusRecords conn $ api_status ^. response_data . status_stations

      -- Insert second round of test data (some of which have reported since the first round was inserted).
      foo <- insertUpdatedStationStatus conn $ updated ^. filter_newer

      -- Insert second round of test data AGAIN.
      bar <- insertUpdatedStationStatus conn $ updated ^. filter_newer
      pure (foo, bar)
