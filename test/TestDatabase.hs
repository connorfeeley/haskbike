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

module TestDatabase where

import           Database.Beam
import           Database.Beam.Postgres

import           Database.BikeShare
import           Database.Operations
import           Database.Utils

import           API.Types              (_info_stations, info_stations, status_stations)
import qualified API.Types              as AT

import           Test.Tasty.HUnit

import           API.ResponseWrapper (ResponseWrapper (..), response_data)
import           Data.Aeson             (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy   as BL
import           Data.Functor           (void)
import qualified Data.Map               as Map
import Control.Lens


-- | Helper function to decode a JSON file.
decodeFile :: FromJSON a => FilePath -> IO (Either String (ResponseWrapper a))
decodeFile file = eitherDecode <$> BL.readFile file


-- | HUnit test for inserting station information.
unit_insertStationInformation :: IO ()
unit_insertStationInformation = do
  -- Connect to the database.
  conn <- setupDatabase

  stationInformationResponse <- decodeFile "test/json/station_information.json"

  -- Insert test data.
  case stationInformationResponse of
    Right stations -> void $ insertStationInformation conn $ _info_stations $ stations ^. response_data
    Left  err      -> assertFailure $ "Error decoding station information JSON: " ++  err


-- | HUnit test for inserting station status.
unit_insertStationStatus :: IO ()
unit_insertStationStatus = do
  -- Connect to the database.
  conn <- setupDatabase

  stationInformationResponse  <- decodeFile "test/json/station_information.json"
  stationStatusResponse       <- decodeFile "test/json/station_status.json"

  -- Insert test data.
  case (stationInformationResponse, stationStatusResponse) of
    (Right info , Right status  ) -> do
      void $ insertStationInformation   conn $  info   ^. response_data . info_stations
      void $ insertUpdatedStationStatus conn $ status ^. response_data . status_stations
    ( _        , _            ) -> assertFailure "Error loading test data"


-- | HUnit test for querying station status.
unit_queryStationStatus :: IO ()
unit_queryStationStatus = do
  -- Connect to the database.
  conn <- setupDatabase

  stationInformationResponse  <- decodeFile "test/json/station_information.json"
  stationStatusResponse       <- decodeFile "test/json/station_status.json"

  -- Insert test data.
  case (stationInformationResponse, stationStatusResponse) of
    (Right info , Right status  ) -> do
      void $ insertStationInformation   conn $ info   ^. response_data . info_stations
      void $ insertUpdatedStationStatus conn $ status ^. response_data . status_stations
    ( _        , _            ) -> assertFailure "Error loading test data"

  -- Query station status.
  void $ queryStationStatus conn -- >>= pPrintCompact


-- | HUnit test for inserting station information, with data from the actual API.
unit_insertStationInformationApi :: IO ()
unit_insertStationInformationApi = do
  -- Connect to the database.
  conn <- setupDatabase

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
  conn <- setupDatabase

  -- Query API for station status.
  -- stationStatusResponse <- runQueryWithEnv stationStatus
  stationStatusResponse <- decodeFile "docs/json/2.3/station_status-1.json"

  case stationStatusResponse of
    (Left err    )  -> assertFailure $ "Error querying API: " ++ show err
    (Right status)  -> do
      void $ insertUpdatedStationStatus conn $ status ^. response_data . status_stations


-- | HUnit test for inserting station information and status, with data from the actual API.
unit_insertStationApi :: IO ()
unit_insertStationApi = do
  -- Connect to the database.
  conn <- setupDatabase

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
      void $ insertStationInformation   conn $ info   ^. response_data . info_stations
      void $ insertUpdatedStationStatus conn $ status ^. response_data . status_stations


-- | HUnit test for querying which station status have reported.
unit_queryUpdatedStatus :: IO ()
unit_queryUpdatedStatus = do
  conn <- setupDatabase

  updated <- doQueryUpdatedStatus conn
  assertEqual "Updated stations" 302 (length updated)

-- | Query updated station status and return a list of database statuses.
doQueryUpdatedStatus :: Connection -> IO [StationStatusT Identity]
doQueryUpdatedStatus conn = do
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
      queryUpdatedStatus conn $ api_status ^. response_data . status_stations


-- | HUnit test for querying which station status have reported.
unit_queryUpdatedStatus' :: IO ()
unit_queryUpdatedStatus' = do
  conn <- setupDatabase

  updated_base <- doQueryUpdatedStatus' conn

  let updated = updated_base ^. _1
  let same    = updated_base ^. _2

  assertEqual "Updated stations" 302 (Map.size updated)
  assertEqual "Same    stations" 407 (Map.size same)

-- | Query updated station status and return a list of API statuses.
doQueryUpdatedStatus' :: Connection -> IO (Map.Map Int AT.StationStatus, Map.Map Int AT.StationStatus)
doQueryUpdatedStatus' conn = do
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
      filterStatusBase conn $ api_status ^. response_data . status_stations


-- | HUnit test to assert that changed station status are inserted.
unit_queryUpdatedStatusInsert :: IO ()
unit_queryUpdatedStatusInsert = do
  conn <- setupDatabase

  updated_base <- doQueryUpdatedStatusInsert conn

  let updated = updated_base ^. _1
  let same    = updated_base ^. _2

  -- Assert no stations are updated.
  assertEqual "Updated stations"   0 (Map.size updated)
  assertEqual "Same    stations" 709 (Map.size same)

-- | Query updated station status and return a list of API statuses.
doQueryUpdatedStatusInsert :: Connection -> IO (Map.Map Int AT.StationStatus, Map.Map Int AT.StationStatus)
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
      updated <- filterStatusUpdated conn $ api_status ^. response_data . status_stations
      -- Insert second round of test data (some of which have reported since the first round was inserted).
      _inserted <- insertUpdatedStationStatus conn updated
      -- Find statuses that need to be updated (second round of data vs. second), returning maps of (updated, same).
      filterStatusBase conn $ api_status ^. response_data . status_stations
