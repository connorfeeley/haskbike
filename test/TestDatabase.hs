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

import           Database.BikeShare
import           Database.Operations
import           Database.Utils

import           API.Types            (status_stations, info_stations)

import           Test.Tasty.HUnit

import           Data.Aeson           (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor         (void)
import           Text.Pretty.Simple   (pPrintString)


-- | Helper function to decode a JSON file.
decodeFile :: FromJSON a => FilePath -> IO (Either String a)
decodeFile file = eitherDecode <$> BL.readFile file

-- | HUnit test for inserting station information.
unit_insertStationInformation :: IO ()
unit_insertStationInformation = do
  -- Connect to the database.
  conn <- setupDatabase

  stationInformationResponse <- decodeFile "test/json/station_information.json"

  -- Insert test data.
  case stationInformationResponse of
    Right stations -> void $ insertStationInformation conn $ info_stations stations
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
      void $ insertStationInformation conn $ info_stations   info
      void $ insertStationStatus      conn $ status_stations status
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
      void $ insertStationInformation conn $ info_stations   info
      void $ insertStationStatus      conn $ status_stations status
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
      void $ insertStationInformation conn $ info_stations info
  -- pure stationInformationResponse

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
      void $ insertStationStatus conn $ status_stations status

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
      void $ insertStationInformation conn $ info_stations   info
      void $ insertStationStatus      conn $ status_stations status

-- | HUnit test for querying which station status have reported.
unit_queryUpdatedStatus :: IO ()
unit_queryUpdatedStatus = do
  updated <- doQueryUpdatedStatus
  case length updated of
    expected_count@302  -> void $ pPrintString $ "\t\t\t\t " ++ "Found " ++ show expected_count ++ " stations that have reported since being inserted"
    count               -> assertFailure $ show count ++ " stations have reported - expected 302"

doQueryUpdatedStatus :: IO [StationStatusT Identity]
doQueryUpdatedStatus = do
  conn <- setupDatabase

  stationInformationResponse    <- decodeFile "docs/json/2.3/station_information-1.json"
  stationStatusResponse         <- decodeFile "docs/json/2.3/station_status-1.json"
  
  -- updatedStationStatusResponse       <- runQueryWithEnv stationStatus
  updatedStationStatusResponse <- decodeFile "docs/json/2.3/station_status-2.json"

  case (stationInformationResponse, stationStatusResponse) of
    (Right info, Right status) -> do
      -- Insert test data.
      void $ insertStationInformation conn $ info_stations   info
      void $ insertStationStatus      conn $ status_stations status
    ( _        , _           ) -> assertFailure "Error loading test data"

  case updatedStationStatusResponse of
    Left   err          -> assertFailure $ "Error decoding station status JSON: " ++ err
    (Right api_status)  -> do
      -- Return stations that have reported since being inserted.
      queryUpdatedStatus conn $ status_stations api_status
