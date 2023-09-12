-- | Test the database.

{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module TestDatabase where

import qualified Database.BikeShare                       as DBS
import           Database.Types
import           Database.Utils

import           API.Types                                (StationInformationResponse (..),
                                                           StationStatusResponse (..))
import qualified Client
import qualified TestClient

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres

import           Text.Pretty.Simple

import           Control.Lens
import           Data.Aeson                               (decode)
import           Data.Aeson.Types                         (FromJSON)
import qualified Data.ByteString                          as B
import           Data.ByteString.Lazy                     (fromStrict)
import           Data.FileEmbed                           (embedDir)
import           Data.Functor                             (void)
import           Test.Tasty.HUnit

-- | Embedded test JSON data.
testJson :: [(FilePath, B.ByteString)]
testJson = $(embedDir "test/json")

-- | Get test JSON corresponding to a file path.
lookupJson :: String -> Maybe B.ByteString
lookupJson fileName = lookup fileName testJson

testValuesInformation :: Maybe StationInformationResponse
testValuesInformation = getTestValues "station_information.json"
testValuesStatus      :: Maybe StationStatusResponse
testValuesStatus      = getTestValues "station_status.json"

getTestValues :: FromJSON a => String -> Maybe a
getTestValues fileName = (decode <$> fromStrict) =<< lookupJson fileName

-- | HUnit test for inserting station information.
unit_insertStationInformation :: IO ()
unit_insertStationInformation = do
  -- Connect to the database.
  conn <- setupDatabase

  case testValuesInformation of
    Just stations -> void $ insertStationInformation conn stations
    Nothing       -> assertFailure "Error decoding station information JSON"
  pure ()

insertStationInformation :: Connection -> StationInformationResponse -> IO [StationInformation]
insertStationInformation conn stations = do
  runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
    insert (DBS.bikeshareDb ^. DBS.bikeshareStationInformation) $
    insertExpressions $ map fromJSONToBeamStationInformation (info_stations stations)

-- | HUnit test for inserting station status.
unit_insertStationStatus :: IO ()
unit_insertStationStatus = do
  -- Connect to the database.
  conn <- setupDatabase

  case testValuesInformation of
    Just stations -> void $ insertStationInformation conn stations
    Nothing       -> assertFailure "Error decoding station information JSON"
  case testValuesStatus of
    Just stations -> void $ insertStationStatus conn stations
    Nothing       -> assertFailure "Error decoding station status JSON"
  pure ()

insertStationStatus :: Connection -> StationStatusResponse -> IO [StationStatus]
insertStationStatus conn status = do
  runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
    insert (DBS.bikeshareDb ^. DBS.bikeshareStationStatus) $
    insertExpressions $ map fromJSONToBeamStationStatus (status_stations status)

-- | HUnit test for querying station status.
unit_queryStationStatus :: IO ()
unit_queryStationStatus = do
  -- Connect to the database.
  conn <- setupDatabase

  case testValuesInformation of
    Just stations -> void $ insertStationInformation conn stations
    Nothing       -> assertFailure "Error decoding station information JSON"
  case testValuesStatus of
    Just stations -> void $ insertStationStatus conn stations
    Nothing       -> assertFailure "Error decoding station status JSON"
  queryStationStatus conn >>= pPrint

queryStationStatus :: Connection -> IO [(StationInformation, StationStatus)]
queryStationStatus conn = do
  runBeamPostgresDebug pPrintString conn $ runSelectReturningList $ select $ do
    info <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationInformation)
    status <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationStatus)
    guard_ (_status_station_id status `references_` info)
    -- station_status <- leftJoin_ (all_(DBS.bikeshareDb ^. DBS.bikeshareStationStatus))
    --   (\station_status -> _station_id station_status `references_` station_information)
    -- guard_ (isJust_ station_status)
    pure (info, status)

queryStationStatusFields conn =
  runBeamPostgresDebug pPrintString conn $ runSelectReturningList $ select $ do
  info <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationInformation)
  status <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationStatus)
  guard_ (_status_station_id status `references_` info)
  pure ( info^.info_name
       , status^.status_num_bikes_available
       , status^.status_num_bikes_disabled
       , status^.status_num_docks_available
       , status^.status_num_docks_disabled
       )

unit_insertStationInformationApi :: IO ()
unit_insertStationInformationApi = do
  -- Connect to the database.
  conn <- setupDatabase

  -- Query API for station information.
  stationInformationResponse <- TestClient.queryApi Client.stationInformation

  -- Insert station information into database.
  void $ -- Suppress return value.
    runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
    insert (DBS.bikeshareDb ^. DBS.bikeshareStationInformation) $
    insertExpressions $ map fromJSONToBeamStationInformation (info_stations stationInformationResponse)

unit_insertStationStatusApi :: IO ()
unit_insertStationStatusApi = do
  -- Connect to the database.
  conn <- setupDatabase

  -- Query API for station status.
  stationStatusResponse <- TestClient.queryApi Client.stationStatus

  -- Insert station status into database.
  void $ -- Suppress return value.
    runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
    insert (DBS.bikeshareDb ^. DBS.bikeshareStationStatus) $
    insertExpressions $ map fromJSONToBeamStationStatus (status_stations stationStatusResponse)

unit_insertStationBothApi :: IO ()
unit_insertStationBothApi = do
  -- Connect to the database.
  conn <- setupDatabase

  -- Query API.
  stationInformationResponse <- TestClient.queryApi Client.stationInformation
  stationStatusResponse <- TestClient.queryApi Client.stationStatus

  -- Insert station status into database.
  void $ -- Suppress return value.
    runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
    insert (DBS.bikeshareDb ^. DBS.bikeshareStationInformation) $
    insertExpressions $ map fromJSONToBeamStationInformation (info_stations stationInformationResponse)

  void $ -- Suppress return value.
    runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
    insert (DBS.bikeshareDb ^. DBS.bikeshareStationStatus) $
    insertExpressions $ map fromJSONToBeamStationStatus (status_stations stationStatusResponse)
