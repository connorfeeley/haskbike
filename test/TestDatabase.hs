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

module TestDatabase where

import qualified Database.BikeShare                       as DBS
import           Database.Operations
import           Database.Types
import           Database.Utils

import           API.Types                                (StationInformationResponse (..),
                                                           StationStatusResponse (..))
import           Client

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

unit_insertStationInformationApi :: IO ()
unit_insertStationInformationApi = do
  -- Connect to the database.
  conn <- setupDatabase

  -- Query API for station information.
  stationInformationResponse <- runQueryWithEnv stationInformation

  case stationInformationResponse of
    (Left err)    -> assertFailure $ "Error querying API: " ++ show err
    (Right info)  -> do
      -- Insert station information into database.
      void $ -- Suppress return value.
        runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
        insert (DBS.bikeshareDb ^. DBS.bikeshareStationInformation) $
        insertExpressions $ map fromJSONToBeamStationInformation (info_stations info)

unit_insertStationStatusApi :: IO ()
unit_insertStationStatusApi = do
  -- Connect to the database.
  conn <- setupDatabase

  -- Query API for station status.
  stationStatusResponse <- runQueryWithEnv stationStatus

  case stationStatusResponse of
    (Left err)      -> assertFailure $ "Error querying API: " ++ show err
    (Right status)  -> do
      -- Insert station status into database.
      void $ -- Suppress return value.
        runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
        insert (DBS.bikeshareDb ^. DBS.bikeshareStationStatus) $
        insertExpressions $ map fromJSONToBeamStationStatus (status_stations status)

unit_insertStationBothApi :: IO ()
unit_insertStationBothApi = do
  -- Connect to the database.
  conn <- setupDatabase

  -- Query API.
  stationInformationResponse  <- runQueryWithEnv stationInformation
  stationStatusResponse       <- runQueryWithEnv stationStatus

  case (stationInformationResponse, stationStatusResponse) of
    (Left err_info, Left err_status)  -> assertFailure $ "Error querying API: " ++ show err_info ++ " " ++ show err_status
    (Left err_info, _)                -> assertFailure $ "Error querying API: " ++ show err_info
    (_, Left err_status)              -> assertFailure $ "Error querying API: " ++ show err_status
    (Right info, Right status)        -> do
      -- Insert station information into database.
      void $ -- Suppress return value.
        runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
        insert (DBS.bikeshareDb ^. DBS.bikeshareStationInformation) $
        insertExpressions $ map fromJSONToBeamStationInformation (info_stations info)

      -- Insert station status into database.
      void $ -- Suppress return value.
        runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
        insert (DBS.bikeshareDb ^. DBS.bikeshareStationStatus) $
        insertExpressions $ map fromJSONToBeamStationStatus (status_stations status)
