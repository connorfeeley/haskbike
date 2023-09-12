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
import           Database.Migrations                      (migrateDB)

import           Database.StationInformation              (name)
import qualified Database.StationInformation              as DSI
import           Database.StationStatus                   (num_bikes_available,
                                                           num_bikes_disabled,
                                                           num_docks_available,
                                                           num_docks_disabled,
                                                           station_id)
import qualified Database.StationStatus                   as DSS

import qualified StationInformation                       as SI
import qualified StationStatus                            as SS

import           Test.Tasty.HUnit

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple

import           Text.Pretty.Simple                       (pPrint, pPrintString)

import           Control.Lens
import           Data.Aeson                               (eitherDecode')
import qualified Data.ByteString                          as B
import           Data.ByteString.Lazy                     (fromStrict)
import           Data.FileEmbed                           (embedDir, embedFile)
import           Data.String                              (fromString)


-- | Embedded test JSON data.
testJson :: [(FilePath, B.ByteString)]
testJson = $(embedDir "test/json")

-- | Get test JSON corresponding to a file path.
lookupJson :: String -> Maybe B.ByteString
lookupJson fileName = lookup fileName testJson

-- | Construct query to drop a table using cascade.
dropCascade :: String -> Query
dropCascade tableName = fromString $ "DROP TABLE IF EXISTS " ++ tableName ++" CASCADE"

-- | Establish a connection to the database.
connectDb :: IO Connection
connectDb =
  connectPostgreSQL $ fromString "host=localhost port=5432 dbname=haskbike connect_timeout=10"

setupDatabase :: IO Connection
setupDatabase = do
  -- Connect to the database.
  conn <- connectDb

  -- Drop all tables.
  _ <- execute_ conn $ dropCascade "station_status"
  _ <- execute_ conn $ dropCascade "station_information"
  _ <- execute_ conn $ dropCascade "beam_migration"
  _ <- execute_ conn $ dropCascade "beam_version"

  -- Initialize the database.
  _ <- migrateDB conn

  pPrintString "Database reinitialization complete."

  pure conn


-- | HUnit test for inserting station information.
unit_insertStationInformation :: IO ()
unit_insertStationInformation = do
  -- Connect to the database.
  conn <- setupDatabase

  _stationInformation <- insertStationInformation conn
  pure ()

insertStationInformation :: Connection -> IO [DSI.StationInformation]
insertStationInformation conn = do
  let stationsJson = fromStrict $(embedFile "test/json/station_information.json")
  let mStations = eitherDecode' stationsJson :: Either String SI.StationInformationResponse
  case mStations of
    Right stations -> do
      insertedStations <- runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
        insert (DBS.bikeshareDb ^. DBS.bikeshareStationInformation) $
        insertExpressions $ map DSI.fromJSONToBeamStationInformation (SI.stations stations)
      pure insertedStations
    Left errorMsg -> assertFailure $ "Error decoding station information" ++ errorMsg


-- | HUnit test for inserting station status.
unit_insertStationStatus :: IO ()
unit_insertStationStatus = do
  -- Connect to the database.
  conn <- setupDatabase

  _stationInformation <- insertStationInformation conn
  _stationStatus      <- insertStationStatus conn
  pure ()

insertStationStatus :: Connection -> IO [DSS.StationStatus]
insertStationStatus conn = do
  let stationsJson = fromStrict $(embedFile "test/json/station_status.json")
  let mStations = eitherDecode' stationsJson :: Either String SS.StationStatusResponse
  case mStations of
    Right stations -> do
      insertedStations <- runBeamPostgresDebug pPrintString conn $ runInsertReturningList $
        insert (DBS.bikeshareDb ^. DBS.bikeshareStationStatus) $
        insertExpressions $ map DSS.fromJSONToBeamStationStatus (SS.stations stations)
      pure insertedStations
    Left errorMsg -> assertFailure $ "Error decoding station status" ++ errorMsg


-- | HUnit test for querying station status.
unit_queryStationStatus :: IO ()
unit_queryStationStatus = do
  -- Connect to the database.
  conn <- setupDatabase

  _stationInformation <- insertStationInformation conn
  _stationStatus      <- insertStationStatus conn
  queryStationStatus conn >>= pPrint

queryStationStatus :: Connection -> IO [(DSI.StationInformation, DSS.StationStatus)]
queryStationStatus conn = do
  insertedStations <- runBeamPostgresDebug pPrintString conn $ runSelectReturningList $ select $ do
    info <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationInformation)
    status <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationStatus)
    guard_ (DSS._station_id status `references_` info)
    -- station_status <- leftJoin_ (all_(DBS.bikeshareDb ^. DBS.bikeshareStationStatus))
    --   (\station_status -> DSS._station_id station_status `references_` station_information)
    -- guard_ (isJust_ station_status)
    pure (info, status)
  pure insertedStations

queryStationStatusFields conn = do
  runBeamPostgresDebug pPrintString conn $ runSelectReturningList $ select $ do
    info <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationInformation)
    status <- all_ (DBS.bikeshareDb ^. DBS.bikeshareStationStatus)
    guard_ (DSS._station_id status `references_` info)
    pure ( info^.name
         , status^.num_bikes_available
         , status^.num_bikes_disabled
         , status^.num_docks_available
         , status^.num_docks_disabled
         )
