{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database.BikeShare.Migrations where

import           AppEnv

import           Control.Monad                                ( forM_, void )

import           Data.Pool

import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres
import           Database.Beam.Postgres.CustomTypes
import qualified Database.Beam.Postgres.Migrate               as PG
import           Database.BikeShare
import           Database.BikeShare.EndpointQueried
import           Database.BikeShare.Tables.QueryLogs
import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationStatus
import           Database.BikeShare.Tables.SystemInformation
import           Database.PostgreSQL.Simple                   ( execute_ )

import           UnliftIO


{- Set up entire database.

Can render with:
  >>> (Text.Pretty.Simple.pPrint . Database.Beam.Postgres.Migrate.migrateScript) initialSetupStep
  >>> (Text.Pretty.Simple.pPrint . take 1 . drop 14 . Database.Beam.Postgres.Migrate.migrateScript) initialSetupStep
-}
initialSetup :: Migration Postgres (CheckedDatabaseSettings Postgres BikeshareDb)
initialSetup = do
  BikeshareDb
  <$> enumSetup -- Create custom Postgres enum type for the different endpoints that are queried.
  <*> createStationInformation
  <*> createStationStatus
  <*> createSystemInformation
  <*> createSystemInformationCount
  <*> createQueries


initialSetupStep :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres BikeshareDb)
initialSetupStep = migrationStep
  "initial_setup"
  (const initialSetup)

-- Beam's simple runner doesn't run destructive migrations
-- by default, so we have to override that.
allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks
  { runIrreversibleHook = pure True }

migrateDB :: AppM (Maybe (CheckedDatabaseSettings Postgres BikeshareDb))
migrateDB = do
  db <- withPostgres $ bringUpToDateWithHooks
    allowDestructive
    PG.migrationBackend
    initialSetupStep

  pool <- withConnPool
  void . liftIO . withResource pool $ \conn -> do
    forM_ extraInfoMigrations $ \migration -> do
      execute_ conn migration
  pure db

enumSetupStep :: MigrationSteps Postgres () (CheckedDatabaseEntity Postgres db (PgType EndpointQueried))
enumSetupStep = migrationStep
  "enum_setup"
  (const enumSetup)

enumSetup :: Migration Postgres (CheckedDatabaseEntity Postgres db (PgType EndpointQueried))
enumSetup = createEndpointQueriedEnum

