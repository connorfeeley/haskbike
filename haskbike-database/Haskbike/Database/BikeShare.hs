-- | Database schema for BikeShare.

module Haskbike.Database.BikeShare
     ( module Haskbike.Database.Schema.V004.BikeShare
     , module Haskbike.Database.Schema.V004.Migrations
     , evaluateBikeShareDatabase
     , printDatabaseMigration
     ) where

import           Database.Beam.Migrate                    ( CheckedDatabaseSettings, evaluateDatabase )
import           Database.Beam.Postgres                   ( Postgres )
import qualified Database.Beam.Postgres.Migrate           as Pg

import           Haskbike.Database.Schema.V004.BikeShare
import           Haskbike.Database.Schema.V004.Migrations

import           Text.Pretty.Simple                       ( pPrint )

-- * Misc/unused database schema inspection utilities.

-- Pretty-print the database DDL.
printDatabaseMigration :: IO ()
printDatabaseMigration = pPrint (Pg.migrateScript migration)

-- Evaluate a 'MigrationSteps' to produce a 'CheckedDatabaseSettings'.
evaluateBikeShareDatabase :: CheckedDatabaseSettings Postgres BikeshareDb
evaluateBikeShareDatabase = evaluateDatabase migration
