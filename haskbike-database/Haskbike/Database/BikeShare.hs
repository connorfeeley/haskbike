-- | Database schema for BikeShare.

module Haskbike.Database.BikeShare
     ( module Haskbike.Database.Schema.V006.BikeShare
     , module Haskbike.Database.Schema.V006.Migrations
     , evaluateBikeShareDatabase
     , printDatabaseMigration
     ) where

import           Database.Beam.Migrate                    ( CheckedDatabaseSettings, evaluateDatabase )
import           Database.Beam.Postgres                   ( Postgres )
import qualified Database.Beam.Postgres.Migrate           as Pg

import           Haskbike.Database.Schema.V006.BikeShare
import           Haskbike.Database.Schema.V006.Migrations

import           Text.Pretty.Simple.Extras                ( pPrintCompact )

-- * Misc/unused database schema inspection utilities.

-- Pretty-print the database DDL.
printDatabaseMigration :: IO ()
printDatabaseMigration = pPrintCompact (Pg.migrateScript migration)

-- Evaluate a 'MigrationSteps' to produce a 'CheckedDatabaseSettings'.
evaluateBikeShareDatabase :: CheckedDatabaseSettings Postgres BikeshareDb
evaluateBikeShareDatabase = evaluateDatabase migration
