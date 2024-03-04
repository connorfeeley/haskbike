-- | Database schema for BikeShare.

module Database.BikeShare
     ( module Database.BikeShare.Schema.V002.BikeShare
     , module Database.BikeShare.Schema.V002.Migrations
     , evaluateBikeShareDatabase
     , printDatabaseMigration
     ) where

import           Database.Beam.Migrate                     ( CheckedDatabaseSettings, evaluateDatabase )
import           Database.Beam.Postgres                    ( Postgres )
import qualified Database.Beam.Postgres.Migrate            as Pg
import           Database.BikeShare.Schema.V002.BikeShare
import           Database.BikeShare.Schema.V002.Migrations

import           Text.Pretty.Simple                        ( pPrint )

-- * Misc/unused database schema inspection utilities.

-- Pretty-print the database DDL.
printDatabaseMigration :: IO ()
printDatabaseMigration = pPrint (Pg.migrateScript migration)

-- Evaluate a 'MigrationSteps' to produce a 'CheckedDatabaseSettings'.
evaluateBikeShareDatabase :: CheckedDatabaseSettings Postgres BikeshareDb
evaluateBikeShareDatabase = evaluateDatabase migration
