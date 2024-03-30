-- | Utility functions for database operations.

module Haskbike.Database.Utils
     ( connectDbName
     , connectProductionDb
     , connectTestDb
     , dbnameProduction
     , dbnameTest
     , dropTables
     , mkDbParams
     , referencesTable
     ) where

import           Control.Monad                ( void )
import           Control.Monad.Catch          ( MonadCatch, MonadThrow )

import           Data.Pool                    ( withResource )
import           Data.String                  ( IsString, fromString )
import qualified Data.Text                    as T

import           Database.Beam.Migrate
import           Database.PostgreSQL.Simple

import           Haskbike.AppEnv
import           Haskbike.Database.Connection

import           UnliftIO


-- | Construct query to drop a table using cascade.
dropCascade :: String -> Query
dropCascade tableName = fromString $ "DROP TABLE IF EXISTS " ++ tableName ++" CASCADE"

-- | Drop all tables in the named database.
dropTables :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m, MonadUnliftIO m)
           => m ()
dropTables = do
  pool <- withConnPool
  void . liftIO . withResource pool $ \conn -> do
  -- Drop all tables.
    execute_ conn $ dropCascade "station_lookup"
    execute_ conn $ dropCascade "queries"
    execute_ conn $ dropCascade "station_status"
    execute_ conn $ dropCascade "station_status_changes"
    execute_ conn $ dropCascade "station_information"
    execute_ conn $ dropCascade "system_information_count"
    execute_ conn $ dropCascade "system_information"
    execute_ conn "DROP TYPE IF EXISTS endpoint_queried CASCADE"
    execute_ conn $ dropCascade "beam_migration"
    execute_ conn $ dropCascade "beam_version"

referencesTable :: BeamMigrateSqlBackend be => T.Text -> [T.Text] -> Constraint be
referencesTable foreignTbl fields =
  Constraint $ referencesConstraintSyntax foreignTbl fields
  Nothing
  (Just referentialActionCascadeSyntax)
  Nothing

addConstraintIfNotExists :: IsString a => T.Text -> T.Text -> T.Text -> [T.Text] -> [T.Text] -> a
addConstraintIfNotExists tableName foreignTableName constraintName columns foreignColumns =
  fromString . T.unpack . T.intercalate "; " $
  [ "do $$"
  ,   "IF (OBJECT_ID('" <> qualifiedConstraint <> "', 'F') IS NOT NULL)"
  ,     "ALTER TABLE " <> qualifiedTable <> " DROP CONSTRAINT " <> constraintName
  ,   "END"
  ,   "ALTER TABLE " <> qualifiedTable <> " ADD CONSTRAINT " <> constraintName <> " FOREIGN KEY (" <> columnsList <> ") REFERENCES " <> qualifiedForeignTable <> " (" <> foreignColumnsList <> ") ON UPDATE CASCADE"
  , "$$"
  ]
  where
    qualifiedTable        = "public." <> tableName
    qualifiedForeignTable = "public." <> foreignTableName
    qualifiedConstraint   = tableName <> "." <> constraintName
    columnsList           = T.intercalate ", " columns
    foreignColumnsList    = T.intercalate ", " foreignColumns
