-- | Utility functions for database operations.

module Database.BikeShare.Utils
     ( connectDbName
     , connectProductionDb
     , connectTestDb
     , dbnameProduction
     , dbnameTest
     , dropTables
     , mkDbConnectInfo
     , mkDbParams
     , referencesTable
     ) where

import           AppEnv

import           Control.Monad                 ( void )
import           Control.Monad.Catch           ( MonadCatch, MonadThrow )

import           Data.Pool                     ( withResource )
import           Data.String                   ( fromString )
import qualified Data.Text                     as T

import           Database.Beam.Migrate
import           Database.BikeShare.Connection
import           Database.PostgreSQL.Simple

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
