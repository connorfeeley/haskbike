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
     ) where

import           AppEnv

import           Control.Monad                 ( void )

import           Data.Pool                     ( withResource )
import           Data.String                   ( fromString )

import           Database.BikeShare.Connection
import           Database.PostgreSQL.Simple

import           UnliftIO


-- | Construct query to drop a table using cascade.
dropCascade :: String -> Query
dropCascade tableName = fromString $ "DROP TABLE IF EXISTS " ++ tableName ++" CASCADE"

-- | Drop all tables in the named database.
dropTables :: AppM ()
dropTables = do
  pool <- withConnPool
  void . liftIO . withResource pool $ \conn -> do
  -- Drop all tables.
    execute_ conn $ dropCascade "queries"
    execute_ conn $ dropCascade "station_status"
    execute_ conn $ dropCascade "station_status_delta"
    execute_ conn $ dropCascade "station_information"
    execute_ conn $ dropCascade "system_information_count"
    execute_ conn $ dropCascade "system_information"
    execute_ conn "DROP TYPE IF EXISTS endpoint_queried CASCADE"
    execute_ conn $ dropCascade "beam_migration"
    execute_ conn $ dropCascade "beam_version"
