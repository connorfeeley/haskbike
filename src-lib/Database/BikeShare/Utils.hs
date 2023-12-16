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

import           Data.String                   ( fromString )

import           Database.Beam.Postgres
import           Database.BikeShare.Connection
import           Database.PostgreSQL.Simple


-- | Construct query to drop a table using cascade.
dropCascade :: String -> Query
dropCascade tableName = fromString $ "DROP TABLE IF EXISTS " ++ tableName ++" CASCADE"

-- | Drop all tables in the named database.
dropTables :: Connection -> IO Connection
dropTables conn = do
  -- Drop all tables.
  _ <- execute_ conn $ dropCascade "queries"
  _ <- execute_ conn $ dropCascade "station_status"
  _ <- execute_ conn $ dropCascade "station_status_delta"
  _ <- execute_ conn $ dropCascade "station_information"
  _ <- execute_ conn $ dropCascade "system_information_count"
  _ <- execute_ conn $ dropCascade "system_information"
  _ <- execute_ conn "DROP TYPE IF EXISTS endpoint_queried CASCADE"
  _ <- execute_ conn $ dropCascade "beam_migration"
  _ <- execute_ conn $ dropCascade "beam_version"

  pure conn
