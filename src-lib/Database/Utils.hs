-- | Utility functions for database operations.

module Database.Utils
  ( connectProductionDb
  , connectTestDb
  , setupProductionDatabase
  , setupDatabaseName
  , dbnameProduction
  , dbnameTest
  , pPrintCompact
  ) where

import           Data.String                (fromString)
import           Text.Pretty.Simple

import           Database.Beam
import           Database.Beam.Postgres
import           Database.Migrations        (migrateDB)
import           Database.PostgreSQL.Simple


-- | Construct query to drop a table using cascade.
dropCascade :: String -> Query
dropCascade tableName = fromString $ "DROP TABLE IF EXISTS " ++ tableName ++" CASCADE"

-- | The name of the production database.
dbnameProduction :: String
dbnameProduction = "haskbike"

-- | The name of the database to use for tests.
dbnameTest :: String
dbnameTest = "haskbike-test"

-- | Establish a connection to the production database.
connectProductionDb :: IO Connection
connectProductionDb =
  connectDbName dbnameProduction

-- | Establish a connection to the testing database.
connectTestDb :: IO Connection
connectTestDb =
  connectDbName dbnameProduction

-- | Establish a connection to the named database.
connectDbName :: String -> IO Connection
connectDbName name =
  connectPostgreSQL $ fromString $ "host=localhost port=5432 dbname=" ++ name ++ " connect_timeout=10"

-- | Setup the production database.
setupProductionDatabase :: IO Connection
setupProductionDatabase = setupDatabaseName dbnameProduction

-- | Setup the named database.
setupDatabaseName :: String -> IO Connection
setupDatabaseName name = do
  -- Connect to the database.
  conn <- connectDbName name

  -- Drop all tables.
  _ <- execute_ conn $ dropCascade "station_status"
  _ <- execute_ conn $ dropCascade "station_information"
  _ <- execute_ conn $ dropCascade "beam_migration"
  _ <- execute_ conn $ dropCascade "beam_version"

  -- Initialize the database.
  _ <- migrateDB conn

  pPrintString "Database reinitialization complete."

  pure conn


-- | pPrint with compact output.
pPrintCompact :: (MonadIO m, Show a) => a -> m ()
pPrintCompact = pPrintOpt CheckColorTty pPrintCompactOpt
  where
    pPrintCompactOpt = defaultOutputOptionsDarkBg { outputOptionsCompact = True }
