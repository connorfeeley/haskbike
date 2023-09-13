-- | Utility functions for database operations.

module Database.Utils where

import           Data.String (fromString)
import           Text.Pretty.Simple

import           Database.Beam
import           Database.Beam.Postgres
import           Database.Migrations (migrateDB)
import           Database.PostgreSQL.Simple


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


-- | pPrint with compact output.
pPrintCompact :: (MonadIO m, Show a) => a -> m ()
pPrintCompact = pPrintOpt CheckColorTty pPrintCompactOpt
  where
    pPrintCompactOpt = defaultOutputOptionsDarkBg { outputOptionsCompact = True }
