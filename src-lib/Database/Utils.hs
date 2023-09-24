{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions for database operations.

module Database.Utils
     ( connectDbName
     , connectProductionDb
     , connectTestDb
     , dbnameProduction
     , dbnameTest
     , pPrintCompact
     , setupDatabaseName
     , setupProductionDatabase
     ) where

import           Data.String                ( fromString )

import           Database.Beam
import           Database.Beam.Postgres
import           Database.Migrations        ( migrateDB )
import           Database.PostgreSQL.Simple

import           System.Environment         ( lookupEnv )

import           Text.Pretty.Simple


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

-- | Establish a connection to the named database, using values from the HASKBIKE_{PGDBHOST,USERNAME,PASSWORD} environment variables.
connectDbName :: String -> IO Connection
connectDbName name = do
  envPgDbHostParam <- mkParam "host=localhost" "host=" =<< lookupEnv "HASKBIKE_PGDBHOST"
  envPgDbPortParam <- mkParam "port=5432" "port=" =<< lookupEnv "HASKBIKE_PGDBPORT"
  envUsername <- mkParam "" "user="  =<< lookupEnv "HASKBIKE_USERNAME"
  envPassword <- mkParam "" "password=" =<< lookupEnv "HASKBIKE_PASSWORD"

  putStrLn $ "Connecting with: " ++
    envPgDbHostParam ++ " " ++
    envPgDbPortParam ++ " " ++
    envUsername ++ " " ++
    "(password) " ++
    " dbname=" ++ name ++
    " connect_timeout=10"

  connectPostgreSQL $ fromString $
    envPgDbHostParam ++ " " ++
    envPgDbPortParam ++ " " ++
    envUsername ++ " " ++
    envPassword ++ " " ++
    " dbname=" ++ name ++
    " connect_timeout=10"
  where
    -- takes a default value, a prefix, and an optional value
    mkParam :: String -> String -> Maybe String -> IO String
    mkParam defaultVal prefix = maybe (pure defaultVal) (pure . (prefix ++))


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
