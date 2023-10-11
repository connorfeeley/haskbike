-- | Utility functions for database operations.

module Database.BikeShare.Utils
     ( connectDbName
     , connectProductionDb
     , connectTestDb
     , dbnameProduction
     , dbnameTest
     , dropTables
     , migrateDatabase
     , mkDbParams
     , runBeamPostgres'
     , runBeamPostgresDebug'
     , setupDatabaseName
     , setupProductionDatabase
     , uncurry5
     ) where

import           Control.Monad                 ( void )

import           Data.String                   ( fromString )

import           Database.Beam.Postgres
import           Database.BikeShare.Migrations ( migrateDB )
import           Database.PostgreSQL.Simple

import           Formatting

import           System.Environment            ( lookupEnv )


debug :: Bool
debug = False


-- | Enable SQL debug output if DEBUG flag is set.
runBeamPostgres' :: Connection  -- ^ Connection to the database.
                 -> Pg a        -- ^ @MonadBeam@ in which we can run Postgres commands.
                 -> IO a
runBeamPostgres' =
  if debug
  then runBeamPostgresDebug'
  else runBeamPostgres


-- | @runBeamPostgresDebug@ prefilled with @pPrintCompact@.
runBeamPostgresDebug' :: Connection     -- ^ Connection to the database.
                      -> Pg a           -- ^ @MonadBeam@ in which we can run Postgres commands.
                      -> IO a
runBeamPostgresDebug' = runBeamPostgresDebug pPrintCompact

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
connectProductionDb = mkDbParams dbnameProduction >>= uncurry5 connectDbName

-- | Establish a connection to the testing database.
connectTestDb :: IO Connection
connectTestDb = mkDbParams dbnameTest >>= uncurry5 connectDbName

-- | Establish a connection to the named database, using values from the HASKBIKE_{PGDBHOST,USERNAME,PASSWORD} environment variables.
connectDbName :: String -> String -> String -> String -> String -> IO Connection
connectDbName name host port username password= do
  connectPostgreSQL $ fromString $
    host ++ " " ++
    port ++ " " ++
    username ++ " " ++
    password ++ " " ++
    " dbname=" ++ name ++
    " connect_timeout=10"


-- | Utility function to uncurry a 5-argument function
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e


mkDbParams :: String -> IO (String, String, String, String, String)
mkDbParams name = do
  envPgDbHostParam <- mkParam "host=localhost" "host="     =<< lookupEnv "HASKBIKE_PGDBHOST"
  envPgDbPortParam <- mkParam "port=5432"      "port="     =<< lookupEnv "HASKBIKE_PGDBPORT"
  envUsername      <- mkParam ""               "user="     =<< lookupEnv "HASKBIKE_USERNAME"
  envPassword      <- mkParam ""               "password=" =<< lookupEnv "HASKBIKE_PASSWORD"

  pure (name, envPgDbHostParam, envPgDbPortParam, envUsername, envPassword)
  where
    -- takes a default value, a prefix, and an optional value
    mkParam :: String -> String -> Maybe String -> IO String
    mkParam defaultVal prefix = maybe (pure defaultVal) (pure . (prefix ++))


-- | Setup the production database.
setupProductionDatabase :: IO Connection
setupProductionDatabase = setupDatabaseName dbnameProduction

-- | Setup the named database.
setupDatabaseName :: String -> IO Connection
setupDatabaseName dbname = do
  -- Connect to named database, drop all tables, and execute migrations.
  mkDbParams dbname >>= uncurry5 connectDbName >>= dropTables >>= migrateDatabase

-- | Drop all tables in the named database.
dropTables :: Connection -> IO Connection
dropTables conn = do
  -- Drop all tables.
  _ <- execute_ conn $ dropCascade "station_status"
  _ <- execute_ conn $ dropCascade "station_information"
  _ <- execute_ conn $ dropCascade "beam_migration"
  _ <- execute_ conn $ dropCascade "beam_version"

  pure conn

-- | Run database migrations.
migrateDatabase :: Connection -> IO Connection
migrateDatabase conn = do
  -- Initialize the database.
  void $ migrateDB conn
  pure conn
