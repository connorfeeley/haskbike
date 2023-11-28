-- | Utility functions for database operations.

module Database.BikeShare.Utils
     ( connectDbName
     , connectProductionDb
     , connectTestDb
     , dbnameProduction
     , dbnameTest
     , dropTables
     , migrateDatabase
     , mkDbConnectInfo
     , mkDbParams
     , runBeamPostgres'
     , runBeamPostgresDebug'
     , setupDatabaseName
     , setupProductionDatabase
     , uncurry5
     ) where



import           Control.Monad                 ( void )
import           Control.Monad.Cont            ( when )

import           Data.Maybe                    ( fromMaybe, isNothing )
import           Data.String                   ( fromString )
import           Data.Word                     ( Word16 )

import           Database.Beam.Postgres
import           Database.BikeShare.Connection
import           Database.BikeShare.Migrations ( migrateDB )
import           Database.PostgreSQL.Simple

import           Formatting

import           System.Environment            ( lookupEnv )

import           Text.Read                     ( readMaybe )


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

-- | Utility function to uncurry a 5-argument function
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e

-- | Setup the production database.
setupProductionDatabase :: IO Connection
setupProductionDatabase = setupDatabaseName dbnameProduction

-- | Setup the named database.
setupDatabaseName :: String -> IO Connection
setupDatabaseName dbname = do
  -- Connect to named database, drop all tables, and execute migrations.
  mkDbConnectInfo dbname >>= connect >>= dropTables >>= migrateDatabase

-- | Drop all tables in the named database.
dropTables :: Connection -> IO Connection
dropTables conn = do
  -- Drop all tables.
  _ <- execute_ conn $ dropCascade "station_status"
  _ <- execute_ conn $ dropCascade "station_information"
  _ <- execute_ conn $ dropCascade "system_information_count"
  _ <- execute_ conn $ dropCascade "system_information"
  _ <- execute_ conn $ dropCascade "beam_migration"
  _ <- execute_ conn $ dropCascade "beam_version"

  pure conn

-- | Run database migrations.
migrateDatabase :: Connection -> IO Connection
migrateDatabase conn = do
  -- Initialize the database.
  void $ migrateDB conn
  pure conn
