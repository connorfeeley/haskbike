-- |

module Haskbike.Database.Connection
     ( connectDbName
     , connectProductionDb
     , connectTestDb
     , dbnameProduction
     , dbnameTest
     , mkDbParams
     ) where

import           Data.String            ( IsString (..) )

import           Database.Beam.Postgres

import           Haskbike.AppEnv

import           System.Environment     ( lookupEnv )


-- | The name of the production database.
dbnameProduction :: String
dbnameProduction = "haskbike"

-- | The name of the database to use for tests.
dbnameTest :: String
dbnameTest = "haskbike-test"

-- | Establish a connection to the production database.
connectProductionDb :: IO Connection
connectProductionDb = mkDbConnectInfo dbnameProduction >>= connect

-- | Establish a connection to the testing database.
connectTestDb :: IO Connection
connectTestDb = mkDbConnectInfo dbnameTest >>= connect

-- | Establish a connection to the named database, using values from the HASKBIKE_{PGDBHOST,USERNAME,PASSWORD} environment variables.
connectDbName :: String -> String -> String -> String -> String -> IO Connection
connectDbName name host port username password = do
  -- Connect using a libpq connection string.
  -- https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING
  connectPostgreSQL $ fromString $
    host ++ " " ++
    port ++ " " ++
    username ++ " " ++
    password ++ " " ++
    " dbname=" ++ name ++
    " connect_timeout=10"


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

