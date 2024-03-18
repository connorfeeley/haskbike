-- |

module Database.BikeShare.Connection
     ( connectDbName
     , connectProductionDb
     , connectTestDb
     , dbnameProduction
     , dbnameTest
     , mkDbConnectInfo
     , mkDbParams
     ) where

import           Control.Monad          ( when )

import           Data.Maybe             ( fromMaybe, isNothing )
import           Data.String            ( IsString (..) )
import           Data.Word              ( Word16 )

import           Database.Beam.Postgres

import           System.Environment     ( lookupEnv )

import           Text.Read              ( readMaybe )

import           UnliftIO               ( MonadIO, liftIO )


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

-- | Construct a 'ConnectInfo' using values from the HASKBIKE_{PGDBHOST,USERNAME,PASSWORD} environment variables.
mkDbConnectInfo :: MonadIO m => String -> m ConnectInfo
mkDbConnectInfo dbName = liftIO $ do
  envPgDbHostParam <- lookupEnv "HASKBIKE_PGDBHOST"
  envPgDbPortParam <- lookupEnv "HASKBIKE_PGDBPORT"
  envUsernameParam <- lookupEnv "HASKBIKE_USERNAME"
  envPasswordParam <- lookupEnv "HASKBIKE_PASSWORD"

  -- Log when using defaults.
  when (isNothing envPgDbHostParam) $ putStrLn "No HASKBIKE_PGDBHOST value found, using default"
  when (isNothing envPgDbPortParam) $ putStrLn "No HASKBIKE_PGDBPORT value found, using default"
  when (isNothing envUsernameParam) $ putStrLn "No HASKBIKE_USERNAME value found, using default"
  when (isNothing envPasswordParam) $ putStrLn "No HASKBIKE_PASSWORD value found, using default"

  pure $ ConnectInfo (fromMaybe (connectHost     defaultConnectInfo) envPgDbHostParam)
                     (fromMaybe (connectPort     defaultConnectInfo) ((readMaybe :: String -> Maybe Word16) =<< envPgDbPortParam))
                     (fromMaybe (connectUser     defaultConnectInfo) envUsernameParam)
                     (fromMaybe (connectPassword defaultConnectInfo) envPasswordParam)
                     dbName
