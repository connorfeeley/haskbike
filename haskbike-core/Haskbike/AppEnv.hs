{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Application environment and monad.
module Haskbike.AppEnv
     ( AppM (..)
     , Env (..)
     , HasEnv (..)
     , Message
     , WithEnv
     , ask
     , asks
     , executeWithConnPool
     , mainEnv
     , mkDatabaseConnectionPool
     , mkDbConnectInfo
     , runAppM
     , runWithAppM
     , runWithAppMDebug
     , runWithAppMSuppressLog
     , simpleEnv
     , withConnPool
     , withManager
     , withPooledConn
     , withPostgres
     , withPostgresTransaction
     ) where

import           Colog                                  ( HasLog (..), LogAction (..), Message, Msg (msgSeverity),
                                                          Severity (..), filterBySeverity, logException,
                                                          richMessageAction, simpleMessageAction )

import           Control.Exception                      ( throw )
import           Control.Monad                          ( when )
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader                   ( MonadReader, ReaderT (..), ask, asks )

import           Data.Maybe                             ( fromMaybe, isNothing )
import           Data.Pool
import           Data.Time                              ( TimeZone, getCurrentTimeZone )
import           Data.Word                              ( Word16 )

import           Database.Beam.Postgres                 ( ConnectInfo (..), Connection, Pg, SqlError, close, connect,
                                                          runBeamPostgres, runBeamPostgresDebug )
import           Database.PostgreSQL.Simple             ( defaultConnectInfo )
import           Database.PostgreSQL.Simple.Transaction ( withTransaction )

import           GHC.Conc                               ( numCapabilities )

import           Haskbike.API.BikeShare

import           Network.HTTP.Client                    ( Manager, newManager )
import           Network.HTTP.Client.TLS                ( tlsManagerSettings )

import           Prelude                                hiding ( log )

import           Servant                                ( ServerError )
import           Servant.Client

import           System.Environment                     ( lookupEnv )

import           Text.Read                              ( readMaybe )

import           UnliftIO                               ( MonadIO (..), MonadUnliftIO )


-- Application environment
data Env m where
  Env :: { envLogAction        :: !(LogAction m Message)
         , envLogDatabase      :: !Bool
         , envMinSeverity      :: !Severity
         , envTimeZone         :: !TimeZone
         , envDBConnectionPool :: !(Pool Connection)
         , envClientManager    :: !Manager
         , envBaseUrl          :: !BaseUrl
         } -> Env m

-- Application type
newtype AppM a where
  AppM :: { unAppM :: ReaderT (Env AppM) IO a
          } -> AppM a
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadUnliftIO
                   , MonadReader (Env AppM)
                   , MonadFail
                   , MonadThrow
                   , MonadCatch )


class (HasLog env Message m, MonadReader env m, MonadIO m) => HasEnv env m where
    getLogDatabase      :: m Bool
    getMinSeverity      :: m Severity
    getTz               :: m TimeZone
    getDBConnectionPool :: m (Pool Connection)
    getClientManager    :: m Manager
    getBaseUrl          :: m BaseUrl

instance (Monad m, MonadReader (Env m) m, MonadIO m) => HasEnv (Env m) m where
    getLogDatabase      = asks envLogDatabase
    getMinSeverity      = asks envMinSeverity
    getTz               = asks envTimeZone
    getDBConnectionPool = asks envDBConnectionPool
    getClientManager    = asks envClientManager
    getBaseUrl          = asks envBaseUrl

{- | Type alias for constraint for:

1. Monad @m@ have access to environment @env@.
2. Environment @env@ contains 'LogAction' that can log messages of type @msg@.
3. Function call stack.

If you use this constraint, function call stack will be propagated and
you will have access to code lines that log messages.
-}
type WithEnv env m = HasEnv env m

instance MonadError ServerError AppM where
  throwError = AppM . throwM
  catchError action handler = AppM $ catch (unAppM action) (unAppM . handler)

-- | Fetch database connection pool from environment monad.
withConnPool :: (HasEnv env m, MonadIO m, MonadThrow m) => m (Pool Connection)
withConnPool = getDBConnectionPool

-- | Helper function to execute a given action with a database connection from a given resource pool.
executeWithConnPool :: MonadIO m => (Connection -> p -> IO a) -> p -> Pool Connection -> m a
executeWithConnPool dbFunction action pool = liftIO (withResource pool (`dbFunction` action))

-- | Acquire a database connection from the resource pool in environment monad, and execute a given action.
withPooledConn :: (HasEnv env m, MonadIO m, MonadThrow m) => (Connection -> p -> IO b) -> p -> m b
withPooledConn dbFunction action = withConnPool >>= executeWithConnPool dbFunction action

-- | Run a Beam operation using database connection from the environment.
withPostgres :: (MonadCatch m, HasEnv env m, MonadIO m) => Pg b -> m b
withPostgres action = do
  logDatabase <- getLogDatabase
  let dbFunction = if logDatabase
        then runBeamPostgresDebug putStrLn
        else runBeamPostgres
  res <- try $ withPooledConn dbFunction action
  case res of
    Left (e :: SqlError) ->
      logException e >>
      throw e
    Right result -> pure result
{-# INLINE withPostgres #-}

-- | Run a Beam operation in a transaction using database connection from the environment.
withPostgresTransaction :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m) => Pg a -> m a
withPostgresTransaction action = do
  logDatabase <- getLogDatabase
  pool <- withConnPool
  let dbFunction = if logDatabase
        then runBeamPostgresDebug putStrLn
        else runBeamPostgres
  res <- try $ liftIO $ withResource pool $ \conn -> withTransaction conn (dbFunction conn action)
  case res of
    Left (e :: SqlError) ->
      logException e >>
      throw e
    Right result -> pure result
{-# INLINE withPostgresTransaction #-}

-- | Fetch client manager from the environment.
withManager :: (HasEnv env m, MonadIO m, MonadThrow m) => m Manager
withManager = getClientManager >>= liftIO . pure

-- Implement logging for the application environment.
instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }
  {-# INLINE setLogAction #-}

-- | Simple environment for the main application.
simpleEnv :: (HasEnv env m, MonadIO m, MonadThrow m) => TimeZone -> Pool Connection -> Manager -> Env m
simpleEnv timeZone connPool manager =
  Env { envLogAction        = mainLogAction Info False
      , envLogDatabase      = False
      , envMinSeverity      = Info
      , envTimeZone         = timeZone
      , envDBConnectionPool = connPool
      , envClientManager    = manager
      , envBaseUrl          = bikeshareBaseUrl
      }

-- | Environment for the main application.
mainEnv :: (MonadIO m, MonadThrow m) => Severity -> Bool -> Bool -> TimeZone -> Pool Connection -> Manager -> Env m
mainEnv sev logDatabase logRichOutput timeZone connPool manager =
  Env { envLogAction        = mainLogAction sev logRichOutput
      , envLogDatabase      = logDatabase
      , envMinSeverity      = Info
      , envTimeZone         = timeZone
      , envDBConnectionPool = connPool
      , envClientManager    = manager
      , envBaseUrl          = bikeshareBaseUrl
      }

-- | Log action for the main application.
mainLogAction :: (MonadIO m)
              => Severity            -- ^ Severity level (verbosity).
              -> Bool                -- ^ If rich log action should be used.
              -> LogAction m Message -- ^ Action used to log.
mainLogAction severity enableRich = filterBySeverity severity msgSeverity (if enableRich then richMessageAction else simpleMessageAction)

  -- | Helper function to run the application.
runAppM :: Env AppM -> AppM a -> IO a
runAppM env app = runReaderT (unAppM app) env

-- | Helper function to run a computation in the AppM monad, returning an IO monad.
runWithAppM :: String -> AppM a -> IO a
runWithAppM dbname action = do
  connPool <- mkDbConnectInfo dbname >>= mkDatabaseConnectionPool
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = mainEnv Info False True currentTimeZone connPool clientManager
  runAppM env action

-- | This function is the same as runWithAppM, but overrides the log action to be a no-op.
runWithAppMSuppressLog :: String -> AppM a -> IO a
runWithAppMSuppressLog dbname action = do
  connPool <- mkDbConnectInfo dbname >>= mkDatabaseConnectionPool
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = (mainEnv Info False True currentTimeZone connPool clientManager :: Env AppM) { envLogAction = mempty }
  runAppM env action


-- | Helper function to run a computation in the AppM monad with debug and database logging, returning an IO monad.
runWithAppMDebug :: String -> AppM a -> IO a
runWithAppMDebug dbname action = do
  connPool <- mkDbConnectInfo dbname >>= mkDatabaseConnectionPool
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = mainEnv Debug True True currentTimeZone connPool clientManager
  runAppM env action

mkDatabaseConnectionPool :: MonadIO m => ConnectInfo -> m (Pool Connection)
mkDatabaseConnectionPool connInfo = liftIO $ newPool (defaultPoolConfig (connect connInfo) close 30 numCapabilities)
{-# INLINE mkDatabaseConnectionPool #-}

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
