{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Application environment and monad.
module Haskbike.AppEnv
     ( AppError (..)
     , AppM (..)
     , Env (..)
     , HasDB (..)
     , HasEnv
     , HasHaskbikeClient (..)
     , HasLogger (..)
     , Message
     , Severity (..)
     , WithEnv
     , ask
     , asks
     , createAppEnv
     , executeWithConnPool
     , mainEnv
     , mkDatabaseConnectionPool
     , mkDatabaseConnectionPoolFrom
     , mkDbConnectInfo
     , runAppM
     , runWithAppM
     , runWithAppMDebug
     , runWithAppMSuppressLog
     , simpleEnv
     , throwAppError
     , withConnPool
     , withManager
     , withPooledConn
     , withPostgres
     , withPostgresAppM
     , withPostgresTransaction
     , withPostgresTransactionAppM
     ) where

import           Colog

import           Control.Exception                      ( throw )
import           Control.Monad                          ( when )
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader                   ( MonadReader, ReaderT (..), ask, asks )

import           Data.Maybe                             ( fromMaybe, isNothing )
import           Data.Pool
import qualified Data.Text                              as T
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

import           UnliftIO                               ( MonadIO (..), MonadUnliftIO, withRunInIO )


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
                   , MonadReader (Env AppM)
                   , MonadFail
                   , MonadThrow
                   , MonadCatch
                   )

-- | Proper MonadUnliftIO instance with resource safety
instance MonadUnliftIO AppM where
  withRunInIO inner = AppM $ withRunInIO $ \run ->
    inner (run . unAppM)


-- | Logger capability typeclass
class (MonadReader env m, MonadIO m, HasLog env Message m) => HasLogger env m where
    getLogAction        :: m (LogAction m Message)
    getMinSeverity      :: m Severity
    getLogDatabase      :: m Bool

-- | Database capability typeclass
class (MonadReader env m, MonadIO m) => HasDB env m where
    getDBConnectionPool :: m (Pool Connection)
    getTz               :: m TimeZone

-- | Client capability typeclass
class (MonadReader env m, MonadIO m) => HasHaskbikeClient env m where
    getClientManager    :: m Manager
    getBaseUrl          :: m BaseUrl

-- | Combined environment capability
class (HasLogger env m, HasDB env m, HasHaskbikeClient env m) => HasEnv env m

-- Logger capability implementation
instance (Monad m, MonadReader (Env m) m, MonadIO m) => HasLogger (Env m) m where
    getLogAction        = asks envLogAction
    {-# INLINE getLogAction #-}

    getMinSeverity      = asks envMinSeverity
    {-# INLINE getMinSeverity #-}

    getLogDatabase      = asks envLogDatabase
    {-# INLINE getLogDatabase #-}

-- Database capability implementation
instance (Monad m, MonadReader (Env m) m, MonadIO m) => HasDB (Env m) m where
    getDBConnectionPool = asks envDBConnectionPool
    {-# INLINE getDBConnectionPool #-}

    getTz               = asks envTimeZone
    {-# INLINE getTz #-}

-- Client capability implementation
instance (Monad m, MonadReader (Env m) m, MonadIO m) => HasHaskbikeClient (Env m) m where
    getClientManager    = asks envClientManager
    {-# INLINE getClientManager #-}

    getBaseUrl          = asks envBaseUrl
    {-# INLINE getBaseUrl #-}

-- Combined environment implementation
instance (HasLogger (Env m) m, HasDB (Env m) m, HasHaskbikeClient (Env m) m) => HasEnv (Env m) m

-- Implement logging for the application environment.
instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }
  {-# INLINE setLogAction #-}

{- | Type alias for constraint for:

1. Monad @m@ have access to environment @env@.
2. Environment @env@ contains 'LogAction' that can log messages of type @msg@.
3. Function call stack.

If you use this constraint, function call stack will be propagated and
you will have access to code lines that log messages.
-}
type WithEnv env m = HasEnv env m

-- | Custom App error type for structured error handling
data AppError
  = DBError SqlError
  | ServerError ServerError
  | ConfigError T.Text
  | GenericError T.Text
  deriving (Show)

instance Exception AppError

-- | Standardized error handling by converting all errors to AppError
instance MonadError ServerError AppM where
  throwError = AppM . throwM . ServerError
  catchError action handler = AppM $ catch (unAppM action) (unAppM . handler)

-- | Helper to throw consistent app errors
throwAppError :: AppError -> AppM a
throwAppError = AppM . throwM

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
withPostgres :: (HasLogger env m, MonadCatch m, HasEnv env m, MonadIO m) => Pg b -> m b
withPostgres action = do
  logDatabase <- getLogDatabase
  let dbFunction = if logDatabase
        then runBeamPostgresDebug putStrLn
        else runBeamPostgres
  res <- try $ withPooledConn dbFunction action
  case res of
    Left (e :: SqlError) -> do
      logException e
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
    Left (e :: SqlError) -> do
      logException e
      throw e
    Right result -> pure result
{-# INLINE withPostgresTransaction #-}

-- | Specialized version of withPostgresTransaction for AppM that uses structured error handling
withPostgresTransactionAppM :: Pg a -> AppM a
withPostgresTransactionAppM action = do
  logDatabase <- getLogDatabase
  pool <- withConnPool
  let dbFunction = if logDatabase
        then runBeamPostgresDebug putStrLn
        else runBeamPostgres
  res <- try $ liftIO $ withResource pool $ \conn -> withTransaction conn (dbFunction conn action)
  case res of
    Left (e :: SqlError) -> do
      logException e
      throwAppError (DBError e)
    Right result -> pure result
{-# INLINE withPostgresTransactionAppM #-}

-- | Fetch client manager from the environment.
withManager :: (HasEnv env m, MonadIO m, MonadThrow m) => m Manager
withManager = getClientManager
{-# INLINE withManager #-}

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

  -- | Specialized version of withPostgres for AppM that uses structured error handling
withPostgresAppM :: Pg b -> AppM b
withPostgresAppM action = do
  logDatabase <- getLogDatabase
  let dbFunction = if logDatabase
        then runBeamPostgresDebug putStrLn
        else runBeamPostgres
  res <- try $ withPooledConn dbFunction action
  case res of
    Left (e :: SqlError) -> do
      logException e
      throwAppError (DBError e)
    Right result -> pure result
{-# INLINE withPostgresAppM #-}

-- | Helper function to run the application.
runAppM :: Env AppM -> AppM a -> IO a
runAppM env app = runReaderT (unAppM app) env

-- | Helper function to create an app environment with standard configuration
createAppEnv :: MonadIO m
             => Severity  -- ^ Log severity level
             -> Bool      -- ^ Log database operations
             -> Bool      -- ^ Use rich logging
             -> String    -- ^ Database name
             -> m (Env AppM)
createAppEnv sev logDatabase richLogging dbname = liftIO $ do
  connPool <- mkDbConnectInfo dbname >>= mkDatabaseConnectionPool
  currentTimeZone <- getCurrentTimeZone
  clientManager <- newManager tlsManagerSettings
  pure $ mainEnv sev logDatabase richLogging currentTimeZone connPool clientManager

-- | Helper function to run a computation in the AppM monad, returning an IO monad.
runWithAppM :: String -> AppM a -> IO a
runWithAppM dbname action = do
  env <- createAppEnv Info False True dbname
  runAppM env action

-- | This function is the same as runWithAppM, but overrides the log action to be a no-op.
runWithAppMSuppressLog :: String -> AppM a -> IO a
runWithAppMSuppressLog dbname action = do
  env <- createAppEnv Info False True dbname
  runAppM (env { envLogAction = mempty }) action

-- | Helper function to run a computation in the AppM monad with debug and database logging, returning an IO monad.
runWithAppMDebug :: String -> AppM a -> IO a
runWithAppMDebug dbname action = do
  env <- createAppEnv Debug True True dbname
  runAppM env action

mkDatabaseConnectionPool :: MonadIO m => ConnectInfo -> m (Pool Connection)
mkDatabaseConnectionPool = mkDatabaseConnectionPoolFrom acquireDbConnection
{-# INLINE mkDatabaseConnectionPool #-}

mkDatabaseConnectionPoolFrom :: MonadIO m => (t -> IO Connection) -> t -> m (Pool Connection)
mkDatabaseConnectionPoolFrom connectFn param =
  liftIO . newPool $ defaultPoolConfig (connectFn param) releaseDbConnection 5 numCapabilities
{-# INLINE mkDatabaseConnectionPoolFrom #-}

acquireDbConnection :: ConnectInfo -> IO Connection
acquireDbConnection conn = do
  -- putStrLn "Connecting to DB..."
  connect conn
{-# INLINE acquireDbConnection #-}

releaseDbConnection :: Connection -> IO ()
releaseDbConnection conn = do
  close conn
  -- putStrLn "Closing DB connection"
{-# INLINE releaseDbConnection #-}

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
