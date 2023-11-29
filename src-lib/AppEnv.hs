{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Application environment and monad.
module AppEnv
     ( AppM (..)
     , Env (..)
     , Message
     , WithAppMEnv
     , ask
     , asks
     , mainEnv
     , runAppM
     , runWithAppM
     , runWithAppMDebug
     , runWithAppMSuppressLog
     , simpleEnv
     , withConn
     , withManager
     , withPostgres
     ) where

import           API.BikeShare

import           Colog                         ( HasLog (..), LogAction (..), Message, Msg (msgSeverity), Severity (..),
                                                 filterBySeverity, logException, richMessageAction,
                                                 simpleMessageAction )

import           Control.Exception             ( throw )
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader          ( MonadReader, ReaderT (..), ask, asks )

import           Data.Time                     ( TimeZone, getCurrentTimeZone )

import           Database.Beam.Postgres        ( Connection, Pg, SqlError, connect, runBeamPostgres,
                                                 runBeamPostgresDebug )
import           Database.BikeShare.Connection ( mkDbConnectInfo )

import           GHC.Stack                     ( HasCallStack )

import           Network.HTTP.Client           ( Manager, newManager )
import           Network.HTTP.Client.TLS       ( tlsManagerSettings )

import           Prelude                       hiding ( log )

import           Servant                       ( ServerError )
import           Servant.Client

import           UnliftIO                      ( MonadUnliftIO )


-- Application environment
data Env m where
  Env :: { envLogAction     :: !(LogAction m Message)
         , envLogDatabase   :: !Bool
         , envMinSeverity   :: !Severity
         , envTimeZone      :: !TimeZone
         , envDBConnection  :: !Connection
         , envClientManager :: !Manager
         , envBaseUrl       :: !BaseUrl
         } -> Env m

-- Application type
newtype AppM a = AppM
  { unAppM :: ReaderT (Env AppM) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (Env AppM), MonadFail, MonadThrow, MonadCatch)


{- | Type alias for constraint for:

1. Monad @m@ have access to environment @env@.
2. Environment @env@ contains 'LogAction' that can log messages of type @msg@.
3. Function call stack.

If you use this constraint, function call stack will be propagated and
you will have access to code lines that log messages.
-}
type WithAppMEnv env msg m = (MonadReader env m, HasLog env msg m, HasCallStack, MonadIO m, MonadUnliftIO m, MonadFail m, MonadThrow m, MonadCatch m)

instance MonadError ServerError AppM where
  throwError = AppM . throwM
  catchError action handler = AppM $ catch (unAppM action) (unAppM . handler)

-- | Fetch database connection from environment monad.
withConn :: (WithAppMEnv (Env env) Message m) => m Connection
withConn = asks envDBConnection >>= liftIO . pure

-- | Run a Beam operation using database connection from the environment.
withPostgres :: (WithAppMEnv (Env env) Message m) => Pg a -> m a
withPostgres action = do
  logDatabase <- asks envLogDatabase
  conn <- withConn
  let dbFunction = if logDatabase
        then runBeamPostgresDebug putStrLn
        else runBeamPostgres
  res <- try $ liftIO (dbFunction conn action)
  case res of
    Left (e :: SqlError) ->
      logException e >>
      throw e
    Right result -> pure result

-- | Fetch client manager from the environment.
withManager :: (WithAppMEnv (Env env) Message m) => m Manager
withManager = asks envClientManager >>= liftIO . pure

-- Implement logging for the application environment.
instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }
  {-# INLINE setLogAction #-}

-- | Simple environment for the main application.
simpleEnv :: TimeZone -> Connection -> Manager -> Env AppM
simpleEnv timeZone conn manager = Env { envLogAction     = mainLogAction Info False
                                      , envLogDatabase   = False
                                      , envMinSeverity   = Info
                                      , envTimeZone      = timeZone
                                      , envDBConnection  = conn
                                      , envClientManager = manager
                                      , envBaseUrl       = bikeshareBaseUrl
                                      }

-- | Environment for the main application.
mainEnv :: Severity -> Bool -> Bool -> TimeZone -> Connection -> Manager -> Env AppM
mainEnv sev logDatabase logRichOutput timeZone conn manager =
  Env { envLogAction     = mainLogAction sev logRichOutput
      , envLogDatabase   = logDatabase
      , envMinSeverity   = Info
      , envTimeZone      = timeZone
      , envDBConnection  = conn
      , envClientManager = manager
      , envBaseUrl       = bikeshareBaseUrl
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
  conn <- mkDbConnectInfo dbname >>= connect
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = mainEnv Info False True currentTimeZone conn clientManager
  runAppM env action

-- | This function is the same as runWithAppM, but overrides the log action to be a no-op.
runWithAppMSuppressLog :: String -> AppM a -> IO a
runWithAppMSuppressLog dbname action = do
  conn <- mkDbConnectInfo dbname >>= connect
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = (mainEnv Info False True currentTimeZone conn clientManager) { envLogAction = mempty }
  runAppM env action


-- | Helper function to run a computation in the AppM monad with debug and database logging, returning an IO monad.
runWithAppMDebug :: String -> AppM a -> IO a
runWithAppMDebug dbname action = do
  conn <- mkDbConnectInfo dbname >>= connect
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = mainEnv Debug True True currentTimeZone conn clientManager
  runAppM env action
