{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}

-- | Application environment and monad.
module AppEnv
     ( App
     , Env (..)
     , Message
     , WithAppEnv
     , liftIO
     , mainEnv
     , runApp
     , runQueryWithManager
     , runWithApp
     , runWithAppDebug
     , simpleEnv
     , withConn
     , withManager
     , withPostgres
     ) where

import           API.Client
import           API.Types

import           Colog                    ( HasLog (..), LogAction (..), Message, Msg (msgSeverity), Severity (..),
                                            filterBySeverity, logException, richMessageAction, simpleMessageAction )

import           Control.Monad.Catch
import           Control.Monad.IO.Class   ( MonadIO )
import           Control.Monad.Reader     ( MonadReader, ReaderT (..), ask, asks )

import           Data.Aeson               ( Object )
import           Data.Time                ( TimeZone, getCurrentTimeZone )

import           Database.Beam.Postgres   ( Connection, Pg, runBeamPostgres, runBeamPostgresDebug )
import           Database.BikeShare.Utils ( connectDbName, mkDbParams, uncurry5 )

import           GHC.Stack                ( HasCallStack )

import           Network.HTTP.Client      ( Manager, newManager )
import           Network.HTTP.Client.TLS  ( tlsManagerSettings )

import           Prelude                  hiding ( log )

import           Servant.Client

import           UnliftIO                 ( MonadUnliftIO, liftIO )

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

{- | Type alias for constraint for:

1. Monad @m@ have access to environment @env@.
2. Environment @env@ contains 'LogAction' that can log messages of type @msg@.
3. Function call stack.

If you use this constraint, function call stack will be propagated and
you will have access to code lines that log messages.
-}
type WithAppEnv env msg m = (MonadReader env m, HasLog env msg m, HasCallStack, MonadIO m, MonadUnliftIO m, MonadFail m, MonadThrow m, MonadCatch m)

-- | Fetch database connection from environment monad.
withConn :: (WithAppEnv (Env env) Message m) => m Connection
withConn = asks envDBConnection >>= liftIO . pure

-- | Run a Beam operation using database connection from the environment.
withPostgres :: (WithAppEnv (Env env) Message m) => Pg a -> m a
withPostgres action = do
  conn <- withConn
  logDatabase <- asks envLogDatabase
  liftIO $
    if logDatabase then runBeamPostgresDebug putStrLn conn action
    else runBeamPostgres conn action

-- | Fetch client manager from the environment.
withManager :: (WithAppEnv (Env env) Message m) => m Manager
withManager = asks envClientManager >>= liftIO . pure

-- | Run API query using client manager from environment monad.
runQueryWithManager :: WithAppEnv (Env env) Message m => ClientM a -> m (Either ClientError a)
runQueryWithManager query = do
  clientManager <- withManager
  liftIO $ runQuery clientManager query

-- Implement logging for the application environment.
instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }
  {-# INLINE setLogAction #-}

-- Application type
newtype App a = App
  { unApp :: ReaderT (Env App) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (Env App), MonadFail, MonadThrow, MonadCatch)

-- | Simple environment for the main application.
simpleEnv :: TimeZone -> Connection -> Manager -> Env App
simpleEnv timeZone conn manager = Env { envLogAction     = mainLogAction Info False
                                      , envLogDatabase   = False
                                      , envMinSeverity   = Info
                                      , envTimeZone      = timeZone
                                      , envDBConnection  = conn
                                      , envClientManager = manager
                                      , envBaseUrl       = bikeshareBaseUrl
                                      }

-- | Environment for the main application.
mainEnv :: Severity -> Bool -> Bool -> TimeZone -> Connection -> Manager -> Env App
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
runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env

-- | Helper function to run a computation in the App monad, returning an IO monad.
runWithApp :: String -> App a -> IO a
runWithApp dbname app = do
  conn <- mkDbParams dbname >>= uncurry5 connectDbName
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  runApp (mainEnv Info False True currentTimeZone conn clientManager) app

-- | Helper function to run a computation in the App monad with debug and database logging, returning an IO monad.
runWithAppDebug :: String -> App a -> IO a
runWithAppDebug dbname app = do
  conn <- mkDbParams dbname >>= uncurry5 connectDbName
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  runApp (mainEnv Debug True True currentTimeZone conn clientManager) app
