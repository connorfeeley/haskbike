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
     , mainLogAction
     , runApp
     , runWithApp
     , runWithAppDebug
     , simpleEnv
     , withConn
     , withPostgres
     ) where

import           Colog                    ( HasLog (..), LogAction (..), Message, Msg (msgSeverity), Severity (..),
                                            filterBySeverity, richMessageAction )

import           Control.Monad.IO.Class   ( MonadIO )
import           Control.Monad.Reader     ( MonadReader, ReaderT (..), asks )

import           Data.Time                ( TimeZone, getCurrentTimeZone )

import           Database.Beam.Postgres   ( Connection, Pg, runBeamPostgres, runBeamPostgresDebug )
import           Database.BikeShare.Utils ( connectDbName, mkDbParams, uncurry5 )

import           Formatting

import           GHC.Stack                ( HasCallStack )

import           Prelude                  hiding ( log )

import           UnliftIO                 ( MonadUnliftIO, liftIO )

-- Application environment
data Env m where
  Env :: { envLogAction    :: !(LogAction m Message)
         , envLogDatabase  :: !Bool
         , envMinSeverity  :: !Severity
         , envTimeZone     :: !TimeZone
         , envDBConnection :: !Connection
         } -> Env m

{- | Type alias for constraint for:

1. Monad @m@ have access to environment @env@.
2. Environment @env@ contains 'LogAction' that can log messages of type @msg@.
3. Function call stack.

If you use this constraint, function call stack will be propagated and
you will have access to code lines that log messages.
-}
type WithAppEnv env msg m = (MonadReader env m, HasLog env msg m, HasCallStack, MonadIO m, MonadUnliftIO m, MonadFail m)

withConn :: (WithAppEnv (Env env) Message m) => m Connection
withConn = asks envDBConnection >>= liftIO . pure

withPostgres :: (WithAppEnv (Env env) Message m) => Pg a -> m a
withPostgres action = do
  conn <- withConn
  logDatabase <- asks envLogDatabase
  liftIO $
    if logDatabase then runBeamPostgresDebug pPrintCompact conn action
    else runBeamPostgres conn action

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
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (Env App), MonadFail)

-- | Simple environment for the main application.
simpleEnv :: TimeZone -> Connection -> Env App
simpleEnv timeZone conn = Env { envLogAction    = mainLogAction Info
                              , envLogDatabase  = False
                              , envMinSeverity  = Info
                              , envTimeZone     = timeZone
                              , envDBConnection = conn
                              }

-- | Environment for the main application.
mainEnv :: Severity -> Bool -> TimeZone -> Connection -> Env App
mainEnv sev logDatabase timeZone conn = Env { envLogAction    = mainLogAction sev
                                            , envLogDatabase  = logDatabase
                                            , envMinSeverity  = Info
                                            , envTimeZone     = timeZone
                                            , envDBConnection = conn
                                            }

-- | Log action for the main application.
mainLogAction :: (MonadIO m)
              => Severity            -- ^ Severity level (verbosity).
              -> LogAction m Message -- ^ Action used to log.
mainLogAction severity = filterBySeverity severity msgSeverity richMessageAction

  -- | Helper function to run the application.
runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env

-- | Helper function to run a computation in the App monad, returning an IO monad.
runWithApp :: String -> App a -> IO a
runWithApp dbname app = do
  conn <- mkDbParams dbname >>= uncurry5 connectDbName
  currentTimeZone <- getCurrentTimeZone
  runApp (mainEnv Info False currentTimeZone conn) app

-- | Helper function to run a computation in the App monad with debug and database logging, returning an IO monad.
runWithAppDebug :: String -> App a -> IO a
runWithAppDebug dbname app = do
  conn <- mkDbParams dbname >>= uncurry5 connectDbName
  currentTimeZone <- getCurrentTimeZone
  runApp (mainEnv Debug True currentTimeZone conn) app
