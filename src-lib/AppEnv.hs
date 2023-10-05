{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}

-- | Application environment and monad.
module AppEnv
     ( App
     , Env (..)
     , WithAppEnv
     , withConn
       -- , HasConnection(..)
     , mainEnv
     , mainLogAction
     , runApp
     , simpleEnv
     ) where

import           Colog                  ( HasLog (..), LogAction (..), Message, Msg (msgSeverity), Severity (..),
                                          filterBySeverity, richMessageAction )

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader   ( MonadReader, ReaderT (..), asks )

import           Data.Time              ( TimeZone )

import           Database.Beam.Postgres ( Connection )

import           GHC.Stack              ( HasCallStack )

import           Prelude                hiding ( log )

import           UnliftIO               ( MonadUnliftIO, liftIO )

-- Application environment
data Env m where
  Env :: { envLogAction    :: !(LogAction m Message)
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
type WithAppEnv env msg m = (MonadReader env m, HasLog env msg m, HasCallStack, MonadIO m, MonadUnliftIO m)

withConn :: (WithAppEnv (Env env) Message m) => m Connection
withConn = asks envDBConnection >>= liftIO . pure

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
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (Env App))

-- | Simple environment for the main application.
simpleEnv :: TimeZone -> Connection -> Env App
simpleEnv timeZone conn = Env { envLogAction    = mainLogAction Info
                              , envMinSeverity  = Info
                              , envTimeZone     = timeZone
                              , envDBConnection = conn
                              }

-- | Environment for the main application.
mainEnv :: Severity -> TimeZone -> Connection -> Env App
mainEnv sev timeZone conn = Env { envLogAction    = mainLogAction sev
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
