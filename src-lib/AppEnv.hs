-- | Application environment and monad.

{-# LANGUAGE DerivingStrategies #-}

module AppEnv
     ( App
     , Env (..)
     , runApp
     , simpleEnv
      , mainEnv
     ) where

import           Colog                  ( HasLog (..), LogAction(..), Message, richMessageAction, filterBySeverity, Severity (..), Msg (msgSeverity))

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader   ( MonadReader, ReaderT (..) )

import           Prelude                hiding ( log )

import           UnliftIO               ( MonadUnliftIO )

-- Application environment
data Env m where
  Env :: { envLogAction   :: !(LogAction m Message)
         , envMinSeverity :: !Severity
         } -> Env m

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
simpleEnv :: Env App
simpleEnv = Env { envLogAction   = mainLogAction Info
                , envMinSeverity = Info
                }

-- | Environment for the main application.
mainEnv :: Severity -> Env App
mainEnv sev = Env { envLogAction   = mainLogAction sev
                  , envMinSeverity = Info
                  }

-- | Log action for the main application.
mainLogAction :: (MonadIO m)
              => Severity            -- ^ Severity level (verbosity).
              -> LogAction m Message -- ^ Action used to log.
mainLogAction severity = filterBySeverity severity msgSeverity richMessageAction

  -- | Helper function to run the application.
runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env
