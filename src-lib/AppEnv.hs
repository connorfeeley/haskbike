{-# LANGUAGE DerivingStrategies #-}

-- | Application environment and monad.
module AppEnv
     ( App
     , Env (..)
     , mainEnv
     , mainLogAction
     , runApp
     , simpleEnv
     ) where

import           Colog                  ( HasLog (..), LogAction (..), Message, Msg (msgSeverity), Severity (..),
                                          filterBySeverity, richMessageAction )

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader   ( MonadReader, ReaderT (..) )

import           Data.Time              ( TimeZone )

import           Prelude                hiding ( log )

import           UnliftIO               ( MonadUnliftIO )

-- Application environment
data Env m where
  Env :: { envLogAction   :: !(LogAction m Message)
         , envMinSeverity :: !Severity
         , envTimeZone    :: !TimeZone
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
simpleEnv :: TimeZone -> Env App
simpleEnv timeZone = Env { envLogAction   = mainLogAction Info
                         , envMinSeverity = Info
                         , envTimeZone    = timeZone
                         }

-- | Environment for the main application.
mainEnv :: Severity -> TimeZone -> Env App
mainEnv sev timeZone = Env { envLogAction   = mainLogAction sev
                           , envMinSeverity = Info
                           , envTimeZone    = timeZone
                           }

-- | Log action for the main application.
mainLogAction :: (MonadIO m)
              => Severity            -- ^ Severity level (verbosity).
              -> LogAction m Message -- ^ Action used to log.
mainLogAction severity = filterBySeverity severity msgSeverity richMessageAction

  -- | Helper function to run the application.
runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env
