-- | Application environment and monad.

{-# LANGUAGE DerivingStrategies #-}

module AppEnv
     ( App
     , Env (..)
     , runApp
     , simpleEnv
     ) where

import           Colog                  ( HasLog (..), LogAction, Message, richMessageAction )

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader   ( MonadReader, ReaderT (..) )

import           Prelude                hiding ( log )

import           UnliftIO               ( MonadUnliftIO )

-- Application environment
data Env m where
  Env :: { envLogAction :: !(LogAction m Message) } -> Env m

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

simpleEnv :: Env App
simpleEnv = Env
    { envLogAction  = richMessageAction
    }

runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env
