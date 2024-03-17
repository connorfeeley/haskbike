{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |

module CLI.Poll.PollClientEnv
     ( HasPollEnv (..)
     , PollAppM (..)
     , PollEnv (..)
     , PollReaderT (..)
     , WithPollEnv
     , runPollReader
     ) where

import           API.ResponseWrapper                ( ResponseWrapper )

import           AppEnv

import           Colog                              ( HasLog (..) )

import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader               ( MonadReader (..), ReaderT (..) )

import           Database.BikeShare.EndpointQueried ( EndpointQueried )

import           GHC.Stack                          ( HasCallStack )

import           Prelude                            hiding ( log )

import           UnliftIO                           ( MonadUnliftIO, TQueue, TVar )


class (HasLog env Message m, MonadReader env m) => HasPollEnv env m where
  getEndpoint :: m EndpointQueried


instance (Monad m, MonadReader (PollEnv m) m, HasLog (PollEnv m) Message m) => HasEnv (PollEnv m) m where
    getLogAction        = asks (envLogAction . pollAppEnv)
    getLogDatabase      = asks (envLogDatabase . pollAppEnv)
    getMinSeverity      = asks (envMinSeverity . pollAppEnv)
    getTimeZone         = asks (envTimeZone . pollAppEnv)
    getDBConnectionPool = asks (envDBConnectionPool . pollAppEnv)
    getClientManager    = asks (envClientManager . pollAppEnv)
    getBaseUrl          = asks (envBaseUrl . pollAppEnv)

instance (Monad m, MonadReader (PollEnv e) m, HasLog (PollEnv e) Message m) => HasPollEnv (PollEnv e) m where
  getEndpoint = asks pollEnvEndpoint


data PollEnv m where
  PollEnv :: { pollAppEnv      :: !(Env m)
             , pollEnvEndpoint :: !EndpointQueried
             , pollEnvTVar     :: !(TVar Int)
             , pollEnvTQueue   :: !(TQueue (ResponseWrapper apiType))
             } -> PollEnv m

newtype PollReaderT r m a = PollReaderT
  { runPollReaderT :: ReaderT r m a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance Monad m => MonadReader (Env AppM) (PollReaderT (PollEnv AppM) m) where
  ask = PollReaderT (asks pollAppEnv)
  local f m = PollReaderT (local (\env -> env {pollAppEnv = f (pollAppEnv env)}) (runPollReaderT m))

runPollReader :: PollReaderT r m a -> r -> m a
runPollReader = runReaderT . runPollReaderT


newtype PollAppM a where
  PollAppM :: { unPollAppM :: ReaderT (PollEnv PollAppM) IO a
              } -> PollAppM a
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (PollEnv PollAppM), MonadFail, MonadThrow, MonadCatch)

-- instance MonadPoll PollAppM

{- | Type alias for constraint for:

1. Monad @m@ have access to environment @env@.
2. Environment @env@ contains 'LogAction' that can log messages of type @msg@.
3. Function call stack.
-}
type WithPollEnv env msg m = ( MonadReader env m
                             , HasLog env msg m
                             , HasCallStack
                             , MonadIO m
                             , MonadUnliftIO m
                             , MonadFail m
                             , MonadThrow m
                             , MonadCatch m
                             )
