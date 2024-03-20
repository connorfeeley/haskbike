{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |

module CLI.Poll.PollClientEnv
     ( HasPollEnv (..)
     , PollEnv (..)
     , PollM (..)
     , runPollM
     ) where

import           API.ResponseWrapper                ( ResponseWrapper )

import           AppEnv

import           Colog                              ( HasLog (..) )
import           Colog.Core

import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader               ( MonadReader (..), ReaderT (..) )

import           Database.BikeShare.EndpointQueried ( EndpointQueried )

import           Prelude                            hiding ( log )

import           UnliftIO                           ( MonadUnliftIO, TQueue, TVar )


-- Poll application type
newtype PollM apiType a where
  PollM :: { unPollM :: ReaderT (PollEnv apiType PollM) IO a
                } -> PollM apiType a
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadUnliftIO
                   , MonadReader (PollEnv apiType PollM)
                   , MonadFail
                   , MonadThrow
                   , MonadCatch )


data PollEnv apiType m where
  PollEnv :: { pollEnvBase          :: !(Env AppM)
             , pollEnvEndpoint      :: !EndpointQueried
             , pollEnvLastUpdated   :: !(TVar Int)
             , pollEnvResponseQueue :: !(TQueue (ResponseWrapper apiType))
             } -> PollEnv apiType m



-- | 'HasPollEnv' class
class (Monad m, MonadReader env m, HasLog env Message m, MonadUnliftIO m, MonadCatch m) => HasPollEnv env apiType m | env m -> apiType where
  getPollEndpoint        :: m EndpointQueried
  getPollLastUpdated     :: m (TVar Int)
  getPollResponseQueue   :: m (TQueue (ResponseWrapper apiType))


-- | 'HasPollEnv' instance for 'PollEnv'
instance (Monad m, MonadReader (PollEnv apiType m) m, HasLog (PollEnv apiType m) Message m, MonadUnliftIO m, MonadCatch m) => HasPollEnv (PollEnv apiType m) apiType m where
  getPollEndpoint        = asks pollEnvEndpoint
  getPollLastUpdated     = asks pollEnvLastUpdated
  getPollResponseQueue   = asks pollEnvResponseQueue


-- | 'HasEnv' instance for 'ServerAppM'
instance (HasEnv (PollEnv apiType PollM) (PollM apiType)) where
    getLogDatabase      = asks (envLogDatabase       . pollEnvBase)
    getMinSeverity      = asks (envMinSeverity       . pollEnvBase)
    getTz               = asks (envTimeZone          . pollEnvBase)
    getDBConnectionPool = asks (envDBConnectionPool  . pollEnvBase)
    getClientManager    = asks (envClientManager     . pollEnvBase)
    getBaseUrl          = asks (envBaseUrl           . pollEnvBase)


-- runPollAppM :: ServerEnv PollM -> ServerAppM a -> IO a
runPollM :: (PollEnv apiType) PollM -> (PollM apiType) a -> IO a
runPollM env app = flip runReaderT env $ unPollM app


-- * 'HasLog' instances

adaptLogAction :: LogAction AppM msg -> LogAction (PollM apiType) msg
adaptLogAction (LogAction logAction') = LogAction $ \msg -> PollM $ do
  appEnv <- asks pollEnvBase
  liftIO $ flip runReaderT appEnv $ unAppM $ logAction' msg

instance HasLog (PollEnv apiType PollM) Message (PollM apiType) where
  getLogAction :: PollEnv apiType PollM -> LogAction (PollM apiType) Message
  getLogAction = adaptLogAction . envLogAction . pollEnvBase
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction (PollM apiType) Message -> PollEnv apiType PollM -> PollEnv apiType PollM
  setLogAction (LogAction newLogAction) extEnv = do
    extEnv { pollEnvBase = (pollEnvBase extEnv) { envLogAction = convertedAction } }
    where
      convertedAction = LogAction $ \msg -> AppM $
        liftIO $ flip runReaderT extEnv $ unPollM $ newLogAction msg
  {-# INLINE setLogAction #-}

instance HasLog (PollEnv apiType m) Message AppM where
  getLogAction :: PollEnv apiType m -> LogAction AppM Message
  getLogAction = getLogAction . pollEnvBase
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction AppM Message -> PollEnv apiType m -> PollEnv apiType m
  setLogAction newLogAction env = env { pollEnvBase = setLogAction newLogAction (pollEnvBase env) }
  {-# INLINE setLogAction #-}
