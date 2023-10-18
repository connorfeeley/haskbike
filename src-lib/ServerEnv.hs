{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- |

module ServerEnv
     ( ServerAppM (..)
     , ServerEnv (..)
     , getEnvFromServer
     , ntAppM
     , ntServerAppM
     , runServerAppM
     ) where

import           AppEnv

import           Control.Monad.Catch  ( MonadCatch, MonadThrow, catch, throwM )
import           Control.Monad.Except
import           Control.Monad.Reader

import           Prelude              ()
import           Prelude.Compat

import           Servant

import           UnliftIO             ( MonadUnliftIO )


{-
The difference between 'ServerEnv AppM' and 'ServerEnv ServerAppM' lies in the context of how they are used.

1. 'ServerEnv AppM':
        'ServerEnv' is taking an 'AppM' as an argument.
        It means that the server environment is constructed on top of an `AppM` environment.
        In plain words, we have a server-specific environment built upon a general application environment.

2. 'ReaderT (ServerEnv AppM) IO a':
        Here, `ReaderT` monad transformer wraps around the `ServerEnv AppM` environment,
        implying that actions executed in the monad will have access to server context built on an `AppM` context.

The significance of defining 'ServerAppM' in terms of 'ReaderT (ServerEnv AppM) IO' is that now the server logic (in 'ServerAppM' context)
is also aware of 'AppM' context because of 'ServerEnv AppM' being inside 'ReaderT'.
We are able to access states from both 'ServerEnv' and 'Env'.
This is helpful in maintaining a clean separation of variables specific to both contexts while still maintaining access to them when needed.
-}

-- | Definition of Server environment data type
data ServerEnv m where
  ServerEnv :: { serverPort      :: !Int
               , serverEnv       :: Env m
               } -> ServerEnv m

-- | Definition of ServerAppM monad with necessary instances derived
newtype ServerAppM a = ServerAppM
  { unServerAppM :: ReaderT (ServerEnv AppM) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (ServerEnv AppM), MonadFail, MonadThrow, MonadCatch)


-- | MonadError instance for ServerAppM to handle Servant errors
instance MonadError ServerError ServerAppM where
  throwError = ServerAppM . throwM
  catchError action handler = ServerAppM $ catch (unServerAppM action) (unServerAppM . handler)


-- | Run the server application by lifting it into IO context
runServerAppM :: ServerEnv AppM -> ServerAppM a -> IO a
runServerAppM senv app = runReaderT (unServerAppM app) senv

-- | Extract Env object from ServerEnv within the ServerAppM context
getEnvFromServer :: ServerAppM (Env AppM)
getEnvFromServer = asks serverEnv

-- * Natural transformations

-- | Natural transformation function to lift ServerAppM into the Handler context
ntServerAppM :: ServerEnv AppM -> ServerAppM a -> Handler a
ntServerAppM s a =
  let r = runReaderT (unServerAppM a) s
  in liftIO r

-- | Natural transformation function to lift AppM into the Handler context
ntAppM :: Env AppM -> AppM a -> Handler a
ntAppM s a =
  let r = runReaderT (unAppM a) s
  in liftIO r
