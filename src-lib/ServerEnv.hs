{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- |

module ServerEnv
     ( ServerAppM (..)
     , ServerEnv (..)
     , getAppEnvFromServer
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


-- | ServerEnv data type that holds server-specific environment including App environment and the server port
-- The m parameter is the type variable which means 'ServerEnv' can hold environment of any Monad 'm'.
data ServerEnv m where
  ServerEnv :: { serverAppEnv    :: Env m
               -- The environment specific for the application
               , serverPort      :: !Int
               -- Port number on which the server is running
               } -> ServerEnv m


-- | ServerAppM is the monad in which the server side computations are carried out.
-- It encompasses both the server-specific environment and the application-specific environment.
-- The 'unServerAppM' function is used to strip away the ServerAppM constructor revealing the underlying ReaderT.
newtype ServerAppM a = ServerAppM
  { unServerAppM :: ReaderT (ServerEnv AppM) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (ServerEnv AppM), MonadFail, MonadThrow, MonadCatch)


-- | This instance allows us to use Servant's throwError and catchError inside actions of the ServerAppM monad.
instance MonadError ServerError ServerAppM where
  throwError = ServerAppM . throwM
  catchError action handler = ServerAppM $ catch (unServerAppM action) (unServerAppM . handler)


-- | Run the server application in the IO monad.
-- This function takes ServerEnv that has been initialized with the AppM environment and server configuration,
-- and a ServerAppM action that it then lifts into an IO.
runServerAppM :: ServerEnv AppM -> ServerAppM a -> IO a
runServerAppM senv app = runReaderT (unServerAppM app) senv


-- | Returns the application environment held within the ServerEnv.
-- This is useful when we want access to the AppM environment from within ServerAppM.
getAppEnvFromServer :: ServerAppM (Env AppM)
getAppEnvFromServer = asks serverAppEnv


-- | Natural transformation function to lift ServerAppM into the Handler monad.
-- ServerAppM actions are transformed into Handler actions using this function.
-- The Handler Monad is the one used by Servant for route handlers,
-- so the natural transformation is necessary to tell Servant how to operate with ServerAppM actions.
ntServerAppM :: ServerEnv AppM -> ServerAppM a -> Handler a
ntServerAppM s a =
  let r = runReaderT (unServerAppM a) s
  in liftIO r

-- | Natural transformation function to lift AppM into the Handler monad.
-- This function allows us to lift actions from AppM monad into the Handler monad,
-- so that Servant can understand and utilize the AppM functionalities.
ntAppM :: Env AppM -> AppM a -> Handler a
ntAppM s a =
  let r = runReaderT (unAppM a) s
  in liftIO r
