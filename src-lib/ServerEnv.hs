{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

-- |

module ServerEnv
     ( ServerAppM (..)
     , ServerEnv (..)
     , getAppEnvFromServer
     , ntAppM
     , ntServerAppM
     , runServerAppM
     , runWithServerAppM
     , runWithServerAppMDebug
     , runWithServerAppMSuppressLog
     ) where

import           AppEnv

import           Colog

import           Control.Monad.Catch      ( MonadCatch, MonadThrow, catch, throwM )
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Time

import           Database.Beam.Postgres
import           Database.BikeShare.Utils

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Prelude                  ()
import           Prelude.Compat

import           Servant

import           UnliftIO                 ( MonadUnliftIO )


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
  ServerEnv :: { serverAppEnv       :: !(Env AppM)
               -- The environment specific for the application
               , serverPort         :: !Int
               -- Port number on which the server is running
               , serverLogAction    :: !(LogAction m Message)
               -- Maximum number of intervals to query for
               , serverMaxIntervals :: Integer
               } -> ServerEnv m

-- Implement logging for the application environment.
instance HasLog (ServerEnv m) Message m where
  getLogAction :: ServerEnv m -> LogAction m Message
  getLogAction = serverLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> ServerEnv m -> ServerEnv m
  setLogAction newLogAction env = env { serverLogAction = newLogAction }
  {-# INLINE setLogAction #-}

-- | ServerAppM is the monad in which the server side computations are carried out.
-- It encompasses both the server-specific environment and the application-specific environment.
-- The 'unServerAppM' function is used to strip away the ServerAppM constructor revealing the underlying ReaderT.
newtype ServerAppM a = ServerAppM
  { unServerAppM :: ReaderT (ServerEnv ServerAppM) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (ServerEnv ServerAppM), MonadFail, MonadThrow, MonadCatch)


-- | This instance allows us to use Servant's throwError and catchError inside actions of the ServerAppM monad.
instance MonadError ServerError ServerAppM where
  throwError = ServerAppM . throwM
  catchError action handler = ServerAppM $ catch (unServerAppM action) (unServerAppM . handler)


-- | Run the server application in the IO monad.
-- This function takes ServerEnv that has been initialized with the AppM environment and server configuration,
-- and a ServerAppM action that it then lifts into an IO.
runServerAppM :: ServerEnv ServerAppM -> ServerAppM a -> IO a
runServerAppM senv app = runReaderT (unServerAppM app) senv


-- | Returns the application environment held within the ServerEnv.
-- This is useful when we want access to the AppM environment from within ServerAppM.
getAppEnvFromServer :: ServerAppM (Env AppM)
getAppEnvFromServer = asks serverAppEnv


-- | Natural transformation function to lift ServerAppM into the Handler monad.
-- ServerAppM actions are transformed into Handler actions using this function.
-- The Handler Monad is the one used by Servant for route handlers,
-- so the natural transformation is necessary to tell Servant how to operate with ServerAppM actions.
ntServerAppM :: ServerEnv ServerAppM -> ServerAppM a -> Handler a
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


-- * Helper functions to run ServerAppM actions (for debugging and test).

-- | Helper function to run a computation in the ServerAppM monad, returning an IO monad.
runWithServerAppM :: String -> ServerAppM a -> IO a
runWithServerAppM dbname action = do
  conn <- mkDbConnectInfo dbname >>= connect
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = mainEnv Info False True currentTimeZone conn clientManager
  let serverEnv = ServerEnv { serverAppEnv    = env
                            , serverPort      = 8081
                            , serverLogAction = simpleMessageAction
                            }
  liftIO $ runServerAppM serverEnv action


-- | This function is the same as runWithServerAppM, but overrides the log action to be a no-op.
runWithServerAppMSuppressLog :: String -> ServerAppM a -> IO a
runWithServerAppMSuppressLog dbname action = do
  conn <- mkDbConnectInfo dbname >>= connect
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = mainEnv Info False True currentTimeZone conn clientManager
  let serverEnv = ServerEnv { serverAppEnv       = env
                            , serverPort         = 8081
                            , serverLogAction    = mempty
                            , serverMaxIntervals = 4
                            }
  liftIO $ runServerAppM serverEnv action


-- | Helper function to run a computation in the ServerAppM monad with debug and database logging, returning an IO monad.
runWithServerAppMDebug :: String -> ServerAppM a -> IO a
runWithServerAppMDebug dbname action = do
  conn <- mkDbConnectInfo dbname >>= connect
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = mainEnv Debug True True currentTimeZone conn clientManager
  let serverEnv = ServerEnv { serverAppEnv    = env
                            , serverPort      = 8081
                            , serverLogAction = simpleMessageAction
                            }
  liftIO $ runServerAppM serverEnv action
