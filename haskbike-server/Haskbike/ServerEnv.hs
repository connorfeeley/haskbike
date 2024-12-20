{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |

module Haskbike.ServerEnv
     ( HasServerEnv (..)
     , module Haskbike.AppEnv
     , ServerAppM (..)
     , ServerEnv (..)
     , WithServerEnv
     , ntServer
     , runServerAppM
     , runWithServerAppM
     , runWithServerAppMDebug
     , runWithServerAppMSuppressLog
     ) where

import           Colog

import           Control.Monad.Catch            ( MonadCatch, MonadThrow, throwM )
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Fixed                     ( Pico )
import           Data.Time

import           Haskbike.AppEnv
import           Haskbike.Server.ExternalAssets

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Prelude                        ()
import           Prelude.Compat

import qualified Servant                        as S

import           UnliftIO

-- Server application type
newtype ServerAppM a where
  ServerAppM :: { unServerAppM :: ReaderT (ServerEnv ServerAppM) IO a
                } -> ServerAppM a
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadUnliftIO
                   , MonadReader (ServerEnv ServerAppM)
                   , MonadFail
                   , MonadThrow
                   , MonadCatch )

data ServerEnv m = ServerEnv
  { serverEnvBase         :: !(Env AppM)
  , serverPort            :: !Int                   -- ^ Port number on which the server is running
  , serverTimeoutSeconds  :: !Int                   -- ^ Timeout in seconds for the server.
  , serverGzipCompression :: !Bool                  -- ^ Whether to use gzip compression.
  , serverMaxIntervals    :: Pico
  , serverContactEmail    :: String
  , serverAssets          :: ExternalAssetLocation
  }

{- | Type alias for constraint for:

1. Monad @m@ have access to environment @env@.
2. Environment @env@ contains 'LogAction' that can log messages of type @msg@.
3. Function call stack.

If you use this constraint, function call stack will be propagated and
you will have access to code lines that log messages.
-}
type WithServerEnv m = HasServerEnv (ServerEnv ServerAppM) m

-- | 'HasServerEnv' class
class (HasLog env Message m, MonadReader env m, MonadError S.ServerError m, MonadUnliftIO m, MonadCatch m) => HasServerEnv env m where
  getServerPort            :: m Int
  getServerTimeoutSeconds  :: m Int
  getServerGzipCompression :: m Bool
  getServerMaxIntervals    :: m Pico
  getServerContactEmail    :: m String
  getServerAssetsLocation  :: m ExternalAssetLocation


-- | 'HasServerEnv' instance for 'ServerEnv'
instance (Monad m, MonadReader (ServerEnv m) m, HasLog (ServerEnv m) Message m, MonadUnliftIO m, MonadError S.ServerError m, MonadCatch m) => HasServerEnv (ServerEnv m) m where
  getServerPort            = asks serverPort
  getServerTimeoutSeconds  = asks serverTimeoutSeconds
  getServerGzipCompression = asks serverGzipCompression
  getServerMaxIntervals    = asks serverMaxIntervals
  getServerContactEmail    = asks serverContactEmail
  getServerAssetsLocation  = asks serverAssets


-- | 'HasEnv' instance for 'ServerAppM'
instance (HasEnv (ServerEnv ServerAppM) ServerAppM) where
    getLogDatabase      = asks (envLogDatabase       . serverEnvBase)
    getMinSeverity      = asks (envMinSeverity       . serverEnvBase)
    getTz               = asks (envTimeZone          . serverEnvBase)
    getDBConnectionPool = asks (envDBConnectionPool  . serverEnvBase)
    getClientManager    = asks (envClientManager     . serverEnvBase)
    getBaseUrl          = asks (envBaseUrl           . serverEnvBase)


-- * 'HasLog' instances

adaptLogAction :: LogAction AppM msg -> LogAction ServerAppM msg
adaptLogAction (LogAction logAction') = LogAction $ \msg -> ServerAppM $ do
  appEnv <- asks serverEnvBase
  liftIO $ flip runReaderT appEnv $ unAppM $ logAction' msg

instance HasLog (ServerEnv ServerAppM) Message ServerAppM where
  getLogAction :: ServerEnv ServerAppM -> LogAction ServerAppM Message
  getLogAction = adaptLogAction . envLogAction . serverEnvBase
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction ServerAppM Message -> ServerEnv ServerAppM -> ServerEnv ServerAppM
  setLogAction (LogAction newLogAction) extEnv = do
    extEnv { serverEnvBase = (serverEnvBase extEnv) { envLogAction = convertedAction } }
    where
      convertedAction = LogAction $ \msg -> AppM $
        liftIO $ flip runReaderT extEnv $ unServerAppM $ newLogAction msg
  {-# INLINE setLogAction #-}

instance HasLog (ServerEnv m) Message AppM where
  getLogAction :: ServerEnv m -> LogAction AppM Message
  getLogAction = getLogAction . serverEnvBase
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction AppM Message -> ServerEnv m -> ServerEnv m
  setLogAction newLogAction env = env { serverEnvBase = setLogAction newLogAction (serverEnvBase env) }
  {-# INLINE setLogAction #-}


-- | MonadError instance for 'ServerAppM'
instance MonadError S.ServerError ServerAppM where
  throwError = ServerAppM . throwM
  catchError action handler = ServerAppM $ catch (unServerAppM action) (unServerAppM . handler)


-- | Natural transformation function to lift ServerAppM into the Handler monad.
-- ServerAppM actions are transformed into Handler actions using this function.
-- The Handler Monad is the one used by Servant for route handlers,
-- so the natural transformation is necessary to tell Servant how to operate with ServerAppM actions.
-- ntServerAppM :: MonadIO m => ServerEnv env m ->  a -> m a
-- ntServerAppM s a =
--   let r = runReaderT (unServerAppM a) s
--   in liftIO r
-- ntServerAppM :: MonadIO m2 => ServerEnv env m -> ServerAppM m a -> m2 a
ntServer :: r -> ReaderT r m a -> m a
ntServer env action = runReaderT action env


-- runServerAppM :: ServerEnv ServerAppM -> ServerAppM a -> IO a
runServerAppM :: ServerEnv ServerAppM -> ServerAppM a -> IO a
runServerAppM env app = flip runReaderT env $ unServerAppM app


-- | Helper function to run a computation in the ServerAppM monad, returning an IO monad.
runWithServerAppM :: String -> ServerAppM a -> IO a
runWithServerAppM dbname action = do
  connPool <- mkDbConnectInfo dbname >>= mkDatabaseConnectionPool
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = mainEnv Info False True currentTimeZone connPool clientManager
  let serverEnv = ServerEnv { serverEnvBase         = env
                            , serverPort            = 8081
                            , serverTimeoutSeconds  = 5 * 60
                            , serverGzipCompression = True
                            , serverMaxIntervals    = 20
                            , serverContactEmail    = "bikes@cfeeley.org"
                            , serverAssets          = ExternalAssetCDN
                            }
  liftIO $ runServerAppM serverEnv action


-- | This function is the same as runWithServerAppM, but overrides the log action to be a no-op.
runWithServerAppMSuppressLog :: String -> ServerAppM a -> IO a
runWithServerAppMSuppressLog dbname action = do
  connPool <- mkDbConnectInfo dbname >>= mkDatabaseConnectionPool
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = mainEnv Info False True currentTimeZone connPool clientManager
  let serverEnv = ServerEnv { serverEnvBase         = env
                            , serverPort            = 8081
                            , serverTimeoutSeconds  = 5 * 60
                            , serverGzipCompression = True
                            , serverMaxIntervals    = 20
                            , serverContactEmail    = "bikes@cfeeley.org"
                            , serverAssets          = ExternalAssetCDN
                            }
  liftIO $ runServerAppM serverEnv action


-- | Helper function to run a computation in the ServerAppM monad with debug and database logging, returning an IO monad.
runWithServerAppMDebug :: String -> ServerAppM a -> IO a
runWithServerAppMDebug dbname action = do
  connPool <- mkDbConnectInfo dbname >>= mkDatabaseConnectionPool
  currentTimeZone <- getCurrentTimeZone
  clientManager <- liftIO $ newManager tlsManagerSettings
  let env = mainEnv Debug True True currentTimeZone connPool clientManager
  let serverEnv = ServerEnv { serverEnvBase         = env
                            , serverPort            = 8081
                            , serverTimeoutSeconds  = 5 * 60
                            , serverGzipCompression = True
                            , serverMaxIntervals    = 20
                            , serverContactEmail    = "bikes@cfeeley.org"
                            , serverAssets          = ExternalAssetCDN
                            }
  liftIO $ runServerAppM serverEnv action


-- Function to adapt LogAction from AppM to ServerAppM
-- adaptLogAction :: LogAction AppM msg -> LogAction ServerAppM msg
-- adaptLogAction (LogAction logAction') = LogAction $ \msg -> ServerAppM $ do
--   appEnv <- getServerAppEnv
--   liftIO $ runReaderT (unAppM $ logAction' msg) appEnv
