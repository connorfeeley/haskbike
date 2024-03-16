{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

-- |

module CLI.Poll.PollClientEnv
     ( PollAppM (..)
     , PollEnv (..)
     ) where

import           API.BikeShare
import           API.ResponseWrapper                    ( ResponseWrapper )

import           AppEnv

import           Colog                                  ( HasLog (..), LogAction (..), Message, Msg (msgSeverity),
                                                          Severity (..), filterBySeverity, logException,
                                                          richMessageAction, simpleMessageAction )

import           Control.Exception                      ( throw )
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader                   ( MonadReader, ReaderT (..), ask, asks )

import           Data.Pool
import           Data.Time                              ( TimeZone, getCurrentTimeZone )

import           Database.Beam.Postgres                 ( ConnectInfo, Connection, Pg, SqlError, close, connect,
                                                          runBeamPostgres, runBeamPostgresDebug )
import           Database.BikeShare.Connection          ( mkDbConnectInfo )
import           Database.BikeShare.EndpointQueried     ( EndpointQueried )
import           Database.PostgreSQL.Simple.Transaction ( withTransaction )

import           GHC.Conc                               ( numCapabilities )
import           GHC.Stack                              ( HasCallStack )

import           Network.HTTP.Client                    ( Manager, newManager )
import           Network.HTTP.Client.TLS                ( tlsManagerSettings )

import           Prelude                                hiding ( log )

import           Servant                                ( ServerError )
import           Servant.Client

import           UnliftIO                               ( MonadUnliftIO, TQueue, TVar )

data PollEnv m where
  PollEnv ::
    { envLogAction        :: !(LogAction m Message)
    , envLogDatabase      :: !Bool
    , envMinSeverity      :: !Severity
    , envTimeZone         :: !TimeZone
    , envDBConnectionPool :: !(Pool Connection)
    , envClientManager    :: !Manager
    , envBaseUrl          :: !BaseUrl
    , pollEnvEndpoint     :: !EndpointQueried
    , pollEnvTVar         :: !(TVar Int)
    , pollEnvTQueue       :: !(TQueue (ResponseWrapper apiType))
    } -> PollEnv m

newtype PollAppM a where
  PollAppM :: { unPollAppM :: ReaderT (PollEnv PollAppM) IO a
              } -> PollAppM a
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader (PollEnv PollAppM), MonadFail, MonadThrow, MonadCatch)
