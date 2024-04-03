{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StarIsType             #-}
{-# LANGUAGE TypeFamilies           #-}

module Haskbike.API.APIEntity
     ( APIPersistable (..)
     , PollException (..)
     , PollResult (..)
     ) where

import           Colog

import           Control.Exception                           ( throw )
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch                         ( MonadCatch, MonadThrow )

import           Data.Maybe                                  ( mapMaybe )
import qualified Data.Text                                   as T
import           Data.Time.Extras

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions    ( MonadBeamInsertReturning (runInsertReturningList) )
import           Database.Beam.Postgres

import           Haskbike.API.ClientLifted
import           Haskbike.API.ResponseWrapper
import qualified Haskbike.API.StationInformation             as AT
import qualified Haskbike.API.StationStatus                  as AT
import qualified Haskbike.API.SystemInformation              as AT
import           Haskbike.AppEnv
import           Haskbike.CLI.Poll.Utils
import qualified Haskbike.Database.BikeShare                 as DB
import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Operations                ( insertStationInformation, insertStationStatus )
import qualified Haskbike.Database.Tables.StationInformation as DB
import qualified Haskbike.Database.Tables.StationStatus      as DB
import qualified Haskbike.Database.Tables.SystemInformation  as DB

import           Servant.Client                              ( ClientError, ClientM )

import           System.Directory.Internal.Prelude           ( exitFailure )

import           TextShow                                    ( showt )

import           UnliftIO
import           UnliftIO.Concurrent


data PollException where
  FullQueue :: EndpointQueried -> PollException
  deriving (Show, Typeable, Exception)


data PollResult where
  PollClientError :: ClientError -> PollResult
  WentBackwards   :: Int -> PollResult
  Success         :: (ResponseWrapper apiType) -> PollResult

class APIPersistable apiType dbType | apiType -> dbType where
  fromAPI :: ResponseWrapper apiType -> [dbType (QExpr Postgres s)]
  fromAPI _   = []

  insertAPI :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
            => ResponseWrapper apiType -> m [dbType Identity]
  insertAPI _ = pure []

  pollThread :: ( HasEnv env m
                -- , HasPollEnv env apiType m
                , MonadIO m
                , MonadThrow m
                , MonadCatch m
                ) => EndpointQueried -> ClientM (ResponseWrapper apiType) -> TVar Int -> TBQueue (ResponseWrapper apiType) -> m PollResult
  pollThread ep apiFetch lastUpdatedVar respQueue = do
    lastUpdated <- liftIO $ readTVarIO lastUpdatedVar
    runQueryM apiFetch >>=
      processResponse ep lastUpdated respQueue >>=
      handleFetchPersistResult ep lastUpdatedVar

  insertThread :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
               => EndpointQueried -> TBQueue (ResponseWrapper apiType) -> m [dbType Identity]
  insertThread ep respQueue = do
    -- Read the response queue.
    resp <- (atomically . readTBQueue) respQueue
    -- Insert the response into the database.
    logThread logDebug "Preparing to insert records into database."
    inserted <- insertAPI resp
    logThread logInfo (message inserted)
    pure inserted
    where
      epName = (T.pack . show) ep
      logThread logAction = logAction . (("[" <> epName <> " :: Insert] ") <>)
      message inserted = "Inserted " <> (T.pack . show . length) inserted <> " records into database."


delaySecs :: MonadIO m => Int -> m ()
delaySecs secs = threadDelay (secs * msPerS)
  where msPerS = 1000000

-- | Handle the result of fetching, decoding, and processing the API response.
handleFetchPersistResult :: (HasEnv env m, MonadIO m, MonadThrow m, Show a)
                         => a
                         -> TVar Int
                         -> PollResult
                         -> m PollResult
-- Handle a client error.
handleFetchPersistResult _ _ result@(PollClientError _err) = liftIO $ delaySecs 10 >> exitFailure >> pure result

-- Handle the API returning stale data.
handleFetchPersistResult _ _ result@(WentBackwards extendByMs) = liftIO (delaySecs extendByMs) >> pure result

-- Handle a successfully decoded response, with a non-stale "last_reported" value.
handleFetchPersistResult ep lastUpdatedVar result@(Success resp) = do
  liftIO $ atomically $ writeTVar lastUpdatedVar (utcToPosix (_respLastUpdated resp) + timeToLiveS resp)
  logInfo $ "[" <> epName <> "] Queued records for insertion - sleeping for " <> ttlTxt
  -- Sleep for requisite TTL.
  liftIO $ delaySecs (timeToLiveS resp)
  pure result
  where
    epName = (T.pack . show) ep
    ttlTxt = (showt . timeToLiveS) resp <> "s"
    timeToLiveS = _respTtl

-- | Process a (undecoded) response from the API.
processResponse :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                => EndpointQueried -> Int -> TBQueue (ResponseWrapper apiType) -> Either ClientError (ResponseWrapper apiType)
                -> m PollResult
processResponse ep lastUpdated respQueue resp = do
  case resp of
    Left err          -> persistQueryLog ep (Left err) >> pure (PollClientError err)
    Right respDecoded -> handleResponseWrapper ep respDecoded lastUpdated >>=
                         processValidResponse  ep respDecoded respQueue


-- | Process a (decoded) response from the API.
processValidResponse :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                     => EndpointQueried -> ResponseWrapper apiType -> TBQueue (ResponseWrapper apiType) -> Maybe Int
                     -> m PollResult
-- Went backwards - return early with amount of time to extend poll period by.
processValidResponse _ _ _ (Just extendByMs) = pure (WentBackwards extendByMs)
-- Went forwards - handle response.
processValidResponse ep resp respQueue Nothing = do
  -- Insert query log.
  persistQueryLog ep ((Right . _respLastUpdated) resp)

  -- Enqueue response data for handling by insertion thread.
  enqueueResponse respQueue resp ep

  pure (Success resp)

enqueueResponse :: (HasEnv env m, MonadIO m)
                => TBQueue (ResponseWrapper apiType) -> ResponseWrapper apiType -> EndpointQueried
                -> m ()
enqueueResponse respQueue resp ep = do
  (isFull, queueLen) <- atomically $ do
    isFull <- isFullTBQueue respQueue
    -- Enqueue response if not full.
    unless isFull $ do
      writeTBQueue respQueue resp

    -- Return new length of TBQueue.
    queueLen <- lengthTBQueue respQueue
    pure (isFull, queueLen)

  -- Log a warning if the queue wasn't already empty (indicating the consumer is not draining it quickly enough).
  when isFull $ do
    logError "Tried enqueued response but queue was full!"
    throw (FullQueue ep)
  -- Log a warning if the queue wasn't already empty (indicating the consumer is not draining it quickly enough).
  when (queueLen > 1) $
    logWarning . T.pack $ "Enqueued response, but queue was not empty! Queue length: " <> show queueLen


-- * Instances.

instance APIPersistable [AT.StationInformation] DB.StationInformationT where
  fromAPI   resp     = mapMaybe (Just . DB.fromJSONToBeamStationInformation (_respLastUpdated resp)) (_respData resp)
  insertAPI infoResp = insertStationInformation (map (_respLastUpdated infoResp, ) (_respData infoResp))


instance APIPersistable [AT.StationStatus] DB.StationStatusT where
  fromAPI  _resp = undefined
  insertAPI resp = insertStationStatus (_respData resp)


instance APIPersistable AT.SystemInformation DB.SystemInformationCountT where
  fromAPI resp = [DB.fromJSONToBeamSystemInformationCount (_respLastUpdated resp) (_respData resp)]

  insertAPI resp = withPostgres $ do
    infCnt <- runInsertReturningList $ insert (DB.bikeshareDb ^. DB.bikeshareSystemInformationCount)
      (insertExpressions (fromAPI resp))
    -- SystemInformationCount needs special handling, since it doesn't fit the typeclass's structure.
    _inf   <- runInsertReturningList $ insert (DB.bikeshareDb ^. DB.bikeshareSystemInformation)
      (insertExpressions [DB.fromJSONToBeamSystemInformation (_respLastUpdated resp) (_respData resp)])
    pure infCnt
