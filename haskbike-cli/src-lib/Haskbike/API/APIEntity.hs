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
import           Control.Monad.Reader                        ( MonadReader )

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

import           UnliftIO
import           UnliftIO.Concurrent


-- | Exceptions thrown by the polling/insertion threads.
data PollException where
  FullQueue :: EndpointQueried -> PollException
  deriving (Show, Typeable, Exception)


-- | Result of a poll operation.
data PollResult a where
  -- | Error querying the API. Abort.
  PollClientError :: ClientError -> PollResult a
  -- | Response successfully decoded.
  Decoded         :: ResponseValid a -> PollResult a


-- | Result of enqueuing a response.
data EnqueueResult where
  QueueFull     :: EnqueueResult
  QueueNotEmpty :: Int -> EnqueueResult
  QueueNominal  :: EnqueueResult


-- | Typeclass for API entities that can be persisted to the database.
class APIPersistable apiType dbType | apiType -> dbType where
  fromAPI :: ResponseWrapper apiType -> [dbType (QExpr Postgres s)]
  fromAPI _   = []

  insertAPI :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
            => ResponseWrapper apiType -> m [dbType Identity]
  insertAPI _ = pure []

  pollThread :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
             => EndpointQueried -> ClientM (ResponseWrapper apiType) -> TVar Int -> TBQueue (ResponseWrapper apiType)
             -> m (PollResult (ResponseWrapper apiType))
  pollThread ep apiFetch lastUpdatedVar respQueue = do
    lastUpdated <- liftIO $ readTVarIO lastUpdatedVar
    resp <- runQueryM apiFetch
    pollResult <- attemptDecoding ep lastUpdated resp
    case pollResult of
      PollClientError _err  -> liftIO $ delaySecs 10    >> exitFailure
      Decoded (StaleResponse delay)   -> liftIO $ delaySecs delay >> pure pollResult
      Decoded (ValidResponse respDecoded)   -> do
        queueResult <- processResponse respDecoded ep lastUpdatedVar respQueue
        case queueResult of
          -- Abort if response queue is full.
          QueueFull       -> throw (FullQueue ep)
          QueueNotEmpty _ -> pure pollResult
          QueueNominal    -> pure pollResult

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


-- * Functions for proccessing response.

-- | Sleep thread for seconds.
delaySecs :: MonadIO m => Int -> m ()
delaySecs secs = threadDelay (secs * msPerS)
  where msPerS = 1000000


-- | Process a (undecoded) response from the API.
attemptDecoding :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
                => EndpointQueried -> Int -> Either ClientError (ResponseWrapper apiType)
                -> m (PollResult (ResponseWrapper apiType))
attemptDecoding ep lastUpdated resp = do
  void $ persistQueryLog ep resp

  case resp of
    Left err          -> pure (PollClientError err)
    Right respDecoded -> do
      valid <- processWrapper  ep respDecoded lastUpdated
      case valid of
        StaleResponse delay   -> pure (Decoded (StaleResponse delay))
        ValidResponse decoded -> pure (Decoded (ValidResponse decoded))


-- | Handle the result of fetching, decoding, and processing the API response.
processResponse :: (MonadReader env m, HasLog env Message m, MonadIO m, HasEnv env m, MonadCatch m)
                => ResponseWrapper apiType -> EndpointQueried -> TVar Int -> TBQueue (ResponseWrapper apiType)
                -> m EnqueueResult
processResponse resp ep lstUpdV  respQueue = do
  queueResult <- enqueueResponse respQueue resp
  logQueueResult ep queueResult
  logInfo $ logBraces epName <> " Queued records for insertion - sleeping for " <> ttlTxt
  liftIO $ do
    -- Write last updated variable.
    atomically (writeTVar lstUpdV (utcToPosix (_respLastUpdated resp) + timeToLiveS resp))
    delaySecs (timeToLiveS resp)
  pure queueResult
  where
    epName      = (T.pack . show) ep
    ttlTxt      = (T.pack . show . timeToLiveS) resp <> "s"
    timeToLiveS = _respTtl


-- | Log the result of enqueing a response.
logQueueResult :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m) => EndpointQueried -> EnqueueResult -> m EnqueueResult
logQueueResult ep result = do
  case result of
    QueueFull        -> logError "Tried enqueueing response but queue was full!" >> throw (FullQueue ep)
    QueueNotEmpty sz -> logWarning ("Enqueued response, but queue was not empty! Queue length: " <> (T.pack . show) sz)
    QueueNominal     -> pure ()
  pure result


enqueueResponse :: (HasEnv env m, MonadIO m)
                => TBQueue (ResponseWrapper apiType) -> ResponseWrapper apiType
                -> m EnqueueResult
enqueueResponse respQueue resp = do
  (isFull, queueLen) <- atomically $ do
    queueLen <- lengthTBQueue respQueue
    isFull <- isFullTBQueue respQueue
    unless isFull $ -- Enqueue response if not full.
      writeTBQueue respQueue resp
    pure (isFull, queueLen)

  pure $ case (isFull, queueLen) of
    (True, _) -> QueueFull
    (_,    0) -> QueueNominal
    (_,   sz) -> QueueNotEmpty (fromIntegral sz)



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
