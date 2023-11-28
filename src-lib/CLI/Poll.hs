{-# OPTIONS_GHC -Wno-orphans #-}
-- | Poll the API for status updates, inserting results in database as needed.
module CLI.Poll
     ( dispatchPoll
     , pollClient
     ) where

import           API.Client
import           API.ClientLifted
import           API.Pollable
import           API.ResponseWrapper
import           API.Types                     ( StationInformationResponse, StationStatusResponse, unInfoStations,
                                                 unStatusStations )

import           AppEnv

import           CLI.Options                   ( PollOptions (..) )

import           Colog                         ( WithLog, log, logException, pattern D, pattern I, pattern W )

import           Control.Concurrent            ( threadDelay )
import           Control.Concurrent.STM
import           Control.Exception             ( throw )
import           Control.Lens
import           Control.Monad                 ( void, when )
import           Control.Monad.Cont            ( forever )
import           Control.Monad.Reader          ( MonadReader )

import           Data.Maybe                    ( fromMaybe, isJust, isNothing )
import qualified Data.Text                     as Text
import           Data.Time.Extras

import           Database.BikeShare
import           Database.BikeShare.Operations

import           Fmt

import           Formatting

import           Prelude                       hiding ( log )

import           TextShow                      ( showt )

import           UnliftIO                      ( MonadIO, MonadUnliftIO, liftIO )
import           UnliftIO.Async                ( concurrently_ )


-- | Dispatch CLI arguments to the poller.
dispatchPoll :: PollOptions
             -> AppM ()
dispatchPoll _options = pollClient


pollClient :: AppM ()
pollClient = do
  (statusTtl, statusLastUpdated, statusQueueResp :: TBQueue StationStatusResponse, infoTtl, infoLastUpdated, infoQueueResp) <- liftIO $
    (,,,,,) <$> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4 <*> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4

  log I "Fetching station information from API once."
  concurrently_
    (informationRequester infoQueueResp infoTtl infoLastUpdated)
    (informationHandler infoQueueResp)

  log I "Polling API for information and status updates."
  void $
    concurrently_
      (spawnPoll infoQueueResp   infoTtl   infoLastUpdated)
      (spawnPoll statusQueueResp statusTtl statusLastUpdated)
  log I "Done."
  where
    spawnPoll :: (Pollable a) => TBQueue a -> TVar Int -> TVar Int -> AppM ()
    spawnPoll queueResp ttl lastUpdated = concurrently_
                    (forever $ pollData   queueResp ttl lastUpdated)
                    (forever $ handleData queueResp)




-- * TTL and timing handlers.

-- | Handle last_updated field in response.
handleTimeElapsed :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
                  => Text.Text         -- ^ Log prefix
                  -> ResponseWrapper a -- ^ TTL from API response.
                  -> TVar Int          -- ^ TVar holding last_updated between threads.
                  -> m (Maybe Int)     -- ^ Optional number of seconds extend TTL by, if last_updated went backwards.
handleTimeElapsed logPrefix apiResult lastUpdatedVar = do
  let currentTime' = apiResult ^. respLastUpdated
  previousTime <- liftIO $ readTVarIO lastUpdatedVar
  let previousTime' = posixToLocal previousTime
  let timeElapsed = utcToPosix currentTime' - previousTime

  -- Check if last_updated went backwards
  if timeElapsed >= 0
    then do -- Update last_updated variable.
      liftIO $ atomically $ writeTVar lastUpdatedVar (utcToPosix currentTime')
      log I $ format "({}) last updated [{}]" logPrefix currentTime'
      pure Nothing
    else do
      log W $ format "({}) last updated went backwards: [{}] -> [{}] | ({})" logPrefix previousTime' currentTime' timeElapsed
      pure (Just (-timeElapsed))


-- | Handle last_updated field in response.
handleTTL :: (WithLog env Message m, MonadIO m, MonadUnliftIO m)
          => Text.Text         -- ^ Log prefix
          -> ResponseWrapper a -- ^ TTL from API response.
          -> TVar Int          -- ^ TVar holding TTL.
          -> Maybe Int         -- ^ Optional number of seconds to extend TTL by.
          -> m ()
handleTTL logPrefix apiResult ttlVar extendBy = do
  let ttlSecs = apiResult ^. respTtl

  when (isJust extendBy) $ log W $ format "({}) extending TTL by {}s" logPrefix (fromMaybe 0 extendBy)
  log I $ format "({}) TTL={}{}" logPrefix (showt ttlSecs) (maybe "" (("+" ++) . show) extendBy)

  liftIO $ atomically $ writeTVar ttlVar (ttlSecs + fromMaybe 0 extendBy)


-- * Station status handling.

instance Pollable StationStatusResponse where
    pollData   = statusRequester
    handleData = statusHandler

-- | Thread action to request station information from API.
statusRequester :: (WithLog env Message m, MonadIO m, MonadUnliftIO m, MonadReader (Env m) m)
                => TBQueue StationStatusResponse -- ^ Queue of responses.
                -> TVar Int                      -- ^ Interval between requests in seconds.
                -> TVar Int                      -- ^ Last updated time.
                -> m ()
statusRequester queue intervalSecsVar lastUpdated = void $ do
  runQueryM stationStatus >>= \case
    Left err -> logException err >> throw err
    Right result -> do
      -- Handle TTL upfront, so that we don't sleep when called
      -- for the first time from 'pollClient' (TTL = 0).
      intervalSecs <- liftIO $ readTVarIO intervalSecsVar
      log D $ format "(Status) Sleeping for {} seconds." intervalSecs
      liftIO $ threadDelay (intervalSecs * 1000000)

      elapsedResult <- handleTimeElapsed "Status" result lastUpdated
      handleTTL "Status" result intervalSecsVar elapsedResult

      -- If 'elapsedResult' is a 'Just', that indicates that last_updated went backwards.
      -- In that case, we should NOT write the result to the queue, and instead extend the
      -- time until our next poll by however long the time went backwards.
      when (isNothing elapsedResult) (liftIO $ atomically $ writeTBQueue queue result)

-- | Thread action to handle API response for station status query.
statusHandler :: TBQueue StationStatusResponse  -- ^ Queue of responses
              -> AppM ()
statusHandler queue = void $ do
  response <- liftIO $ atomically $ readTBQueue queue

  let status = response ^. (respData . unStatusStations)
  log I $ format "(Status) Received {} status records from API." (length status)

  -- Insert the updated status.
  insertedResult <- insertStationStatus status

  -- Log each station ID updated.
  mapM_ (log D . Text.pack . fmtLog . dataToTuple) insertedResult

  -- Log number of inserted rows.
  log I $ format "(Status) Inserted: {}" (length insertedResult)

  where
    dataToTuple s = ( s ^. statusStationId . unInformationStationId
                    , s ^. statusLastReported
                    )
    fmtLog (sid, lr) = format "ID: {} {}" sid (pShowCompact lr)


-- * Station information handling.

instance Pollable StationInformationResponse where
    pollData   = informationRequester
    handleData = informationHandler

-- | Thread action to request station information from API.
informationRequester :: (WithLog env Message m, MonadIO m, MonadUnliftIO m, MonadReader (Env m) m)
                     => TBQueue StationInformationResponse -- ^ Queue of responses.
                     -> TVar Int                           -- ^ Interval between requests, in seconds.
                     -> TVar Int                           -- ^ Last updated time.
                     -> m ()
informationRequester queue intervalSecsVar lastUpdated = void $ do
  runQueryM stationInformation >>= \case
    Left err -> logException err >> throw err
    Right result -> do
      -- Handle TTL upfront, so that we don't sleep when called
      -- for the first time from 'pollClient' (TTL = 0).
      intervalSecs <- liftIO $ readTVarIO intervalSecsVar
      log D $ format "(Info) Sleeping for {} seconds." intervalSecs
      liftIO $ threadDelay (intervalSecs * 1000000)

      elapsedResult <- handleTimeElapsed "Info" result lastUpdated
      handleTTL "Info" result intervalSecsVar elapsedResult

      -- If 'elapsedResult' is a 'Just', that indicates that last_updated went backwards.
      -- In that case, we should NOT write the result to the queue, and instead extend the
      -- time until our next poll by however long the time went backwards.
      when (isNothing elapsedResult) (liftIO $ atomically $ writeTBQueue queue result)

-- | Thread action to handle API response for station information query.
informationHandler :: TBQueue StationInformationResponse  -- ^ Queue of responses
                   -> AppM ()
informationHandler queue = void $ do
  response <- liftIO $ atomically $ readTBQueue queue

  let info = response ^. (respData . unInfoStations)
  log I $ format "(Info) Received {} info records from API." (length info)

  -- Insert the station information (updating existing records, if existing).
  insertedResult <- insertStationInformation info

  -- Log each station ID updated.
  mapM_ (log D . Text.pack . fmtLog) insertedResult

  -- Log number of inserted rows.
  log I $ format "(Info) Updated/inserted {} records into database." (length insertedResult)

  where
    fmtLog inserted = format "ID: {}" (pShowCompact inserted)
