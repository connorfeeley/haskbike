-- | Poll the API for status updates, inserting results in database as needed.
module CLI.Poll
     ( dispatchPoll
     , pollClient
     ) where


import           API.Client
import           API.ClientLifted
import           API.Pollable
import           API.ResponseWrapper
import qualified API.Types                     as AT

import           AppEnv

import           CLI.Options                   ( PollOptions (..) )

import           Colog                         ( log, logException, logInfo, pattern D, pattern I, pattern W )

import           Control.Concurrent            ( threadDelay )
import           Control.Concurrent.STM
import           Control.Exception             ( throw )
import           Control.Lens
import           Control.Monad                 ( void, when )
import           Control.Monad.Cont            ( forever )

import           Data.Maybe                    ( fromMaybe, isJust, isNothing )
import qualified Data.Text                     as Text
import           Data.Time.Extras

import           Database.BikeShare
import           Database.BikeShare.Operations

import           Fmt

import           Formatting

import           Prelude                       hiding ( log )

import           Servant.Client                ( ClientM )

import           TextShow                      ( showt )

import           UnliftIO                      ( Async, async, liftIO, waitAnyCancel, waitBoth )


-- | Dispatch CLI arguments to the poller.
dispatchPoll :: PollOptions
             -> AppM ()
dispatchPoll _options = pollClient


pollClient :: AppM ()
pollClient = do
    -- Initialize TVars and TBQueues for station information and station status.
    (statusTtl, statusLastUpdated, statusQueueResp    :: TBQueue (ResponseWrapper [AT.StationStatus]),
     infoTtl, infoLastUpdated, infoQueueResp          :: TBQueue (ResponseWrapper [AT.StationInformation]),
     sysInfoTtl, sysInfoLastUpdated, sysInfoQueueResp :: TBQueue (ResponseWrapper AT.SystemInformation)) <- liftIO $
        (,,,,,,,,) <$> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4
                   <*> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4
                   <*> newTVarIO 0 <*> newTVarIO 0 <*> newTBQueueIO 4

    logInfo "Fetching station information from API once."
    -- Threads to fetch and handle station information from API once.
    infoPollOnce   <- async (requester infoQueueResp infoTtl infoLastUpdated)
    infoHandleOnce <- async (handler   infoQueueResp)
    -- Wait for both threads to finish.
    waitBoth infoPollOnce infoHandleOnce


    logInfo "Initializing polling and handling threads."
    -- The polling threads.
    infoPollingThread    <- createPollingThread infoQueueResp    infoTtl    infoLastUpdated
    statusPollingThread  <- createPollingThread statusQueueResp  statusTtl  statusLastUpdated
    sysInfoPollingThread <- createPollingThread sysInfoQueueResp sysInfoTtl sysInfoLastUpdated

    -- The handling threads for station information and station status.
    infoHandlingThread    <- createHandlingThread infoQueueResp
    statusHandlingThread  <- createHandlingThread statusQueueResp
    sysInfoHandlingThread <- createHandlingThread sysInfoQueueResp

    -- Start concurrent operations and wait until any one of them finishes (which should not happen).
    -- If any thread finishes, we cancel the others as there is an implied dependency between them.
    _ <- waitAnyCancel [ infoPollingThread,  statusPollingThread,  sysInfoPollingThread
                       , infoHandlingThread, statusHandlingThread, sysInfoHandlingThread
                       ]

    logInfo "Polling and handling threads running. Press Ctrl+C to terminate."
    return ()

-- Create async actions for both requester and handler threads.
createPollingThread :: Pollable a => TBQueue a -> TVar Int -> TVar Int -> AppM (Async b)
createPollingThread queue ttlVar lastUpdatedVar = (async . forever) (requester queue ttlVar lastUpdatedVar)

createHandlingThread :: Pollable a => TBQueue a -> AppM (Async b)
createHandlingThread queue = async . forever $ handler queue

-- * TTL and timing handlers.

-- | Handle last_updated field in response.
handleTimeElapsed :: Text.Text         -- ^ Log prefix
                  -> ResponseWrapper a -- ^ TTL from API response.
                  -> TVar Int          -- ^ TVar holding last_updated between threads.
                  -> AppM (Maybe Int)     -- ^ Optional number of seconds extend TTL by, if last_updated went backwards.
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
handleTTL :: Pollable (ResponseWrapper a)
          => Text.Text         -- ^ Log prefix
          -> ResponseWrapper a -- ^ TTL from API response.
          -> TVar Int          -- ^ TVar holding TTL.
          -> Maybe Int         -- ^ Optional number of seconds to extend TTL by.
          -> AppM ()
handleTTL logPrefix apiResult ttlVar extendBy = do
  let ttlSecs = apiResult ^. respTtl

  when (isJust extendBy) $ log W $ format "({}) extending TTL by {}s" logPrefix (fromMaybe 0 extendBy)
  log I $ format "({}) TTL={}{}" logPrefix (showt ttlSecs) (maybe "" (("+" ++) . show) extendBy)

  liftIO $ atomically $ writeTVar ttlVar (ttlSecs + fromMaybe 0 extendBy)

requesterFn :: Pollable (ResponseWrapper a)
            => Text.Text
            -> ClientM (ResponseWrapper a)
            -> TBQueue (ResponseWrapper a)
            -> TVar Int
            -> TVar Int
            -> AppM ()
requesterFn prefix query queue intervalSecsVar lastUpdated = void $ do
  runQueryM query >>= \case
    Left err -> logException err >> throw err
    Right result -> do
      -- Handle TTL upfront, so that we don't sleep when called
      -- for the first time from 'pollClient' (TTL = 0).
      intervalSecs <- liftIO $ readTVarIO intervalSecsVar
      log D $ format "({}) Sleeping for {} seconds." prefix intervalSecs
      liftIO $ threadDelay (intervalSecs * 1000000)

      elapsedResult <- handleTimeElapsed prefix result lastUpdated
      handleTTL prefix result intervalSecsVar elapsedResult

      -- If 'elapsedResult' is a 'Just', that indicates that last_updated went backwards.
      -- In that case, we should NOT write the result to the queue, and instead extend the
      -- time until our next poll by however long the time went backwards.
      when (isNothing elapsedResult) (liftIO $ atomically $ writeTBQueue queue result)

-- * Station status handling.

instance Pollable (ResponseWrapper [AT.StationStatus]) where
  -- | Endpoint (ClientM a)
  request = stationStatus

  -- | Thread action to request station information from API.
  requester = requesterFn "Stn Status" request

  -- | Thread action to handle API response for station status query.
  handler queue = void $ do
    response <- liftIO $ atomically $ readTBQueue queue

    let status = response ^. respData
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

instance Pollable (ResponseWrapper [AT.StationInformation]) where
  -- | Endpoint (ClientM a)
  request = stationInformation

  -- | Thread action to request station information from API.
  requester = requesterFn "Stn Info" request

  -- | Thread action to handle API response for station information query.
  handler queue = void $ do
    response <- liftIO $ atomically $ readTBQueue queue

    let info = response ^. respData
    log I $ format "(Info) Received {} info records from API." (length info)

    -- Insert the station information (updating existing records, if existing).
    insertedResult <- insertStationInformation info

    -- Log each station ID updated.
    mapM_ (log D . Text.pack . fmtLog) insertedResult

    -- Log number of inserted rows.
    log I $ format "(Info) Updated/inserted {} records into database." (length insertedResult)

    where
      fmtLog inserted = format "ID: {}" (pShowCompact inserted)

-- * System information handling.

instance Pollable (ResponseWrapper AT.SystemInformation) where
  -- | Endpoint (ClientM a)
  request = systemInformation

  -- | Thread action to request station information from API.
  requester = requesterFn "Sys Info" request

  -- | Thread action to handle API response for station information query.
  handler queue = void $ do
    response <- liftIO $ atomically $ readTBQueue queue

    let (reported, info) = (response ^. respLastUpdated, response ^. respData)
    log I "(Sys Info) Received system info from API."

    -- Insert the station information (updating existing records, if existing).
    (_insertedInfo, _insertedInfoCount) <- insertSystemInformation reported info

    -- Log number of inserted rows.
    logInfo "(Sys Info) Updated/inserted system information into database."
