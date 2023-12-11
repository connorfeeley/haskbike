{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions for polling the API.

module CLI.Poll.Utils
     ( createHandlingThread
     , createPollingThread
     , errToQueryLog
     , handleResponseError
     , handleResponseSuccess
     , handleTTL
     , handleTimeElapsed
     , maybeDecodeFailure
     , requesterFn
     ) where

import           API.ClientLifted                    ( runQueryM )
import           API.Pollable
import           API.ResponseWrapper

import           AppEnv

import           Colog

import           Control.Concurrent
import           Control.Exception                   ( throw )
import           Control.Lens
import           Control.Monad

import           Data.Aeson
import           Data.Maybe                          ( fromMaybe, isJust, isNothing )
import qualified Data.Text                           as T
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.EndpointQueried
import           Database.BikeShare.Operations       ( insertQueryLog )
import           Database.BikeShare.Tables.QueryLogs

import           Fmt                                 ( format )

import           Servant.Client

import           TextShow                            ( showt )

import           UnliftIO


-- Create async actions for both requester and handler threads.
createPollingThread :: Pollable a => TBQueue a -> TVar Int -> TVar Int -> AppM (Async b)
createPollingThread queue ttlVar lastUpdatedVar = (async . forever) (requester queue ttlVar lastUpdatedVar)

createHandlingThread :: Pollable a => TBQueue a -> AppM (Async b)
createHandlingThread queue = async . forever $ handler queue

-- * TTL and timing handlers.

-- | Handle last_updated field in response.
handleTimeElapsed :: T.Text            -- ^ Log prefix
                  -> ResponseWrapper a -- ^ TTL from API response.
                  -> TVar Int          -- ^ TVar holding last_updated between threads.
                  -> AppM (Maybe Int)  -- ^ Optional number of seconds extend TTL by, if last_updated went backwards.
handleTimeElapsed logPrefix apiResult lastUpdatedVar = do
  let currentTime' = apiResult ^. respLastUpdated
  previousTime <- liftIO $ readTVarIO lastUpdatedVar
  let previousTime' = posixToLocal previousTime
  let timeElapsed = utcToPosix currentTime' - previousTime

  -- Check if last_updated went backwards
  if timeElapsed >= 0
    then do -- Update last_updated variable.
      liftIO $ atomically $ writeTVar lastUpdatedVar (utcToPosix currentTime')
      logInfo $ format "({}) last updated [{}]" logPrefix currentTime'
      pure Nothing
    else do
      logWarning $ format "({}) last updated went backwards: [{}] -> [{}] | ({})" logPrefix previousTime' currentTime' timeElapsed
      pure (Just (-timeElapsed))


-- | Handle last_updated field in response.
handleTTL :: Pollable (ResponseWrapper a)
          => T.Text            -- ^ Log prefix
          -> ResponseWrapper a -- ^ TTL from API response.
          -> TVar Int          -- ^ TVar holding TTL.
          -> Maybe Int         -- ^ Optional number of seconds to extend TTL by.
          -> AppM ()
handleTTL logPrefix apiResult ttlVar extendBy = do
  let ttlSecs = apiResult ^. respTtl

  when (isJust extendBy) $ logWarning $ format "({}) extending TTL by {}s" logPrefix (fromMaybe 0 extendBy)
  logInfo $ format "({}) TTL={}{}" logPrefix (showt ttlSecs) (maybe "" (("+" ++) . show) extendBy)

  liftIO $ atomically $ writeTVar ttlVar (ttlSecs + fromMaybe 0 extendBy)

requesterFn :: Pollable (ResponseWrapper a)
            => EndpointQueried
            -> T.Text
            -> ClientM (ResponseWrapper a)
            -> TBQueue (ResponseWrapper a)
            -> TVar Int
            -> TVar Int
            -> AppM ()
requesterFn ep prefix query queue intervalSecsVar lastUpdated = void $ do
  runQueryM query >>= \case
    Left err -> handleResponseError ep err
    Right result -> do
      handleResponseSuccess ep (result ^. respLastUpdated)

      -- Handle TTL upfront, so that we don't sleep when called
      -- for the first time from 'pollClient' (TTL = 0).
      intervalSecs <- liftIO $ readTVarIO intervalSecsVar
      logDebug $ format "({}) Sleeping for {} seconds." prefix intervalSecs
      liftIO $ threadDelay (intervalSecs * 1000000)

      elapsedResult <- handleTimeElapsed prefix result lastUpdated
      handleTTL prefix result intervalSecsVar elapsedResult

      -- If 'elapsedResult' is a 'Just', that indicates that last_updated went backwards.
      -- In that case, we should NOT write the result to the queue, and instead extend the
      -- time until our next poll by however long the time went backwards.
      when (isNothing elapsedResult) (liftIO $ atomically $ writeTBQueue queue result)


-- * Functions for handling and inserting the appropriate query log records.

handleResponseSuccess :: EndpointQueried -> UTCTime -> AppM [QueryLog]
handleResponseSuccess ep timestamp = do
  insertQueryLog query
  where query = QuerySuccess timestamp ep

handleResponseError :: EndpointQueried -> ClientError -> AppM ()
handleResponseError ep err = do
  curTime <- liftIO getCurrentTime
  logException err
  insertQueryLog $ queryFailure curTime
  pure $ throw err
  where
    queryFailure t = QueryFailure t ep (errTxt err) jsonForDecodeFailure
    errTxt = T.pack . errToQueryLog
    jsonForDecodeFailure = maybeDecodeFailure err

errToQueryLog :: ClientError -> String
errToQueryLog (FailureResponse req resp)              = "Failure response: "          <> show req <> " " <> show resp
errToQueryLog (DecodeFailure txt resp)                = "Decode failure: "            <> show txt <> " " <> show resp
errToQueryLog (UnsupportedContentType mediaType resp) = "Unsupported content type: "  <> show mediaType <> " " <> show resp
errToQueryLog (InvalidContentTypeHeader resp)         = "Invalid content type header" <> show resp
errToQueryLog (ConnectionError exep)                  = "Connection error: "          <> show exep

-- | Try to decode the response body as JSON, defaulting to 'Null' on failure.
maybeDecodeFailure :: ClientError -> Value
maybeDecodeFailure (DecodeFailure _ resp) = fromMaybe Null ((decode . responseBody) resp)
maybeDecodeFailure _                      = Null
