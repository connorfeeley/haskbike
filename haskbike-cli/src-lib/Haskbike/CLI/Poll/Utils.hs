{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions for polling the API.

module Haskbike.CLI.Poll.Utils
     ( handleResponseWrapper
     , persistQueryLog
     ) where

import           Colog

import           Control.Exception                      ( throw )
import           Control.Monad.Catch                    ( MonadCatch, MonadThrow )

import           Data.Aeson
import           Data.Maybe                             ( fromMaybe )
import qualified Data.Text                              as T
import           Data.Time
import           Data.Time.Extras

import           Haskbike.API.ResponseWrapper
import           Haskbike.AppEnv
import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Operations.QueryLogs ( insertQueryLog )
import           Haskbike.Database.Tables.QueryLogs

import           Servant.Client

import           UnliftIO


-- * Helper functions.

handleResponseWrapper :: (HasEnv env m, MonadIO m)
                      => EndpointQueried -> ResponseWrapper a -> Int -> m (Maybe Int)
handleResponseWrapper ep resp pLastUpdated = do
  logDebug (responseUpdatedLog ep pLastUpdated lastUpdated timeElapsed)
  if timeElapsed < 0
    then pure (Just (-timeElapsed))
    else pure Nothing
  -- Crappy API returned stale data.
  where lastUpdated = (utcToPosix . _respLastUpdated) resp
        -- Time elapsed since last poll; also amount to extend poll by when negative.
        timeElapsed = lastUpdated - pLastUpdated


responseUpdatedLog :: (Show a1, Show a2) => a1 -> Int -> Int -> a2 -> T.Text
responseUpdatedLog ep pLastUpdated lastUpdated timeElapsed = T.pack $
  epTxt <> lastUpdatedMessage <> logBraces pLastUpdatedTxt <> " -> " <> logBraces lastUpdatedTxt <> " | " <> timeElapsedTxt
  where
    epTxt = show ep
    lastUpdatedMessage = if lastUpdated - pLastUpdated < 0 then "last updated went backwards: " else " last updated: "
    logBraces txt = "[" <> txt <> "]"
    pLastUpdatedTxt = (show . posixToUtc) pLastUpdated
    lastUpdatedTxt  = (show . posixToUtc) lastUpdated
    timeElapsedTxt  = show timeElapsed <> "s"



-- * Functions for handling and inserting the appropriate query log records.

-- | Handle the response from the API and insert the appropriate query log record.
persistQueryLog :: (HasEnv env m, MonadIO m, MonadThrow m, MonadCatch m)
               => EndpointQueried -> Either ClientError UTCTime -> m [QueryLog]
persistQueryLog ep (Right timestamp) = insertQueryLog (QuerySuccess timestamp ep)
persistQueryLog ep (Left err) = do
  curTime <- liftIO getCurrentTime
  logException err
  insertQueryLog $ QueryFailure curTime ep (errTxt err) jsonForDecodeFailure
  pure $ throw err
  where
    errTxt = T.pack . errToQueryLog
    jsonForDecodeFailure = maybeDecodeFailure err

errToQueryLog :: ClientError -> String
errToQueryLog (FailureResponse req resp)              = "Failure response: "          <> show req <> " " <> show resp
errToQueryLog (DecodeFailure txt _resp)               = "Decode failure: "            <> show txt
errToQueryLog (UnsupportedContentType mediaType resp) = "Unsupported content type: "  <> show mediaType <> " " <> show resp
errToQueryLog (InvalidContentTypeHeader resp)         = "Invalid content type header" <> show resp
errToQueryLog (ConnectionError exep)                  = "Connection error: "          <> show exep

-- | Try to decode the response body as JSON, defaulting to 'Null' on failure.
maybeDecodeFailure :: ClientError -> Value
maybeDecodeFailure (DecodeFailure _ resp) = fromMaybe Null ((decode . responseBody) resp)
maybeDecodeFailure _                      = Null
