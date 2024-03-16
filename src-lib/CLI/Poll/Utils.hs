{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions for polling the API.

module CLI.Poll.Utils
     ( errToQueryLog
     , handleResponse
     , handleResponseBackwards
     , handleResponseForwards
     , handleResponseWrapper
     , maybeDecodeFailure
     ) where

import           API.ResponseWrapper

import           AppEnv

import           Colog

import           Control.Exception                       ( throw )

import           Data.Aeson
import           Data.Maybe                              ( fromMaybe )
import qualified Data.Text                               as T
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.EndpointQueried
import           Database.BikeShare.Operations.QueryLogs ( insertQueryLog )
import           Database.BikeShare.Tables.QueryLogs

import           Servant.Client

import           UnliftIO


-- * Helper functions.

handleResponseWrapper :: EndpointQueried -> ResponseWrapper a -> Int -> AppM (Maybe Int)
handleResponseWrapper ep resp pLastUpdated = do
  if timeElapsed < 0 -- Crappy API returned stale data.
    then do
      extendMs <- handleResponseBackwards ep resp pLastUpdated lastUpdated timeElapsed
      pure (Just extendMs)
  else do
    handleResponseForwards ep resp pLastUpdated lastUpdated timeElapsed >> pure Nothing
  where lastUpdated = (utcToPosix . _respLastUpdated) resp
        -- Time elapsed since last poll; also amount to extend poll by when negative.
        timeElapsed = lastUpdated - pLastUpdated

handleResponseBackwards :: EndpointQueried -> ResponseWrapper a -> Int -> Int -> Int -> AppM Int
handleResponseBackwards ep _resp pLastUpdated lastUpdated timeElapsed = do
  logDebug $ (T.pack . show) ep <> " last updated went backwards: [" <> (T.pack . show . posixToUtc) pLastUpdated <> "] -> [" <> (T.pack . show) (posixToUtc lastUpdated) <> "] | " <> (T.pack . show) timeElapsed <> "s"
  pure (-timeElapsed)

handleResponseForwards :: EndpointQueried -> ResponseWrapper a -> Int -> Int -> Int -> AppM Int
handleResponseForwards ep _resp pLastUpdated lastUpdated timeElapsed = do
  logDebug $ "(" <> (T.pack . show) ep <> ") last updated: [" <> (T.pack . show) (posixToUtc pLastUpdated) <> "] -> [" <> (T.pack . show . posixToUtc) lastUpdated <> "] | " <> (T.pack . show) timeElapsed <> "s"
  pure (-timeElapsed)


-- * Functions for handling and inserting the appropriate query log records.

-- | Handle the response from the API and insert the appropriate query log record.
handleResponse :: EndpointQueried -> Either ClientError UTCTime -> AppM [QueryLog]
handleResponse ep (Right timestamp) = insertQueryLog (QuerySuccess timestamp ep)
handleResponse ep (Left err) = do
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
