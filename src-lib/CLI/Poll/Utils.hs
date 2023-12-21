{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions for polling the API.

module CLI.Poll.Utils
     ( errToQueryLog
     , handleResponseBackwards
     , handleResponseError
     , handleResponseForwards
     , handleResponseSuccess
     , handleResponseWrapper
     , maybeDecodeFailure
     ) where

import           API.ResponseWrapper

import           AppEnv

import           Colog

import           Control.Exception                   ( throw )

import           Data.Aeson
import           Data.Maybe                          ( fromMaybe )
import qualified Data.Text                           as T
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.EndpointQueried
import           Database.BikeShare.Operations       ( insertQueryLog )
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
  -- logDebug ((T.pack . show) ep <> " last updated went backwards: [" <> (T.pack . show) posixToUtc pLastUpdated <> "] -> ["(T.pack . show) (posixToUtc lastUpdated) <> "] | " (T.pack . show) timeElapsed <> "s")
  pure (-timeElapsed)

handleResponseForwards :: EndpointQueried -> ResponseWrapper a -> Int -> Int -> Int -> AppM Int
handleResponseForwards ep _resp pLastUpdated lastUpdated timeElapsed = do
  logDebug errorLog
  pure (-timeElapsed)
  where errorLog = "(" <> (T.pack . show) ep <>") last updated: [" <> (T.pack . show) (posixToUtc pLastUpdated) <> "] -> [" <> (T.pack . show) (posixToUtc lastUpdated) <> "] | " <> (T.pack . show) timeElapsed <> "s"



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
