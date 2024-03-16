{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StarIsType             #-}
{-# LANGUAGE TypeFamilies           #-}

module API.APIEntity
     ( APIPersistable (..)
     , PollResult (..)
     ) where

import           API.ClientLifted
import           API.ResponseWrapper
import qualified API.StationInformation                       as AT
import qualified API.StationStatus                            as AT
import qualified API.SystemInformation                        as AT

import           AppEnv

import           CLI.Poll.Utils

import           Colog

import           Control.Lens

import           Data.Maybe                                   ( mapMaybe )
import qualified Data.Text                                    as T
import           Data.Time.Extras

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions     ( MonadBeamInsertReturning (runInsertReturningList) )
import           Database.Beam.Postgres
import qualified Database.BikeShare                           as DB
import           Database.BikeShare.EndpointQueried
import           Database.BikeShare.Operations                ( insertStationInformation, insertStationStatus )
import qualified Database.BikeShare.Tables.StationInformation as DB
import qualified Database.BikeShare.Tables.StationStatus      as DB
import qualified Database.BikeShare.Tables.SystemInformation  as DB

import           Servant.Client                               ( ClientError, ClientM )

import           System.Directory.Internal.Prelude            ( exitFailure )

import           TextShow                                     ( showt )

import           UnliftIO
import           UnliftIO.Concurrent


data PollResult where
  PollClientError :: ClientError -> PollResult
  WentBackwards   :: Int -> PollResult
  Success         :: (ResponseWrapper apiType, [dbType Identity]) -> PollResult

class APIPersistable apiType dbType | apiType -> dbType where
  fromAPI :: ResponseWrapper apiType -> [dbType (QExpr Postgres s)]
  fromAPI _   = []

  insertAPI :: ResponseWrapper apiType -> AppM [dbType Identity]
  insertAPI _ = pure []

  fetchAndPersist :: EndpointQueried
                  -> ClientM (ResponseWrapper apiType)
                  -- ^ The function to fetch data from the API.
                  -> Int
                  -- ^ Last updated field of previous successful query.
                  -> AppM PollResult
                  -- ^ Return either an error or inserted DB items.
  fetchAndPersist ep apiFetch lastUpdated =
    runQueryM apiFetch >>= processResponse ep lastUpdated

  pollThread :: EndpointQueried -> ClientM (ResponseWrapper apiType) -> TVar Int -> AppM ()
  pollThread ep apiFetch lastUpdatedVar = do
    lastUpdated <- liftIO $ readTVarIO lastUpdatedVar
    result <- fetchAndPersist ep apiFetch lastUpdated
    case result of
      PollClientError _err     -> liftIO delayAndExitFailure
      WentBackwards extendByMs -> liftIO (delaySecs extendByMs)
      Success (resp, inserted) -> do
        liftIO $ atomically $ writeTVar lastUpdatedVar (utcToPosix (_respLastUpdated resp) + timeToLiveS resp)
        logInfo $ "[" <> (T.pack . show) ep <> "] Inserted " <> (showt . length) inserted <> " records - sleeping for " <> showt (timeToLiveS resp) <> "s"
        -- Sleep for requisite TTL.
        liftIO $ threadDelay (timeToLiveS resp * msPerS)
    pure ()
      where timeToLiveS = _respTtl
            msPerS = 1000000
            delaySecs secs = threadDelay (secs * msPerS)
            delayAndExitFailure = delaySecs 10 >> exitFailure

-- | Process a (undecoded) response from the API.
processResponse :: APIPersistable apiType dbType => EndpointQueried -> Int -> Either ClientError (ResponseWrapper apiType) -> AppM PollResult
processResponse ep _ (Left err) =
  handleResponseError ep err >>
  pure (PollClientError err)
processResponse ep lastUpdated (Right respDecoded) =
  handleResponseWrapper ep respDecoded lastUpdated >>=
  processValidResponse ep respDecoded

-- | Process a (decoded) response from the API.
processValidResponse :: APIPersistable apiType dbType => EndpointQueried -> ResponseWrapper apiType -> Maybe Int -> AppM PollResult
processValidResponse _ _ (Just extendByMs) = pure (WentBackwards extendByMs) -- Went backwards - return early.
processValidResponse ep respDecoded Nothing = do -- Went forwards - handle response.
  let resp = respDecoded -- Response has now been vetted.
  handleResponseSuccess ep (_respLastUpdated resp) -- Insert query log.
  inserted <- insertAPI resp -- Insert response into database.
  pure (Success (resp, inserted))

-- * Instances.

instance APIPersistable [AT.StationInformation] DB.StationInformationT where
  fromAPI resp = mapMaybe (Just . DB.fromJSONToBeamStationInformation (_respLastUpdated resp)) (_respData resp)
  insertAPI resp = insertStationInformation (_respLastUpdated resp) (_respData resp)


instance APIPersistable [AT.StationStatus] DB.StationStatusT where
  fromAPI _resp = undefined
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
