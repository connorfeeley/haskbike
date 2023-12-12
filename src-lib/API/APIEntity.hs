{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StarIsType             #-}
{-# LANGUAGE TypeFamilies           #-}

module API.APIEntity where

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
import           Data.Time.Extras

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions     ( MonadBeamInsertReturning (runInsertReturningList) )
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Full                  hiding ( insert )
import qualified Database.BikeShare                           as DB
import           Database.BikeShare.EndpointQueried
import qualified Database.BikeShare.Tables.StationInformation as DB
import qualified Database.BikeShare.Tables.StationStatus      as DB
import qualified Database.BikeShare.Tables.SystemInformation  as DB

import           Fmt

import           Servant.Client                               ( ClientError, ClientM )

import           System.Directory.Internal.Prelude            ( exitFailure )

import           UnliftIO
import           UnliftIO.Concurrent


data PollResult where
  PollClientError :: ClientError -> PollResult
  WentBackwards   :: Int -> PollResult
  Success         :: (ResponseWrapper apiType, [dbType Identity]) -> PollResult

class APIPersistable apiType dbType | apiType -> dbType where
  fromAPI :: ResponseWrapper apiType -> [dbType (QExpr Postgres s)]

  insertAPI :: ResponseWrapper apiType -> AppM [dbType Identity]

  fetchAndPersist :: EndpointQueried
                  -> ClientM (ResponseWrapper apiType)
                  -- ^ The function to fetch data from the API.
                  -> Int
                  -- ^ Last updated field of previous successful query.
                  -> AppM PollResult
                  -- ^ Return either an error or inserted DB items.
  pollThread :: EndpointQueried -> ClientM (ResponseWrapper apiType) -> TVar Int -> AppM ()

  -- Provide default implementations that can be overridden if needed.
  fromAPI _   = []

  insertAPI _ = pure []

  fetchAndPersist ep apiFetch lastUpdated =
    runQueryM apiFetch >>= \case
      -- Handle client errors.
      Left err -> do handleResponseError ep err >> pure (PollClientError err)

      -- Retrieved response successfully.
      Right respUnchecked -> do
        handleResponseWrapper ep respUnchecked lastUpdated >>= \case
          Just extendByMs -> pure (WentBackwards extendByMs) -- Went backwards - return early.
          Nothing -> do -- Went forwards - handle response.
            let resp = respUnchecked -- Response has now been vetted.
            handleResponseSuccess ep (_respLastUpdated resp) -- Insert query log.
            inserted <- insertAPI resp -- Insert response into database.
            pure (Success (resp, inserted))


  pollThread ep apiFetch lastUpdatedVar = do
    lastUpdated <- liftIO $ readTVarIO lastUpdatedVar
    result <- fetchAndPersist ep apiFetch lastUpdated
    case result of
      PollClientError _err     -> liftIO delayAndExitFailure
      WentBackwards extendByMs -> liftIO (delaySecs extendByMs)
      Success (resp, inserted) -> do
        liftIO $ atomically $ writeTVar lastUpdatedVar (utcToPosix (_respLastUpdated resp) + timeToLiveS resp)
        logInfo (format "[{}] Inserted {} records - sleeping for {}s" (show ep) (length inserted) (timeToLiveS resp))
        -- Sleep for requisite TTL.
        liftIO $ threadDelay (timeToLiveS resp * msPerS)
    pure ()
      where timeToLiveS = _respTtl
            msPerS = 1000000
            delaySecs secs = threadDelay (secs * msPerS)
            delayAndExitFailure = delaySecs 10 >> exitFailure

-- * Instances.

instance APIPersistable [AT.StationInformation] DB.StationInformationT where
  fromAPI resp = mapMaybe (Just . DB.fromJSONToBeamStationInformation) (_respData resp)
  insertAPI resp = withPostgres $ runInsertReturningList $ insertOnConflict (DB.bikeshareDb ^. DB.bikeshareStationInformation)
      (insertExpressions (fromAPI resp))
      (conflictingFields primaryKey) (onConflictUpdateInstead (\i -> ( DB._infoName                    i
                                                                     , DB._infoPhysicalConfiguration   i
                                                                     , DB._infoCapacity                i
                                                                     , DB._infoIsChargingStation       i
                                                                     , DB._infoIsValetStation          i
                                                                     , DB._infoIsVirtualStation        i
                                                                     )
                                                              ))


instance APIPersistable [AT.StationStatus] DB.StationStatusT where
  fromAPI resp = mapMaybe DB.fromJSONToBeamStationStatus (_respData resp)
  insertAPI resp = withPostgres $ runInsertReturningList $ insertOnConflict (DB.bikeshareDb ^. DB.bikeshareStationStatus)
    (insertExpressions (fromAPI resp))
    anyConflict onConflictDoNothing


instance APIPersistable AT.SystemInformation DB.SystemInformationCountT where
  fromAPI resp = [DB.fromJSONToBeamSystemInformationCount (_respLastUpdated resp) (_respData resp)]

  insertAPI resp = withPostgres $ do
    infCnt <- runInsertReturningList $ insert (DB.bikeshareDb ^. DB.bikeshareSystemInformationCount)
      (insertExpressions (fromAPI resp))
    -- SystemInformationCount needs special handling, since it doesn't fit the typeclass's structure.
    _inf   <- runInsertReturningList $ insert (DB.bikeshareDb ^. DB.bikeshareSystemInformation)
      (insertExpressions [DB.fromJSONToBeamSystemInformation (_respLastUpdated resp) (_respData resp)])
    pure infCnt
