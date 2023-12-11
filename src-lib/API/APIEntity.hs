{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module API.APIEntity where

import           API.Client
import           API.ClientLifted
import           API.ResponseWrapper
import qualified API.StationInformation                       as AT
import qualified API.StationStatus                            as AT
import qualified API.SystemInformation                        as AT

import           AppEnv

import           Control.Applicative                          ( liftA2 )
import           Control.Lens

import           Data.Maybe                                   ( mapMaybe )
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions     ( MonadBeamInsertReturning (runInsertReturningList) )
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Full                  hiding ( insert )
import qualified Database.BikeShare                           as DB
import qualified Database.BikeShare.Tables.StationInformation as DB
import qualified Database.BikeShare.Tables.StationStatus      as DB
import qualified Database.BikeShare.Tables.SystemInformation  as DB

import           Servant.Client                               ( ClientError, ClientM )


-- Typeclass for fetching data from the API.
class Monad m => ApiFetcher m apiType where
  fetchFromApi :: m (Either ClientError (ResponseWrapper apiType))

class ApiConverter apiType interType where
  prepareData :: ResponseWrapper apiType -> interType

-- Typeclass for converting from API types to database types.
class ToDbEntity apiType dbType where
  toDbEntity :: apiType -> dbType

-- Typeclass for inserting entities into the database.
class Monad m => DbInserter m apiType dbType dbMonad where
  dbInsert :: HasTable dbType => [apiType] -> m [dbType dbMonad]

class HasTable dbType where
  getTable :: DB.BikeshareDb f -> f (TableEntity dbType)


-- * StationInformation instances.

instance ApiFetcher AppM [AT.StationInformation] where
  fetchFromApi = runQueryM stationInformation

instance ApiConverter [AT.StationInformation] [AT.StationInformation] where
  prepareData = _respData

instance ToDbEntity AT.StationInformation (DB.StationInformationT (QExpr Postgres s)) where
  toDbEntity = DB.fromJSONToBeamStationInformation

instance DbInserter AppM AT.StationInformation DB.StationInformationT Identity where
  dbInsert stations = withPostgres $ runInsertReturningList $ insertOnConflict (getTable DB.bikeshareDb)
    (insertExpressions (map toDbEntity stations))
    (conflictingFields primaryKey) (onConflictUpdateInstead (\i -> ( DB._infoName                    i
                                                                   , DB._infoPhysicalConfiguration   i
                                                                   , DB._infoCapacity                i
                                                                   , DB._infoIsChargingStation       i
                                                                   , DB._infoIsValetStation          i
                                                                   , DB._infoIsVirtualStation        i
                                                                   )
                                                            ))

instance HasTable DB.StationInformationT where
  getTable = DB._bikeshareStationInformation


-- * StationStatus instances.

instance Monad ClientM => ApiFetcher AppM [AT.StationStatus] where
  fetchFromApi = runQueryM stationStatus

instance ApiConverter [AT.StationStatus] [AT.StationStatus] where
  prepareData = _respData

instance ToDbEntity AT.StationStatus (Maybe (DB.StationStatusT (QExpr Postgres s))) where
  toDbEntity = DB.fromJSONToBeamStationStatus

instance DbInserter AppM AT.StationStatus DB.StationStatusT Identity where
  dbInsert stations = withPostgres $ runInsertReturningList $ insertOnConflict (getTable DB.bikeshareDb)
    (insertExpressions (mapMaybe toDbEntity stations))
    anyConflict onConflictDoNothing

instance HasTable DB.StationStatusT where
  getTable = DB._bikeshareStationStatus


-- * SystemInformation instances.

instance Monad ClientM => ApiFetcher AppM AT.SystemInformation where
  fetchFromApi = runQueryM systemInformation

instance ApiConverter AT.SystemInformation (UTCTime, AT.SystemInformation) where
  prepareData = liftA2 (,) _respLastUpdated _respData

instance ToDbEntity (UTCTime, AT.SystemInformation) (DB.SystemInformationT (QExpr Postgres s)) where
  toDbEntity = uncurry DB.fromJSONToBeamSystemInformation

instance ToDbEntity (UTCTime, AT.SystemInformation) (DB.SystemInformationCountT (QExpr Postgres s)) where
  toDbEntity = uncurry DB.fromJSONToBeamSystemInformationCount

instance DbInserter AppM (UTCTime, AT.SystemInformation) DB.SystemInformationT Identity where
  dbInsert inf = withPostgres $ runInsertReturningList $ insert (getTable DB.bikeshareDb)
    (insertExpressions (map toDbEntity inf))

instance DbInserter AppM (UTCTime, AT.SystemInformation) DB.SystemInformationCountT Identity where
  dbInsert inf = withPostgres $ runInsertReturningList $ insert (getTable DB.bikeshareDb)
    (insertExpressions (map toDbEntity inf))

instance HasTable DB.SystemInformationT where
  getTable = DB._bikeshareSystemInformation

instance HasTable DB.SystemInformationCountT where
  getTable = DB._bikeshareSystemInformationCount
