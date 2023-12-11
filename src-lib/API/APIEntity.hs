{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module API.APIEntity where

import           API.Client
import           API.ResponseWrapper
import qualified API.StationInformation                       as AT
import qualified API.StationStatus                            as AT
import qualified API.SystemInformation                        as AT

import           AppEnv

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

import           Servant.Client                               ( ClientM )


-- Typeclass for fetching data from the API.
class ApiFetcher a where
  apiFetch :: ClientM (ResponseWrapper a)

-- Typeclass for converting from API types to database types.
class ToDbEntity apiType dbType where
  toDbEntity :: apiType -> dbType

-- Typeclass for converting from database types to API types.
class FromDbEntity dbType apiType where
  fromDbEntity :: dbType -> apiType

-- Typeclass for inserting entities into the database.
class Monad m => DbInserter m apiType dbType dbMonad where
  dbInsert :: HasTable dbType => [apiType] -> m [dbType dbMonad]

class HasTable dbType where
  getTable :: DB.BikeshareDb f -> f (TableEntity dbType)


-- * StationInformation instances.

instance Monad ClientM => ApiFetcher [AT.StationInformation] where
  apiFetch = stationInformation

instance ToDbEntity AT.StationInformation (DB.StationInformationT (QExpr Postgres s)) where
  toDbEntity = DB.fromJSONToBeamStationInformation

instance DbInserter AppM AT.StationInformation DB.StationInformationT Identity where
  dbInsert stations =
    withPostgres $ runInsertReturningList $
      insertOnConflict (getTable DB.bikeshareDb)
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

instance Monad ClientM => ApiFetcher [AT.StationStatus] where
  apiFetch = stationStatus

instance ToDbEntity AT.StationStatus (Maybe (DB.StationStatusT (QExpr Postgres s))) where
  toDbEntity = DB.fromJSONToBeamStationStatus

instance DbInserter AppM AT.StationStatus DB.StationStatusT Identity where
  dbInsert stations =
    withPostgres $ runInsertReturningList $
      insertOnConflict (getTable DB.bikeshareDb)
      (insertExpressions (mapMaybe toDbEntity stations))
      anyConflict onConflictDoNothing

instance HasTable DB.StationStatusT where
  getTable = DB._bikeshareStationStatus


-- * SystemInformation instances.

instance Monad ClientM => ApiFetcher AT.SystemInformation where
  apiFetch = systemInformation

instance ToDbEntity (UTCTime, AT.SystemInformation) (DB.SystemInformationT (QExpr Postgres s)) where
  toDbEntity = uncurry DB.fromJSONToBeamSystemInformation

instance ToDbEntity (UTCTime, AT.SystemInformation) (DB.SystemInformationCountT (QExpr Postgres s)) where
  toDbEntity = uncurry DB.fromJSONToBeamSystemInformationCount

instance DbInserter AppM (UTCTime, AT.SystemInformation) DB.SystemInformationT Identity where
  dbInsert inf =
    withPostgres $ runInsertReturningList $
      insert (getTable DB.bikeshareDb)
      (insertExpressions (map toDbEntity inf))

instance DbInserter AppM (UTCTime, AT.SystemInformation) DB.SystemInformationCountT Identity where
  dbInsert inf =
    withPostgres $ runInsertReturningList $
      insert (getTable DB.bikeshareDb)
      (insertExpressions (map toDbEntity inf))

instance HasTable DB.SystemInformationT where
  getTable = DB._bikeshareSystemInformation

instance HasTable DB.SystemInformationCountT where
  getTable = DB._bikeshareSystemInformationCount
