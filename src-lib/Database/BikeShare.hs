-- | Database schema for BikeShare.

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Database.BikeShare
     ( BikeshareDb (..)
     , module Database.BikeShare.Types
     , bikeshareDb
       -- , bikeshareDiagnostics
     , bikeshareQueryLog
     , bikeshareStationInformation
     , bikeshareStationStatus
     , bikeshareSystemInformation
     , bikeshareSystemInformationCount
     ) where

import           Control.Lens                       ( Lens' )

import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Postgres.CustomTypes
import           Database.BikeShare.Types


data BikeshareDb f where
  BikeshareDb :: { _bikeshareEndpointQueriedType    :: f (PgType EndpointQueried)
                 -- ^ Custom Postgres enum type for the different endpoints that are queried.
                 , _bikeshareStationInformation     :: f (TableEntity StationInformationT)
                 , _bikeshareStationStatus          :: f (TableEntity StationStatusT)
                 , _bikeshareSystemInformation      :: f (TableEntity SystemInformationT)
                 , _bikeshareSystemInformationCount :: f (TableEntity SystemInformationCountT)
                 , _bikeshareQueryLog               :: f (TableEntity QueryLogT)
                 } -> BikeshareDb f
  deriving (Generic, Database Postgres)

-- | Description of the database.
bikeshareDb :: DatabaseSettings Postgres BikeshareDb
bikeshareDb = defaultDbSettings `withDbModification`
  dbModification
  { _bikeshareStationInformation = stationInformationModification
  , _bikeshareStationStatus = stationStatusModification
  , _bikeshareSystemInformation = systemInformationModification
  , _bikeshareSystemInformationCount = systemInformationCountModification
  , _bikeshareQueryLog = queryLogModification
  }

-- * Lenses
-- NOTE: no lens for _bikeshareEndpointQueriedType.
bikeshareStationInformation     :: Lens' (BikeshareDb f) (f (TableEntity StationInformationT))
bikeshareStationStatus          :: Lens' (BikeshareDb f) (f (TableEntity StationStatusT))
bikeshareSystemInformation      :: Lens' (BikeshareDb f) (f (TableEntity SystemInformationT))
bikeshareSystemInformationCount :: Lens' (BikeshareDb f) (f (TableEntity SystemInformationCountT))
bikeshareQueryLog               :: Lens' (BikeshareDb f) (f (TableEntity QueryLogT))
BikeshareDb _ (TableLens bikeshareStationInformation) _ _ _ _     = dbLenses
BikeshareDb _ _ (TableLens bikeshareStationStatus) _ _ _          = dbLenses
BikeshareDb _ _ _ (TableLens bikeshareSystemInformation) _ _      = dbLenses
BikeshareDb _ _ _ _ (TableLens bikeshareSystemInformationCount) _ = dbLenses
BikeshareDb _ _ _ _ _ (TableLens bikeshareQueryLog)               = dbLenses
