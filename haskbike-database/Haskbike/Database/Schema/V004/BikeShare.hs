-- | V004 Database schema for BikeShare.

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Haskbike.Database.Schema.V004.BikeShare
     ( BikeshareDb (..)
     , bikeshareDb
     , bikeshareQueryLog
     , bikeshareStationInformation
     , bikeshareStationLookup
     , bikeshareStationOccupancy
     , bikeshareStationStatus
     , bikeshareStationStatusChanges
     , bikeshareSystemInformation
     , bikeshareSystemInformationCount
     ) where

import           Control.Lens                                     ( Lens' )

import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Postgres.CustomTypes

import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Schema.V001.QueryLogs          as V001
import           Haskbike.Database.Schema.V001.StationInformation as V001
import           Haskbike.Database.Schema.V001.StationStatus      as V001
import           Haskbike.Database.Schema.V001.SystemInformation  as V001
import           Haskbike.Database.Schema.V002.StationLookup      as V002
import           Haskbike.Database.Schema.V004.StationOccupancy   as V004


data BikeshareDb f where
  BikeshareDb :: { _bikeshareEndpointQueriedType    :: f (PgType EndpointQueried)
                 -- ^ Custom Postgres enum type for the different endpoints that are queried.
                 , _bikeshareStationInformation     :: f (TableEntity V001.StationInformationT)
                 , _bikeshareStationStatus          :: f (TableEntity V001.StationStatusT)
                 , _bikeshareStationStatusChanges   :: f (TableEntity V001.StationStatusT)
                 , _bikeshareSystemInformation      :: f (TableEntity V001.SystemInformationT)
                 , _bikeshareSystemInformationCount :: f (TableEntity V001.SystemInformationCountT)
                 , _bikeshareQueryLog               :: f (TableEntity V001.QueryLogT)
                 , _bikeshareStationLookup          :: f (TableEntity V002.StationLookupT)
                 -- Add table to store a cache of station occupancy calculations.
                 , _bikeshareStationOccupancy       :: f (TableEntity V004.StationOccupancyT)
                 } -> BikeshareDb f
  deriving (Generic, Database Postgres)

-- | Description of the database.
bikeshareDb :: DatabaseSettings Postgres BikeshareDb
bikeshareDb = defaultDbSettings `withDbModification`
  dbModification
  { _bikeshareStationInformation     = V001.stationInformationModification
  , _bikeshareStationStatus          = V001.stationStatusModification "station_status"
  , _bikeshareStationStatusChanges   = V001.stationStatusModification "station_status_changes"
  , _bikeshareSystemInformation      = V001.systemInformationModification
  , _bikeshareSystemInformationCount = V001.systemInformationCountModification
  , _bikeshareQueryLog               = V001.queryLogModification
  , _bikeshareStationLookup          = V002.stnLookupModification
  -- Add table to store a cache of station occupancy calculations.
  , _bikeshareStationOccupancy       = V004.stnOccupancyModification
  }

-- * Lenses
-- NOTE: no lens for _bikeshareEndpointQueriedType.
bikeshareStationInformation     :: Lens' (BikeshareDb f) (f (TableEntity V001.StationInformationT))
bikeshareStationStatus          :: Lens' (BikeshareDb f) (f (TableEntity V001.StationStatusT))
bikeshareStationStatusChanges   :: Lens' (BikeshareDb f) (f (TableEntity V001.StationStatusT))
bikeshareSystemInformation      :: Lens' (BikeshareDb f) (f (TableEntity V001.SystemInformationT))
bikeshareSystemInformationCount :: Lens' (BikeshareDb f) (f (TableEntity V001.SystemInformationCountT))
bikeshareQueryLog               :: Lens' (BikeshareDb f) (f (TableEntity V001.QueryLogT))
bikeshareStationLookup          :: Lens' (BikeshareDb f) (f (TableEntity V002.StationLookupT))
bikeshareStationOccupancy       :: Lens' (BikeshareDb f) (f (TableEntity V004.StationOccupancyT))
BikeshareDb _ (TableLens bikeshareStationInformation)     _ _ _ _ _ _ _ = dbLenses
BikeshareDb _ _ (TableLens bikeshareStationStatus)          _ _ _ _ _ _ = dbLenses
BikeshareDb _ _ _ (TableLens bikeshareStationStatusChanges)   _ _ _ _ _ = dbLenses
BikeshareDb _ _ _ _ (TableLens bikeshareSystemInformation)      _ _ _ _ = dbLenses
BikeshareDb _ _ _ _ _ (TableLens bikeshareSystemInformationCount) _ _ _ = dbLenses
BikeshareDb _ _ _ _ _ _ (TableLens bikeshareQueryLog)               _ _ = dbLenses
BikeshareDb _ _ _ _ _ _ _ (TableLens bikeshareStationLookup)          _ = dbLenses
BikeshareDb _ _ _ _ _ _ _ _ (TableLens bikeshareStationOccupancy)       = dbLenses
