{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Table to lookup latest station status records.

module Haskbike.Database.Schema.V002.StationLookup
     ( PrimaryKey (..)
     , StationLookup
     , StationLookupId
     , StationLookupT (..)
     , createStationLookup
     , extraLookupMigrations
     , stnLookup
     , stnLookupModification
     ) where

import           Control.Lens

import           Data.String                                       ( IsString )

import           Database.Beam
import           Database.Beam.Backend                             ( IsSql92DataTypeSyntax (..) )
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Haskbike.Database.Schema.V001.StationInformation as V001
import           Haskbike.Database.Schema.V001.StationStatus      as V001


data StationLookupT f where
  StationLookup :: { _stnLookup :: PrimaryKey V001.StationStatusT f
                   } -> StationLookupT f
  deriving (Generic, Beamable)

type StationLookup   = StationLookupT Identity
type StationLookupId = PrimaryKey StationLookupT Identity
deriving instance Show StationLookupId
deriving instance Show StationLookup

instance Table StationLookupT where
  data PrimaryKey StationLookupT f =
    StationLookupId { _unStnLookup :: PrimaryKey StationStatusT f
                    }
    deriving (Generic, Beamable)
  primaryKey = StationLookupId <$> _stnLookup

-- | StationLookup Lenses
stnLookup  :: Getter (StationLookupT Identity) (PrimaryKey V001.StationStatusT Identity)
stnLookup = to _stnLookup

-- * Table modifications and migrations.

-- | Table modifications for 'StationStatus' table.
stnLookupModification :: EntityModification (DatabaseEntity be db) be (TableEntity StationLookupT)
stnLookupModification =
  setEntityName "station_lookup" <> modifyTableFields tableModification
  { _stnLookup = V001.StationStatusId (StationInformationId "info_station_id" "info_reported") "status_last_reported"
  }

-- | Migration for the StationStatus table.
createStationLookup :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity StationLookupT))
createStationLookup =
  createTable "station_lookup" $ StationLookup
  { _stnLookup                = V001.StationStatusId ( V001.StationInformationId
                                                      (field "info_station_id" int notNull unique)
                                                      (field "info_reported" (DataType (timestampType Nothing True)) notNull)
                                                     )
                                (field "status_last_reported" (DataType (timestampType Nothing True)) notNull)
  }


extraLookupMigrations :: IsString a => [a]
extraLookupMigrations = ["ALTER TABLE public.station_lookup ADD CONSTRAINT fk_station_lookup FOREIGN KEY (info_station_id, info_reported, status_last_reported) REFERENCES public.station_status (info_station_id, info_reported, last_reported) ON UPDATE CASCADE"]
