{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Table to store station occupancy calculations.

module Haskbike.Database.Schema.V004.StationOccupancy
     ( PrimaryKey (..)
     , StationOccupancy
     , StationOccupancyId
     , StationOccupancyT (..)
     , createStationOccupancy
     , extraOccupancyMigrations
     , stnOccCalculated
     , stnOccEmptySec
     , stnOccEmptyThresh
     , stnOccFullSec
     , stnOccFullThresh
     , stnOccInfo
     , stnOccRangeEnd
     , stnOccRangeStart
     , stnOccupancyModification
     ) where

import           Control.Lens

import           Data.Int                                         ( Int32 )
import           Data.String                                      ( IsString )
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend                            ( IsSql92DataTypeSyntax (..) )
import           Database.Beam.Migrate
import           Database.Beam.Postgres

import           Haskbike.Database.Schema.V001.StationInformation as V001


data StationOccupancyT f where
  StationOccupancy :: { _stnOccInfo        :: PrimaryKey V001.StationInformationT f
                      , _stnOccCalculated  :: C f UTCTime -- ^ The time the calculation was done.
                      , _stnOccRangeStart  :: C f UTCTime -- ^ The start time of the occupancy calculation.
                      , _stnOccRangeEnd    :: C f UTCTime -- ^ The end time of the occupancy calculation.
                      , _stnOccEmptyThresh :: C f Int32   -- ^ The threshold (0 + x) for the station to be considered empty.
                      , _stnOccFullThresh  :: C f Int32   -- ^ The threshold (capacity - x) for the station to be considered full.
                      , _stnOccEmptySec    :: C f Int32   -- ^ The number of seconds the station was empty.
                      , _stnOccFullSec     :: C f Int32   -- ^ The number of seconds the station was full.
                      } -> StationOccupancyT f
  deriving (Generic, Beamable)

type StationOccupancy   = StationOccupancyT Identity
type StationOccupancyId = PrimaryKey StationOccupancyT Identity
deriving instance Show StationOccupancyId
deriving instance Show StationOccupancy

instance Table StationOccupancyT where
  data PrimaryKey StationOccupancyT f =
    StationOccupancyId { _unStnOccInfo        :: PrimaryKey StationInformationT f
                       , _unStnOccCalculated  :: C f UTCTime
                       , _unStnOccRangeStart  :: C f UTCTime
                       , _unStnOccRangeEnd    :: C f UTCTime
                       , _unStnOccEmptyThresh :: C f Int32
                       , _unStnOccFullThresh  :: C f Int32
                       }
    deriving (Generic, Beamable)
  primaryKey = StationOccupancyId <$> _stnOccInfo
                                  <*> _stnOccCalculated
                                  <*> _stnOccRangeStart
                                  <*> _stnOccRangeEnd
                                  <*> _stnOccEmptyThresh
                                  <*> _stnOccFullThresh

-- * 'StationOccupancy' Lenses
stnOccInfo        :: Getter (StationOccupancyT Identity) (PrimaryKey V001.StationInformationT Identity)
stnOccCalculated  :: Lens' (StationOccupancyT f) (C f UTCTime)
stnOccRangeStart  :: Lens' (StationOccupancyT f) (C f UTCTime)
stnOccRangeEnd    :: Lens' (StationOccupancyT f) (C f UTCTime)
stnOccEmptyThresh :: Lens' (StationOccupancyT f) (C f Int32)
stnOccFullThresh  :: Lens' (StationOccupancyT f) (C f Int32)
stnOccEmptySec    :: Lens' (StationOccupancyT f) (C f Int32)
stnOccFullSec     :: Lens' (StationOccupancyT f) (C f Int32)
stnOccInfo = to _stnOccInfo
StationOccupancy _ (LensFor stnOccCalculated)  _ _ _ _ _ _ = tableLenses
StationOccupancy _ _ (LensFor stnOccRangeStart)  _ _ _ _ _ = tableLenses
StationOccupancy _ _ _ (LensFor stnOccRangeEnd)    _ _ _ _ = tableLenses
StationOccupancy _ _ _ _ (LensFor stnOccEmptyThresh) _ _ _ = tableLenses
StationOccupancy _ _ _ _ _ (LensFor stnOccFullThresh)  _ _ = tableLenses
StationOccupancy _ _ _ _ _ _ (LensFor stnOccEmptySec)    _ = tableLenses
StationOccupancy _ _ _ _ _ _ _ (LensFor stnOccFullSec)     = tableLenses


-- * Table modifications and migrations.

-- | Table modifications for 'StationOccupancy' table.
stnOccupancyModification :: EntityModification (DatabaseEntity be db) be (TableEntity StationOccupancyT)
stnOccupancyModification =
  setEntityName "station_occupancy" <> modifyTableFields tableModification
  { _stnOccInfo = V001.StationInformationId "info_station_id" "info_reported"
  }

-- | Migration for the StationOccupancy table.
createStationOccupancy :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity StationOccupancyT))
createStationOccupancy =
  createTable "station_occupancy" $ StationOccupancy
  { _stnOccInfo = V001.StationInformationId (field "info_station_id" int notNull)
                                            (field "info_reported" (DataType (timestampType Nothing True)) notNull)
  , _stnOccCalculated  = field "calculated"   (DataType (timestampType Nothing True)) notNull
  , _stnOccRangeStart  = field "range_start"  (DataType (timestampType Nothing True)) notNull
  , _stnOccRangeEnd    = field "range_end"    (DataType (timestampType Nothing True)) notNull
  , _stnOccEmptyThresh = field "empty_thresh" int notNull
  , _stnOccFullThresh  = field "full_thresh"  int notNull
  , _stnOccEmptySec    = field "empty_sec"    int notNull
  , _stnOccFullSec     = field "full_sec"     int notNull
  }


extraOccupancyMigrations :: IsString a => [a]
extraOccupancyMigrations = ["ALTER TABLE public.station_occupancy ADD CONSTRAINT IF NOT EXISTS fk_station_occupancy FOREIGN KEY (info_station_id, info_reported) REFERENCES public.station_information (station_id, reported) ON UPDATE CASCADE"]
