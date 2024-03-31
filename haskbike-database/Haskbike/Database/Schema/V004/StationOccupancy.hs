{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Table to store station occupancy calculations.

module Haskbike.Database.Schema.V004.StationOccupancy
     ( EmptyFull (..)
     , EmptyFullRecord (..)
     , PrimaryKey (..)
     , StationOccupancy
     , StationOccupancyId
     , StationOccupancyT (..)
     , createStationOccupancy
     , emptyFullFromSecs
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

import           Control.Lens                                     hiding ( (.=) )

import           Data.Aeson
import           Data.Int                                         ( Int32 )
import           Data.String                                      ( IsString, fromString )
import qualified Data.Text                                        as T
import           Data.Time

import           Database.Beam
import           Database.Beam.Backend                            ( IsSql92DataTypeSyntax (..) )
import           Database.Beam.Migrate
import           Database.Beam.Postgres

import           Haskbike.Database.Schema.V001.StationInformation as V001
import           Haskbike.Database.Tables.StationStatus



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
  { _stnOccInfo        = V001.StationInformationId "station_id" "reported"
  , _stnOccCalculated  = "calculated"
  , _stnOccRangeStart  = "range_start"
  , _stnOccRangeEnd    = "range_end"
  , _stnOccEmptyThresh = "empty_thresh"
  , _stnOccFullThresh  = "full_thresh"
  , _stnOccEmptySec    = "empty_sec"
  , _stnOccFullSec     = "full_sec"
  }

-- | Migration for the StationOccupancy table.
createStationOccupancy :: Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity StationOccupancyT))
createStationOccupancy =
  createTable "station_occupancy" $ StationOccupancy
  { _stnOccInfo = V001.StationInformationId (field "station_id" int notNull
                                            )
                                            (field "reported" (DataType (timestampType Nothing True)) notNull
                                            )
  , _stnOccCalculated  = field "calculated"   (DataType (timestampType Nothing True)) notNull
  , _stnOccRangeStart  = field "range_start"  (DataType (timestampType Nothing True)) notNull
  , _stnOccRangeEnd    = field "range_end"    (DataType (timestampType Nothing True)) notNull
  , _stnOccEmptyThresh = field "empty_thresh" int notNull
  , _stnOccFullThresh  = field "full_thresh"  int notNull
  , _stnOccEmptySec    = field "empty_sec"    int notNull
  , _stnOccFullSec     = field "full_sec"     int notNull
  }


extraOccupancyMigrations :: IsString a => [a]
extraOccupancyMigrations = [ extraOccupancyConstraint "station_occupancy" "station_information" "fk_station_occupancy" ["station_id", "reported"] ["station_id", "reported"]
                           ]

extraOccupancyConstraint :: IsString a => T.Text -> T.Text -> T.Text -> [T.Text] -> [T.Text] -> a
extraOccupancyConstraint tableName foreignTableName constraintName columns foreignColumns =
  fromString . T.unpack $
  "ALTER TABLE " <> qualifiedTable <> " ADD CONSTRAINT " <> constraintName <>
  " FOREIGN KEY (" <> columnsList <> ") REFERENCES " <>
  qualifiedForeignTable <> " (" <> foreignColumnsList <> ") ON UPDATE CASCADE"
  where
    qualifiedTable        = "public." <> tableName
    qualifiedForeignTable = "public." <> foreignTableName
    columnsList           = T.intercalate ", " columns
    foreignColumnsList    = T.intercalate ", " foreignColumns


-- * Associated utility types and functions.

data EmptyFull where
  EmptyFull :: { _emptyTime :: Maybe NominalDiffTime
               , _fullTime  :: Maybe NominalDiffTime
               } -> EmptyFull
  deriving (Generic, Show, Eq)

emptyFullFromSecs :: (Integral a1, Integral a2) => Maybe a2 -> Maybe a1 -> EmptyFull
emptyFullFromSecs empty full = EmptyFull emptyTime fullTime
  where
    emptyTime = secondsToNominalDiffTime . fromIntegral <$> empty
    fullTime  = secondsToNominalDiffTime . fromIntegral <$> full

instance ToJSON EmptyFull where
  toJSON record =
    object [ "empty" .= _emptyTime record
           , "full"  .= _fullTime  record
           ]

-- | Type for serializing to JSON for the station empty/full list.
data EmptyFullRecord where
  EmptyFullRecord :: { _emptyFullInformation :: StationInformation
                     , _emptyFullStatus      :: StationStatus
                     , _emptyFullDurations   :: EmptyFull
                     } -> EmptyFullRecord

instance ToJSON EmptyFullRecord where
  toJSON record =
    object [ "station_information" .= fromBeamStationInformationToJSON  (_emptyFullInformation record)
           , "station_status"      .= fromBeamStationStatusToJSON       (_emptyFullStatus   record)
           , "durations"           .= _emptyFullDurations   record
           ]
