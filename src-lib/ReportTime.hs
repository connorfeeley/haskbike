-- | This module defines the ReportTime datatype, which is a newtype wrapper for the last_reported field in the StationStatus table.

{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module ReportTime
     ( ReportTime (..)
     , reportTime
     , reportTimeType
     , reportTimeZone
     , reportToLocal
     , systemToReport
       -- Re-export general time functions which used to be in this module.
     , module Data.Time.Extras
       -- Re-exports for ReportTime constructors.
     , Day (..)
     , TimeOfDay (..)
     , fromGregorian
     ) where

import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Time.Extras

import           Database.Beam
import           Database.Beam.Backend                ( HasSqlValueSyntax (sqlValueSyntax),
                                                        IsSql92DataTypeSyntax (timestampType), SqlNull (SqlNull) )
import           Database.Beam.Migrate                ( HasDefaultSqlDataType )
import           Database.Beam.Postgres               ( Postgres )
import           Database.Beam.Postgres.Syntax        ( PgValueSyntax )
import           Database.PostgreSQL.Simple.FromField ( FromField (..) )
import           Database.PostgreSQL.Simple.ToField   ( ToField (..) )

-- | Smart constructor for the ReportTime datatype.
reportTime :: Day -> TimeOfDay -> ReportTime
reportTime day time_of_day = ReportTime $ LocalTime day time_of_day

-- | Newtype wrapper for the last_reported field, which is a POSIX timestamp in the JSON API.
newtype ReportTime where
  ReportTime :: LocalTime -> ReportTime
  deriving (Eq, Ord, Show, Read, FromField, ToField) via LocalTime
  deriving (HasSqlValueSyntax PgValueSyntax) via LocalTime
  deriving (FromBackendRow Postgres) via LocalTime
  deriving (HasSqlEqualityCheck Postgres) via LocalTime
  deriving (HasDefaultSqlDataType Postgres) via LocalTime
  deriving (HasSqlTime) via LocalTime
  deriving (HasSqlDate) via LocalTime

instance Num ReportTime where
    fromInteger i = ReportTime $ utcToLocalTime reportTimeZone . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral $ i
    abs           = error "BeamReportTime: abs not implemented"
    signum        = error "BeamReportTime: signum not implemented"
    negate        = error "BeamReportTime: negate not implemented"
    (+)           = error "BeamReportTime: (+) not implemented"
    (-)           = error "BeamReportTime: (-) not implemented"
    (*)           = error "BeamReportTime: (*) not implemented"

instance Real ReportTime where
  toRational (ReportTime t) = toRational . localToPosix $ t

instance Integral ReportTime => Enum ReportTime where
  toEnum = fromIntegral :: Int -> ReportTime
  fromEnum a = fromIntegral a :: Int

instance Enum ReportTime => Integral ReportTime where
  toInteger (ReportTime t) = fromIntegral $ localToPosix t
  quotRem a _ = (a, a)

instance (HasSqlValueSyntax ReportTime x, HasSqlValueSyntax ReportTime SqlNull) => HasSqlValueSyntax ReportTime (Maybe x) where
  sqlValueSyntax (Just x) = sqlValueSyntax x
  sqlValueSyntax Nothing  = sqlValueSyntax SqlNull

-- | Beam (migrate) datatype for the last_reported field in the StationStatus table.
reportTimeType :: DataType Postgres ReportTime
reportTimeType = DataType (timestampType Nothing True)


-- | TimeZone used to convert the API.T.StationStatus.last_reported field to a local time (effectively UTC).
reportTimeZone :: TimeZone
reportTimeZone = TimeZone 0 False "UTC"

-- | Convert ReportTime to LocalTime
reportToLocal :: TimeZone -> ReportTime -> LocalTime
reportToLocal timeZone (ReportTime localTime) = localTime

systemToReport :: IO ReportTime
systemToReport = do
  currentTime <- getCurrentTime
  -- currentTimeZone <- getCurrentTimeZone
  -- pure $ ReportTime <$> currentTime
  let currentLocal = utcToLocalTime utcTimeZone currentTime
  pure $ ReportTime currentLocal
  where
    utcTimeZone = TimeZone 0 False "UTC"
    -- timezone = reportTimeZone
