-- | This module defines the ReportTime datatype, which is a newtype wrapper for the last_reported field in the StationStatus table.

{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module ReportTime
     ( ReportTime (..)
     , addHours
     , localToPosix
     , localToSystem
     , localToSystem'
     , posixToLocal
     , reportTime
     , reportTimeType
     , reportTimeZone
     , reportToLocal
       -- Re-exports for ReportTime constructors
     , Day (..)
     , TimeOfDay (..)
     , fromGregorian
     ) where

import           Data.Time
import           Data.Time.Clock.POSIX

import           Database.Beam
import           Database.Beam.Backend                ( HasSqlValueSyntax (sqlValueSyntax),
                                                        IsSql92DataTypeSyntax (timestampType), SqlNull (SqlNull) )
import           Database.Beam.Migrate                ( HasDefaultSqlDataType )
import           Database.Beam.Postgres               ( Postgres )
import           Database.Beam.Postgres.Syntax        ( PgValueSyntax )
import           Database.PostgreSQL.Simple.FromField ( FromField (..) )
import           Database.PostgreSQL.Simple.ToField   ( ToField (..) )


-- | Subtract a number of hours from a LocalTime.
addHours :: NominalDiffTime -> LocalTime -> LocalTime
addHours h time' = utcToLocalTime reportTimeZone (addUTCTime (h*3600) (localTimeToUTC utc time'))

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

-- | Convert a LocalTime to a LocalTime in the system's current timezone.
localToSystem :: TimeZone -> LocalTime -> LocalTime
localToSystem currentTimeZone localTime = do
  -- Convert the local time to a POSIX time, using "fake UTC" as the timezone
  let asPosix = localToPosix localTime
  -- Convert the POSIX time to a local time, using the system's current timezone
  posixToLocal' currentTimeZone asPosix
  where
    posixToLocal' timezone = utcToLocalTime timezone . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral

-- | Convert a LocalTime to a LocalTime in the system's current timezone.
localToSystem' :: LocalTime -> IO LocalTime
localToSystem' localTime = do
  -- Get the current timezone
  currentTimeZone <- getCurrentTimeZone
  pure $ localToSystem currentTimeZone localTime


-- | TimeZone used to convert the API.T.StationStatus.last_reported field to a local time (effectively UTC).
reportTimeZone :: TimeZone
reportTimeZone = TimeZone 0 False "UTC"

posixToLocal :: Int -> LocalTime
posixToLocal = utcToLocalTime reportTimeZone . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral

localToPosix :: LocalTime -> Int
localToPosix = floor . utcTimeToPOSIXSeconds . localTimeToUTC reportTimeZone

-- | Convert ReportTime to LocalTime
reportToLocal :: ReportTime -> LocalTime
reportToLocal (ReportTime localTime) = localTime
