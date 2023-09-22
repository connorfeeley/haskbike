-- | This module defines the BeamReportTime datatype, which is a newtype wrapper for the last_reported field in the StationStatus table.

{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module ReportTime
     ( BeamReportTime (..)
     , localToPosix
     , localToSystem
     , posixToLocal
     , reportTimeType
     , reportTimeZone
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


-- | Newtype wrapper for the last_reported field, which is a POSIX timestamp in the JSON API.
newtype BeamReportTime where
  BeamReportTime :: LocalTime -> BeamReportTime
  deriving (Eq, Ord, Show, Read, FromField, ToField) via LocalTime
  deriving (HasSqlValueSyntax PgValueSyntax) via LocalTime
  deriving (FromBackendRow Postgres) via LocalTime
  deriving (HasSqlEqualityCheck Postgres) via LocalTime
  deriving (HasDefaultSqlDataType Postgres) via LocalTime

instance Num BeamReportTime where
    fromInteger i = BeamReportTime $ utcToLocalTime reportTimeZone . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral $ i
    abs           = error "BeamReportTime: abs not implemented"
    signum        = error "BeamReportTime: signum not implemented"
    negate        = error "BeamReportTime: negate not implemented"
    (+)           = error "BeamReportTime: (+) not implemented"
    (-)           = error "BeamReportTime: (-) not implemented"
    (*)           = error "BeamReportTime: (*) not implemented"

instance Real BeamReportTime where
  toRational (BeamReportTime t) = toRational . localToPosix $ t

instance Integral BeamReportTime => Enum BeamReportTime where
  toEnum = fromIntegral :: Int -> BeamReportTime
  fromEnum a = fromIntegral a :: Int

instance Enum BeamReportTime => Integral BeamReportTime where
  toInteger (BeamReportTime t) = fromIntegral $ localToPosix t
  quotRem a _ = (a, a)

instance (HasSqlValueSyntax BeamReportTime x, HasSqlValueSyntax BeamReportTime SqlNull) => HasSqlValueSyntax BeamReportTime (Maybe x) where
  sqlValueSyntax (Just x) = sqlValueSyntax x
  sqlValueSyntax Nothing  = sqlValueSyntax SqlNull

-- | Beam (migrate) datatype for the last_reported field in the StationStatus table.
reportTimeType :: DataType Postgres BeamReportTime
reportTimeType = DataType (timestampType Nothing True)

localToSystem :: LocalTime -> IO LocalTime
localToSystem localTime = do
  -- Get the current timezone
  currentTimeZone <- getCurrentTimeZone
  -- Convert the local time to a POSIX time, using "fake UTC" as the timezone
  let asPosix = localToPosix localTime
  -- Convert the POSIX time to a local time, using the system's current timezone
  pure $ posixToLocal' currentTimeZone asPosix
  where
    posixToLocal' timezone = utcToLocalTime timezone . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral

-- | TimeZone used to convert the API.T.StationStatus.last_reported field to a local time (effectively UTC).
reportTimeZone :: TimeZone
reportTimeZone = TimeZone 0 False "UTC"

posixToLocal :: Int -> LocalTime
posixToLocal = utcToLocalTime reportTimeZone . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral

localToPosix :: LocalTime -> Int
localToPosix = floor . utcTimeToPOSIXSeconds . localTimeToUTC reportTimeZone
