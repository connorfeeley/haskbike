{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module include various convenience extensions to 'Data.Time'.

module Data.Time.Extras
     ( TimePair (..)
     , addHours
     , addMinutes
     , hourBefore
     , htmlTimeFormat
     , localToPosix
     , localToSystem
     , localToSystem'
     , posixToLocal
     , posixToUtc
     , posixToZonedTime
     , shortTimeFormat
     , utcFromGregorian
     , utcToPosix
     , zonedTimeToPosix
     ) where

import           Data.Time
import           Data.Time.Clock.POSIX


-- | Data type containing a pair of times.
data TimePair a where
  TimePair :: { earliestTime   :: !a
              , latestTime     :: !a
              , tz             :: !TimeZone
              , currentUtcTime :: !UTCTime
              } -> TimePair a
  deriving (Show, Eq, Ord)

utcFromGregorian :: Year -> MonthOfYear -> DayOfMonth -> UTCTime
utcFromGregorian year month day = UTCTime (fromGregorian year month day) (timeOfDayToTime midnight)

-- | Subtract an hour from a UTCTime.
hourBefore :: UTCTime -> UTCTime
hourBefore = addUTCTime (-3600)

-- | Add a number of hours to a LocalTime.
addHours :: NominalDiffTime -> LocalTime -> LocalTime
addHours h time' = utcToLocalTime utc (addUTCTime (h*3600) (localTimeToUTC utc time'))

-- | Add a number of hours to a LocalTime.
addMinutes :: NominalDiffTime -> LocalTime -> LocalTime
addMinutes m time' = utcToLocalTime utc (addUTCTime (m*60) (localTimeToUTC utc time'))

-- | Convert a LocalTime to a LocalTime in the given timezone.
localToSystem :: TimeZone -> LocalTime -> LocalTime
localToSystem systemTimeZone localTime = do
  -- Convert the local time to a POSIX time, using "fake UTC" as the timezone
  let !asPosix = localToPosix localTime
  -- Convert the POSIX time to a local time, using the system's current timezone
  posixToLocal' systemTimeZone asPosix
  where
    posixToLocal' timezone = utcToLocalTime timezone . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral

-- | Convert a LocalTime to a LocalTime in the system's current timezone.
localToSystem' :: LocalTime -> IO LocalTime
localToSystem' localTime = do
  -- Get the current timezone
  !currentTimeZone <- getCurrentTimeZone
  pure $ localToSystem currentTimeZone localTime


-- * POSIX conversion functions
-- NOTE: POSIX time is by nature UTC.
-- POSIX <-> LocalTime
posixToLocal :: Int -> LocalTime
posixToLocal = utcToLocalTime utc . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral

localToPosix :: LocalTime -> Int
localToPosix = floor . utcTimeToPOSIXSeconds . localTimeToUTC utc

-- POSIX <-> ZonedTime
posixToZonedTime :: TimeZone -> Int -> ZonedTime
posixToZonedTime tz = utcToZonedTime tz . (posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral)

zonedTimeToPosix :: ZonedTime -> Int
zonedTimeToPosix = floor . utcTimeToPOSIXSeconds . zonedTimeToUTC

-- POSIX <-> ZonedTime
posixToUtc :: Int -> UTCTime
posixToUtc = posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral

utcToPosix :: UTCTime -> Int
utcToPosix = floor . utcTimeToPOSIXSeconds

htmlTimeFormat :: String
htmlTimeFormat = "%FT%H:%M"

-- Short month name, day, hours-minutes-seconds
shortTimeFormat :: String
shortTimeFormat = "%b %d %H:%M:%S"
