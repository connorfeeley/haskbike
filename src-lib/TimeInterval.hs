-- | Functions for handling time intervals/ranges.

module TimeInterval
     ( fromPico
     , generateTimeRange
     , incrementsPerRange
     , minsPerHourlyInterval
     , oneHour
     , oneMinute
     , secondsPerIntervalForRange
     ) where

import           Data.Fixed         ( Fixed (MkFixed), Pico )
import           Data.Int           ( Int32 )
import           Data.Time

import           Database.Beam
import           Database.BikeShare


_statusToIdAndTime :: StationStatusT f -> (Columnar f Int32, Columnar f UTCTime)
_statusToIdAndTime s = ((_unInformationStationId . _statusStationId) s, _statusLastReported s)


{- Generate a list of times between two times, incrementing every whole unit of the specified number of minutes.

>>> generateTimeRange (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 00 15 00))) 15
[ 2023-10-27 00:00:00 UTC, 2023-10-27 00:15:00 UTC, 2023-10-27 00:30:00 UTC ]

>>> generateTimeRange (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 01 15 00))) 15
[ 2023-10-27 00:00:00 UTC, 2023-10-27 00:15:00 UTC, 2023-10-27 00:30:00 UTC, 2023-10-27 00:45:00 UTC, 2023-10-27 01:00:00 UTC, 2023-10-27 01:15:00 UTC ]
-}
generateTimeRange :: UTCTime -> UTCTime -> Integer -> [UTCTime]
generateTimeRange start end incMinutes =
    [addUTCTime (fromInteger $ incMinutes * 60 * n) start | n <- [0 .. deltaIncrements]]
        where
          deltaIncrements = ceiling (diffUTCTime end start / fromInteger (60 * incMinutes)) :: Integer


{-

>>> incrementsPerRange
    (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 01 00 00)))
    (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 01 15 00)))
    (60*15)
-}
incrementsPerRange :: UTCTime -> UTCTime -> NominalDiffTime -> Integer
incrementsPerRange start end intervalSecs = intervals
  where
    diff :: NominalDiffTime -- ^ Difference in seconds between end and start
    diff = diffUTCTime end start

    intervals :: Integer    -- ^ Intervals within time range; rounded up.
    intervals = ceiling (diff / intervalSecs)


oneMinute, oneHour :: NominalDiffTime
oneMinute = secondsToNominalDiffTime 60
oneHour   = secondsToNominalDiffTime (60 * 60)


{- Calculate number of minutes per interval in an hour.
>>> minsPerHourlyInterval 4
15
-}
minsPerHourlyInterval :: NominalDiffTime -> Integer
minsPerHourlyInterval = (ceiling . (/) oneHour) . (*) 60

secondsPerIntervalForRange :: UTCTime -> UTCTime -> Pico -> Integer
secondsPerIntervalForRange start end numMaxIntervals = (flip div 1000000000000 . fromPico) (nominalDiffTimeToSeconds (diffUTCTime end start) / numMaxIntervals)


fromPico :: Pico -> Integer
fromPico (MkFixed i) = i
