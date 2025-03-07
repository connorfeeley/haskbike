-- | Functions for handling time intervals/ranges.

module Haskbike.TimeInterval
     ( fromPico
     , generateTimeRange
     , incrementsPerRange
     , minsPerHourlyInterval
     , oneHour
     , oneMinute
     , secondsPerIntervalForRange
     ) where

import           Data.Fixed ( Fixed (MkFixed), Pico )
import           Data.Time



{- Generate a list of times between two times, incrementing every whole unit of the specified number of minutes.

>>> generateTimeRange (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 00 15 00))) 15
[ 2023-10-27 00:00:00 UTC, 2023-10-27 00:15:00 UTC, 2023-10-27 00:30:00 UTC ]

>>> generateTimeRange (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime midnight)) (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 01 15 00))) 15
[ 2023-10-27 00:00:00 UTC, 2023-10-27 00:15:00 UTC, 2023-10-27 00:30:00 UTC, 2023-10-27 00:45:00 UTC, 2023-10-27 01:00:00 UTC, 2023-10-27 01:15:00 UTC ]
-}
generateTimeRange :: UTCTime -> UTCTime -> Integer -> [UTCTime]
generateTimeRange start end incMinutes =
    let !incSeconds = incMinutes * 60
        !diffSecs = diffUTCTime end start
        !deltaIncrements = ceiling (diffSecs / fromInteger incSeconds) :: Integer
        addTimeStep !n = addUTCTime (fromInteger $ incSeconds * n) start
    in [addTimeStep n | n <- [0 .. deltaIncrements]]


{-

>>> incrementsPerRange
    (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 01 00 00)))
    (UTCTime (fromGregorian 2023 10 27) (timeOfDayToTime (TimeOfDay 01 15 00)))
    (60*15)
-}
incrementsPerRange :: UTCTime -> UTCTime -> NominalDiffTime -> Integer
incrementsPerRange start end intervalSecs = 
    let !diff = diffUTCTime end start  -- ^ Difference in seconds between end and start
    in ceiling (diff / intervalSecs)    -- ^ Intervals within time range; rounded up.


oneMinute, oneHour :: NominalDiffTime
oneMinute = secondsToNominalDiffTime 60
oneHour   = secondsToNominalDiffTime (60 * 60)


{- Calculate number of minutes per interval in an hour.
>>> minsPerHourlyInterval 4
15
-}
minsPerHourlyInterval :: NominalDiffTime -> Integer
minsPerHourlyInterval numIntervals = 
    let !secondsPerInterval = (*) 60 numIntervals
    in ceiling (oneHour / secondsPerInterval)

secondsPerIntervalForRange :: UTCTime -> UTCTime -> Pico -> Integer
secondsPerIntervalForRange start end numMaxIntervals = 
    let !diffSeconds = nominalDiffTimeToSeconds (diffUTCTime end start)
        !intervalsRatio = diffSeconds / numMaxIntervals
        !picoValue = fromPico intervalsRatio
    in picoValue `div` 1000000000000


fromPico :: Pico -> Integer
fromPico (MkFixed i) = i
