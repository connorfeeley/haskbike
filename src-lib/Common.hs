-- | Common functions and types for the project.

module Common
  ( reportTimeZone
  , posixToLocal
  , localToPosix
  ) where

import           Data.Time             (LocalTime, TimeZone (..),
                                        localTimeToUTC,
                                        secondsToNominalDiffTime,
                                        utcToLocalTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)

-- | TimeZone used to convert the API.T.StationStatus.last_reported field to a local time (effectively UTC).
reportTimeZone :: TimeZone
reportTimeZone = TimeZone 0 False "UTC"

posixToLocal :: Int -> LocalTime
posixToLocal = utcToLocalTime reportTimeZone . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral

localToPosix :: LocalTime -> Int
localToPosix = floor . utcTimeToPOSIXSeconds . localTimeToUTC reportTimeZone
