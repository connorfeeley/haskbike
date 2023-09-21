-- | Common functions and types for the project.

module Common
     ( localToPosix
     , localToSystem
     , posixToLocal
     , reportTimeZone
     ) where

import           Data.Time             ( LocalTime, TimeZone (..), localTimeToUTC, secondsToNominalDiffTime,
                                         utcToLocalTime )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import           Data.Time.LocalTime   ( getCurrentTimeZone )


localToSystem :: LocalTime -> IO LocalTime
localToSystem time = do
  -- Get the current timezone
  currentTimeZone <- getCurrentTimeZone
  -- Convert the local time to a POSIX time, using "fake UTC" as the timezone
  let asPosix = localToPosix time
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
