-- |

module Haskbike.Server.StatusDataParams
     ( StatusDataParams (..)
     , enforceTimeRangeBounds
     ) where
import           Data.Maybe       ( fromMaybe )
import           Data.Time
import           Data.Time.Extras

import           GHC.Generics     ( Generic )

data StatusDataParams a where
  StatusDataParams :: { visTimeZone :: TimeZone
                      , visCurrentUtc :: UTCTime
                      , visTimeRange :: TimePair a
                      } -> StatusDataParams a
  deriving (Show, Generic, Eq, Ord)

enforceTimeRangeBounds :: StatusDataParams (Maybe LocalTime) -> TimePair LocalTime
enforceTimeRangeBounds params = TimePair start end tz currentUtc
  where
    tz = visTimeZone params
    currentUtc = visCurrentUtc params
    yesterday = addUTCTime (-24 * 3600) currentUtc
    earliest = earliestTime (visTimeRange params)
    latest   = latestTime   (visTimeRange params)

    -- Default to 24 hours ago -> now.
    start = fromMaybe (utcToLocalTime tz yesterday)  earliest
    end   = fromMaybe (utcToLocalTime tz currentUtc) latest
