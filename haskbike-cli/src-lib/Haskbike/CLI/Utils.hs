-- | This module contains utility functions for the CLI.

module Haskbike.CLI.Utils
     ( boxDrawingCharacter
     , boxLine
     , formatTime'
     , indent
     , timeFormat
     ) where

import           Data.Text.Lazy ( Text, intercalate, pack )
import           Data.Time      ( UTCTime, defaultTimeLocale, formatTime )

-- | Generate a line of box-drawing characters of length 'n'.
boxLine :: Int -> Text
boxLine n = intercalate "" (replicate n boxDrawingCharacter)

-- | A box-drawing character.
boxDrawingCharacter :: Text
boxDrawingCharacter = "\x2500"

-- | Adds 'n' amount of indentation (empty spaces).
indent :: Int -> Text
indent (n :: Int) = pack (unwords (replicate n ""))

-- | Time format that presents: Day of the week, Month, Day, Hour, Minute and Second.
timeFormat :: String
timeFormat = "%A, %b %e, %T (%Z)" -- DayOfWeek Month Day Hour:Minute:Second (TimeZone)

-- | Formats 'LocalTime' to the 'timeFormat' using the given 'TimeZone'.
formatTime' :: UTCTime -> String
formatTime' = formatTime defaultTimeLocale timeFormat
