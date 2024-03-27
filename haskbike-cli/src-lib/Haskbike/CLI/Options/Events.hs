{-# LANGUAGE RecordWildCards #-}

-- | Options for the events commands.

module Haskbike.CLI.Options.Events
     ( EventCountOptions (..)
     , EventRangeOptions (..)
     , EventSubcommand (..)
     , EventsOptions (..)
     , dayParser
     , eventRangeOptionsParser
     , eventsCountOptionsParser
     , eventsCountsLimit
     , eventsOptionsParser
     , timeOfDayParser
     ) where

import           Data.Time

import           Options.Applicative


-- | Options for the 'Events' command.
data EventsOptions where
  EventsOptions :: { optEventsSubcommand :: EventSubcommand
                   , optEventsLimit :: Maybe Int
                   } -> EventsOptions
  deriving (Show)

data EventSubcommand where
  EventCounts :: EventCountOptions -> EventSubcommand
  EventRange  :: EventRangeOptions -> EventSubcommand
  deriving (Show)

data EventCountOptions where
  EventCountOptions :: { optEventsCountLimit :: Maybe Int
                       , optEventsCountStationId :: Maybe Int
                       , optEventsCountStartDay :: Maybe Day
                       , optEventsCountStartTime :: Maybe TimeOfDay
                       , optEventsCountEndDay :: Maybe Day
                       , optEventsCountEndTime :: Maybe TimeOfDay
                       } -> EventCountOptions
  deriving (Show)

data EventRangeOptions where
  EventRangeOptions :: { startDay :: Maybe Day
                       , startTime :: Maybe TimeOfDay
                       , endDay :: Maybe Day
                       , endTime :: Maybe TimeOfDay
                       } -> EventRangeOptions
  deriving (Show)

-- | Parser for 'EventsOptions'.
eventsOptionsParser :: Parser EventsOptions
eventsOptionsParser = EventsOptions
  <$> hsubparser
    (  command "counts" (info (EventCounts <$> eventsCountOptionsParser) (progDesc "Counts of docking and undocking events."))
    <> command "range"  (info (EventRange <$> eventRangeOptionsParser)   (progDesc "Docking and undocking events within a date range."))
    )
  <*> argument auto
    ( metavar "LIMIT"
    <> showDefault
    <> value Nothing
    <> help "Limit the number of events displayed."
    )

eventsCountOptionsParser :: Parser EventCountOptions
eventsCountOptionsParser = do
  optEventsCountLimit <- eventsCountsLimit
  optEventsCountStationId <- option (optional auto)
    ( metavar "STATION_ID"
    <> long "station"
    <> short 's'
    <> showDefault
    <> value Nothing
    <> help "Restrict to a specific station (ID)."
    )
  optEventsCountStartDay <- dayParser
  optEventsCountEndDay <- dayParser
  optEventsCountStartTime <- optional timeOfDayParser
  optEventsCountEndTime <- optional timeOfDayParser
  return EventCountOptions {..}

eventsCountsLimit :: Parser (Maybe Int)
eventsCountsLimit =
  option (optional auto)
    ( metavar "LIMIT"
    <> long "limit"
    <> short 'n'
    <> showDefault
    <> value Nothing
    <> help "Limit the number of events displayed."
    )

eventRangeOptionsParser :: Parser EventRangeOptions
eventRangeOptionsParser = do
  startDay <- dayParser
  endDay <- dayParser
  startTime <- optional timeOfDayParser
  endTime <- optional timeOfDayParser
  return EventRangeOptions {..}

dayParser :: Parser (Maybe Day)
dayParser =
  argument (optional auto)
    ( metavar "DATE"
    <> help "A date in the format yyyy-mm-dd."
    )

timeOfDayParser :: Parser TimeOfDay
timeOfDayParser =
  argument auto
    ( metavar "TIME"
    <> help "A time in the format HH:MM:SS."
    )
