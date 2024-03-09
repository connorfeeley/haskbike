module Server.Page.StatusVisualization
     ( boolToText
     , dateHeader
     , endTimeInput
     , formatTimeHtml
     , makeInputField
     , maybeHeader
     , mkHeader
     , prettyTime
     , startTimeInput
     , submitInput
     , times
     , vegaChart
     ) where

import qualified Data.Text                      as T
import           Data.Time
import           Data.Time.Extras

import qualified Graphics.Vega.VegaLite         as VL

import           Lucid

import           Servant

import           Server.Page.Utils
import           Server.StatusDataParams

import           Visualization.StationOccupancy

dateHeader :: TimePair LocalTime -> T.Text
dateHeader t = T.pack (prettyTime (earliestTime t)) <> " ➜ " <>
               T.pack (prettyTime (latestTime   t))

-- This helper creates an input field with the provided 'id' and 'type' attributes.
makeInputField :: Monad m => HtmlT m () -> T.Text -> T.Text -> T.Text -> HtmlT m ()
makeInputField f t id' val =
  label_ [for_ id', style_ "width: fit-content"] $
  f <> input_ [ type_ t
              , id_ (id' <> T.pack "-input")
              , name_ id'
              , class_ "pure-input-rounded"
              , value_ val
              , style_ "width: 95%"
              ]

formatTimeHtml :: LocalTime -> T.Text
formatTimeHtml = T.pack . formatTime defaultTimeLocale htmlTimeFormat

-- * Input helpers
startTimeInput, endTimeInput :: Monad m => LocalTime -> HtmlT m ()
startTimeInput  = makeInputField "Start Time"        "datetime-local" "start-time" . formatTimeHtml
endTimeInput    = makeInputField "End Time"          "datetime-local" "end-time"   . formatTimeHtml

submitInput :: Monad m => HtmlT m ()
submitInput = makeInputField (i_ "Or hit Enter") "submit" "" "Submit"

prettyTime :: LocalTime -> String
prettyTime = formatTime defaultTimeLocale "%A, %b %e, %T"

vegaChart :: ([String] -> [T.Text]) -> Link -> VL.VegaLite
vegaChart filterFn dataLink = availBikesOverTimeVL filterFn ("/" <> toUrlPiece dataLink)

boolToText :: Bool -> T.Text
boolToText True  = "Yes"
boolToText False = "No"

maybeHeader :: Monad m => Bool -> HtmlT m () -> Maybe (HtmlT m ())
maybeHeader cond expr =
  if cond
  then Just expr
  else Nothing

times :: TimeZone -> UTCTime -> TimePair (Maybe LocalTime) -> TimePair LocalTime
times tz currentUtc range  = enforceTimeRangeBounds (StatusDataParams tz currentUtc range)

mkHeader params static range station headerComponent = hxSpinner_ staticPath componentLink
  where
    staticPath    = static params
    earliestTimeP = (earliestTime . range) params
    latestTimeP   = (latestTime   . range) params
    componentLink = fieldLink headerComponent station earliestTimeP latestTimeP
