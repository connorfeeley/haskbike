{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Server.Page.StatusVisualization
     ( boolToText
     , dateHeader
     , maybeHeader
     , mkHeader
     , prettyTime
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
