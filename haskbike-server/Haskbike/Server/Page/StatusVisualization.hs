{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Haskbike.Server.Page.StatusVisualization
     ( boolToText
     , dateHeader
     , maybeHeader
     , mkHeader
     , prettyTime
     , times
     , vegaChart
     ) where

import qualified Data.Text                               as T
import           Data.Time
import           Data.Time.Extras

import qualified Graphics.Vega.VegaLite                  as VL

import           Haskbike.Server.Page.Utils
import           Haskbike.Server.Routes.Components
import           Haskbike.Server.StatusDataParams
import           Haskbike.Visualization.StationOccupancy

import           Lucid

import           Servant

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

mkHeader params range station headerComponent = hxSpinner_ componentLink
  where
    earliestTimeP = (earliestTime . range) params
    latestTimeP   = (latestTime   . range) params
    componentLink = (headerComponent . eventsComponents $ componentsRoutesLinks) station earliestTimeP latestTimeP
