-- | This module defines the data types used to render the station status visualization page.

module API.Server.Types.Page.StationStatusVisualization
     ( StationStatusVisualizationPage (..)
     ) where

import qualified Graphics.Vega.VegaLite         as VL

import           Lucid

import           Visualization.StationOccupancy

data StationStatusVisualizationPage where
  StationStatusVisualizationPage :: { _statusVisPageStationId :: Int } -> StationStatusVisualizationPage

-- HTML serialization of a single person
instance ToHtml StationStatusVisualizationPage where
  toHtml statusVisualization =
    toHtmlRaw (VL.toHtml availBikesOverTimeVL)
    -- tr_ $ do
    --   td_ (toHtml ("test" :: String))
    --   td_ (toHtml (show (_statusVisPageStationId statusVisualization)))

  -- do not worry too much about this
  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [StationStatusVisualizationPage] where
  toHtml visualizations = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"

    -- this just calls toHtml on each person of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml visualizations

  toHtmlRaw = toHtml
