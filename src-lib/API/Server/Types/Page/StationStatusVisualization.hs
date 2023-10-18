-- | This module defines the data types used to render the station status visualization page.

module API.Server.Types.Page.StationStatusVisualization
     ( StationStatusVisualizationPage (..)
     ) where


import           Data.Aeson
import qualified Data.Text                      as T
import           Data.Time
import           Data.Time.Extras

import qualified Graphics.Vega.VegaLite         as VL

import           Lucid

import           TextShow

import           Visualization.StationOccupancy

data StationStatusVisualizationPage where
  StationStatusVisualizationPage :: { _statusVisPageStationId :: Int
                                    , _statusVisPageTimeRange :: TimePair (Maybe LocalTime)
                                    } -> StationStatusVisualizationPage

-- HTML serialization of a single person
instance ToHtml StationStatusVisualizationPage where
  toHtml statusVisualization = div_ $ do
    style_ ".grid-container { display: grid; grid-template-columns: auto auto auto; } .grid-container > div { padding: 20px 0; } .vega-embed { width: 80%; height: 70%; }"
    h1_ (toHtml ("Available Bikes Over Time" :: String))
    -- div_ $ toHtml (safeLink "/")-- (T.intercalate "/" dataSourceSegments))
    div_ vegaContainerStyle (toHtmlRaw (VL.toHtmlWith vegaEmbedCfg (availBikesOverTimeVL (_statusVisPageStationId statusVisualization))))
    where _dataSourceSegments :: [T.Text] = [ "/data", "station-status", showt 7001]
          vegaContainerStyle = [ style_ "flex:1 1 0%; position:relative; outline:none; display:flex; min-height:30px; min-width:100px" ]
          vegaEmbedCfg :: Maybe Value = Just $ toJSON ("logLevel", 4)

  -- do not worry too much about this
  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [StationStatusVisualizationPage] where
  toHtml visualizations = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"

    -- this just calls toHtml on each visualization of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml visualizations

  toHtmlRaw = toHtml
