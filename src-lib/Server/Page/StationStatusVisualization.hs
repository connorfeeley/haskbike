{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.StationStatusVisualization
     ( StationStatusVisualizationPage (..)
     ) where


import           Data.Aeson
import qualified Data.Text                              as T
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.StationInformation

import           Fmt

import qualified Graphics.Vega.VegaLite                 as VL

import           Lucid
import           Lucid.Servant

import           Servant

import           Server.Data.StationStatusVisualization
import           Server.DataAPI

import           TextShow

import           Visualization.StationOccupancy

data StationStatusVisualizationPage where
  StationStatusVisualizationPage :: { _statusVisPageStationInfo :: StationInformation
                                    , _statusVisPageStationId   :: Int
                                    , _statusVisPageTimeRange   :: TimePair (Maybe LocalTime)
                                    , _statusVisPageTimeZone    :: TimeZone
                                    , _statusVisPageCurrentUtc  :: UTCTime
                                    } -> StationStatusVisualizationPage

-- HTML serialization of a single person
instance ToHtml StationStatusVisualizationPage where
  toHtml params = div_ $ do
    style_ ".grid-container { display: grid; grid-template-columns: auto auto auto; } .grid-container > div { padding: 20px 0; } .vega-embed { width: 80%; height: 70%; }"
    h1_ (toHtml pageTitle)
    h2_ (toHtml dateHeader)
    h3_ (toHtml stationInfoHeader)
    -- a_ [href_ $ renderText $ safeLink apiProxy apiDataStationStatus id] "Station Status Data"
    -- div_ $ toHtml (safeLink "/")-- (T.intercalate "/" dataSourceSegments))
    div_ vegaContainerStyle (toHtmlRaw (VL.toHtmlWith vegaEmbedCfg vegaChart))
    where _dataSourceSegments :: [T.Text] = [ "/data", "station-status", showt 7001]
          vegaContainerStyle = [ style_ "flex:1 1 0%; position:relative; outline:none; display:flex; min-height:30px; min-width:100px" ]
          vegaEmbedCfg :: Maybe Value = Just $ toJSON ("logLevel", 4)
          inf = _statusVisPageStationInfo params
          pageTitle :: T.Text = format "Available Bikes at {} (#{})"
                                (_infoName inf)
                                (_statusVisPageStationId params)
          dateHeader :: T.Text = format "{} to {}"
                                 (formatTime' (earliestTime times))
                                 (formatTime' (latestTime times))
          stationInfoHeader :: T.Text = format "Capacity: {} | Charging station: {}" (_infoCapacity inf) (_infoIsChargingStation inf)
          vegaChart = availBikesOverTimeVL (_statusVisPageStationId params) earliest latest

          times = enforceTimeRangeBounds (StatusDataParams (_statusVisPageTimeZone params) (_statusVisPageCurrentUtc params) (_statusVisPageTimeRange params))
          earliest = earliestTime times
          latest   = latestTime times

          formatTime' :: LocalTime -> String
          formatTime' = formatTime defaultTimeLocale "%A, %b %e, %T"

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
