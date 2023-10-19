{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.StationStatusVisualization
     ( StationStatusVisualizationPage (..)
     ) where


import           Data.Aeson
import           Data.Text
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.StationInformation

import           Fmt

import qualified Graphics.Vega.VegaLite                 as VL

import           Lucid
import           Lucid.Base                             ( makeAttribute )
import           Lucid.Servant

import           Servant

import           Server.Data.StationStatusVisualization

import           TextShow

import           Visualization.StationOccupancy


data StationStatusVisualizationPage where
  StationStatusVisualizationPage :: { _statusVisPageStationInfo :: StationInformation
                                    , _statusVisPageStationId   :: Int
                                    , _statusVisPageTimeRange   :: TimePair (Maybe LocalTime)
                                    , _statusVisPageTimeZone    :: TimeZone
                                    , _statusVisPageCurrentUtc  :: UTCTime
                                    , _statusVisPageDataLink    :: Link
                                    } -> StationStatusVisualizationPage

-- HTML serialization of a single person
instance ToHtml StationStatusVisualizationPage where
  toHtml params = div_ $ do
    head_ $ do
      link_ [ makeAttribute "rel" "stylesheet"
            , makeAttribute "type" "text/css"
            , makeAttribute "href" "/static/haskbike.css"
            ]
    with h1_ [class_ "header-large"] (toHtml pageTitle)
    with h2_ [class_ "header-small"] (toHtml dateHeader)
    h3_ (toHtml stationInfoHeader)
    with div_ [class_ "main-container"] $ do
      with div_ [name_ "vega-lite-container", class_ "graph"]
      -- , style_ "flex:1 1 0%; position:relative; outline:none; display:flex; min-height:30px; min-width:100px"
        -- VegaLite chart.
        (toHtmlRaw (VL.toHtmlWith vegaEmbedCfg vegaChart))
      with div_ [class_ "sidebar"] $ do
        with div_ [class_ "station-info"]
          "Station info goes here"
        form_ [] $ do
          p_ $ label_ $ "Station ID: " >> input_ [ makeAttribute "type" "number",         makeAttribute "name" "station-id", makeAttribute "value" (showt $ _statusVisPageStationId params) ]
          p_ $ label_ $ "Start Time: " >> input_ [ makeAttribute "type" "datetime-local", makeAttribute "name" "start-time", makeAttribute "value" (pack $ formatTimeHtml earliest) ]
          p_ $ label_ $ "End Time: "   >> input_ [ makeAttribute "type" "datetime-local", makeAttribute "name" "end-time",   makeAttribute "value" (pack $ formatTimeHtml latest) ]
    a_ [linkHref_ "/" (_statusVisPageDataLink params)] "Station Status Data"
    where
      _dataSourceSegments :: [Text] = [ "/data", "station-status", showt 7001]

      vegaEmbedCfg :: Maybe Value = Just $ toJSON ("logLevel", 4)

      inf = _statusVisPageStationInfo params

      pageTitle :: Text
      pageTitle = format "Available Bikes at {} (#{})"
                  (_infoName inf)
                  (_statusVisPageStationId params)
      dateHeader :: Text
      dateHeader = format "{} to {}"
                   (prettyTime (earliestTime times))
                   (prettyTime (latestTime times))
      stationInfoHeader :: Text
      stationInfoHeader = format "Capacity: {} | Charging station: {}"
                          (_infoCapacity inf)
                          (_infoIsChargingStation inf)


      times = enforceTimeRangeBounds (StatusDataParams (_statusVisPageTimeZone params) (_statusVisPageCurrentUtc params) (_statusVisPageTimeRange params))
      earliest = earliestTime times
      latest   = latestTime times

      prettyTime :: LocalTime -> String
      prettyTime = formatTime defaultTimeLocale "%A, %b %e, %T"
      formatTimeHtml = formatTime defaultTimeLocale htmlTimeFormat

      vegaChart :: VL.VegaLite
      vegaChart = availBikesOverTimeVL ("/" <> toUrlPiece (_statusVisPageDataLink params))

  -- do not worry too much about this
  toHtmlRaw = toHtml

