{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.StationStatusVisualization
     ( StationStatusVisualizationPage (..)
     , vegaSourceUrlsCdn
     , vegaSourceUrlsLocal
     ) where


import           Data.Aeson
import           Data.Text
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.StationInformation

import           Fmt

import qualified Graphics.Vega.VegaLite                 as VL
import           Graphics.Vega.VegaLite.Extra

import           Lucid
import           Lucid.Base                             ( makeAttribute )
import           Lucid.Servant

import           Servant

import           Server.Data.StationStatusVisualization
import           Server.Page.Utils
import           Server.PureCSS

import           TextShow

import           Visualization.StationOccupancy


data StationStatusVisualizationPage where
  StationStatusVisualizationPage :: { _statusVisPageStationInfo :: StationInformation
                                    , _statusVisPageStationId   :: Int
                                    , _statusVisPageTimeRange   :: TimePair (Maybe LocalTime)
                                    , _statusVisPageTimeZone    :: TimeZone
                                    , _statusVisPageCurrentUtc  :: UTCTime
                                    , _statusVisPageDataLink    :: Link
                                    , _statusVisPageStaticLink  :: Link
                                    } -> StationStatusVisualizationPage

instance ToHtml StationStatusVisualizationPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      h1_ [] (toHtml (pageTitle (_infoName inf) (_statusVisPageStationId params)))
      h2_ [] (toHtml dateHeader)
    div_ [class_ "content"] $ do
      contentSubhead stationInfoHeader
      with div_ [class_ "main-container"] $ do
        -- Selection form
        form_ [] $ do
          p_ $ label_ $ "Station ID: " >> input_ [ makeAttribute "type" "number",         makeAttribute "name" "station-id", makeAttribute "value" (showt $ _statusVisPageStationId params) ]
          p_ $ label_ $ "Start Time: " >> input_ [ makeAttribute "type" "datetime-local", makeAttribute "name" "start-time", makeAttribute "value" (pack $ formatTimeHtml earliest) ]
          p_ $ label_ $ "End Time: "   >> input_ [ makeAttribute "type" "datetime-local", makeAttribute "name" "end-time",   makeAttribute "value" (pack $ formatTimeHtml latest) ]
        with div_ [class_ "graph"]
          (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal vegaEmbedCfg vegaChart))

    where
      _dataSourceSegments :: [Text] = [ "/data", "station-status", showt 7001]

      inf = _statusVisPageStationInfo params

      pageTitle :: Text -> Int -> Text
      pageTitle = format "Available Bikes at {} (#{})"

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

      vegaEmbedCfg :: Maybe Value
      vegaEmbedCfg =  Just (toJSON $ object [("logLevel", "4"), ("$schema", "/static/js/vega/schema/vega-lite/v4.json")])

      vegaChart :: VL.VegaLite
      vegaChart = availBikesOverTimeVL ("/" <> toUrlPiece (_statusVisPageDataLink params))

      staticPath :: Text
      staticPath = "/" <> toUrlPiece (_statusVisPageStaticLink params)
