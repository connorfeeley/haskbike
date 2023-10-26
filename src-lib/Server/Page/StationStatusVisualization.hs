{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.StationStatusVisualization
     ( StationStatusVisualizationPage (..)
     , vegaSourceUrlsCdn
     , vegaSourceUrlsLocal
     ) where


import           Control.Lens

import           Data.Aeson
import           Data.Text
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.Operations          ( DockingEventsCount (..), EventsCountResult (..) )
import           Database.BikeShare.StationInformation

import           Fmt

import qualified Graphics.Vega.VegaLite                 as VL
import           Graphics.Vega.VegaLite.Extra

import           Lucid

import           Servant

import           Server.Classes
import           Server.Data.StationStatusVisualization
import           Server.PureCSS

import           TextShow

import           Visualization.StationOccupancy


data StationStatusVisualizationPage where
  StationStatusVisualizationPage :: { _statusVisPageStationInfo   :: StationInformation
                                    , _statusVisPageStationId     :: Int
                                    , _statusVisPageTimeRange     :: TimePair (Maybe LocalTime)
                                    , _statusVisPageTimeZone      :: TimeZone
                                    , _statusVisPageCurrentUtc    :: UTCTime
                                    , _statusVisPageDockingEvents :: DockingEventsCount
                                    , _statusVisPageChargings     :: (Int, Int, Int)
                                    , _statusVisPageDataLink      :: Link
                                    , _statusVisPageStaticLink    :: Link
                                    } -> StationStatusVisualizationPage

instance ToHtml StationStatusVisualizationPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      h1_ [] (toHtml (pageTitle (_statusVisPageStationId params) (_infoName inf)))
      h2_ [] (toHtml dateHeader)
    div_ [class_ "content"] $ do
      contentSubhead stationInfoHeader
      -- Selection form
      form_ [class_ "pure-form pure-form-stacked"] $ fieldset_ $ do
        legend_ "Select Query Parameters"
        div_ [class_ "pure-g"] $ do

          div_ [class_ "pure-u-1 pure-u-md-1-4"] $
            label_ [for_ "station-id-input"] $ "Station ID" <> input_ [ type_ "number", id_ "station-id-input", name_ "station-id", class_ "pure-input-rounded", value_ (showt $ _statusVisPageStationId params) ]

          div_ [class_ "pure-u-1 pure-u-md-1-4"] $
            label_ [for_ "start-time-input"] $ "Start Time" <> input_ [ type_ "datetime-local", id_ "start-time-input", name_ "start-time", class_ "pure-input-rounded", value_ (pack $ formatTimeHtml earliest) ]

          div_ [class_ "pure-u-1 pure-u-md-1-4"] $
            label_ [for_ "end-time-input"] $ "End Time"      <> input_ [ type_ "datetime-local", id_ "end-time-input", name_ "end-time", class_ "pure-input-rounded", value_ (pack $ formatTimeHtml latest) ]

          div_ [class_ "pure-u-1 pure-u-md-1-4"] $
            label_ [for_ "end-time-input"] "Submit" <> input_ [ type_ "submit", id_ "submit-form", name_ "submit-form", class_ "pure-input-rounded", value_ "Submit" ]

      with div_ [class_ "graph"] (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal vegaEmbedCfg vegaChart))

    where
      _dataSourceSegments :: [Text] = [ "/data", "station-status", showt 7001]

      inf = _statusVisPageStationInfo params

      pageTitle :: Int -> Text -> Text
      pageTitle = format "#{}: {}"

      dateHeader :: Text
      dateHeader = format "{} ➜ {}"
                   (prettyTime (earliestTime times))
                   (prettyTime (latestTime times))
      stationInfoHeader :: Text
      stationInfoHeader = format "Capacity: {} docks | Charging station: {} | Undockings(I/E/E5): {} [{}/{}/{}] | Dockings(I/E/E5): {} [{}/{}/{}] | Bikes charged: {}"
                          (_infoCapacity inf)
                          (boolToText (_infoIsChargingStation inf))
                          (abs (_eventsCountUndockings (_eventsIconicCount events) + _eventsCountUndockings (_eventsEfitCount events) + _eventsCountUndockings (_eventsEfitG5Count events)))
                            (abs (_eventsCountUndockings (_eventsIconicCount events))) (abs (_eventsCountUndockings (_eventsEfitCount events))) (abs (_eventsCountUndockings (_eventsEfitG5Count events)))
                          (_eventsCountDockings (_eventsIconicCount events) + _eventsCountDockings (_eventsEfitCount events) + _eventsCountDockings (_eventsEfitG5Count events))
                            (_eventsCountDockings (_eventsIconicCount events)) (_eventsCountDockings (_eventsEfitCount events)) (_eventsCountDockings (_eventsEfitG5Count events))
                          (abs (_statusVisPageChargings params ^. _1))
                          -- FIXME: these values make no sense
                          -- (abs (_statusVisPageChargings params ^. _2)) (abs (_statusVisPageChargings params ^. _3))
        where
          events = _statusVisPageDockingEvents params
          undockings = _eventsBoostCount events

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

      boolToText :: Bool -> Text
      boolToText True  = "Yes"
      boolToText False = "No"

instance ToHtmlComponents StationStatusVisualizationPage where
  toMenuHeading _ = menuHeading "#visualization" "Available Bikes"
