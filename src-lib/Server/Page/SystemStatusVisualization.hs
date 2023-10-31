{-# LANGUAGE DataKinds #-}
-- | This module defines the data types used to render the station status visualization page.

module Server.Page.SystemStatusVisualization
     ( SystemStatusVisualizationPage (..)
     , vegaSourceUrlsCdn
     , vegaSourceUrlsLocal
     ) where


import           Data.Maybe                      ( catMaybes )
import qualified Data.Text                       as T
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare              ( StationStatus )
import           Database.BikeShare.Operations

import           Fmt

import           Graphics.Vega.VegaLite.Extra

import           Lucid

import           Servant

import           Server.Classes
import           Server.Page.StatusVisualization

import           TextShow

data SystemStatusVisualizationPage where
  SystemStatusVisualizationPage :: { _systemStatusVisPageTimeRange     :: TimePair (Maybe LocalTime)
                                   , _systemStatusVisPageTimeZone      :: TimeZone
                                   , _systemStatusVisPageCurrentUtc    :: UTCTime
                                   , _systemStatusVisPageDockingEvents :: [DockingEventsCount]
                                   , _systemStatusVisPageChargings     :: [(StationStatus, [ChargingEvent])]
                                   , _systemStatusVisPageStaticLink    :: Link
                                   } -> SystemStatusVisualizationPage

instance ToHtmlComponents SystemStatusVisualizationPage where
  toMenuHeading _ = menuHeading "#visualization" "Available Bikes"

instance ToHtml SystemStatusVisualizationPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      h1_ [] "System Information"
      h2_ [] (toHtml dateHeader)
    br_ []
    div_ [class_ "content"] $ do
      -- Informational headers
      h2_ [style_ "text-align: center"] "System Information & Statistics"
      br_ []
      div_ [class_ "pure-g", style_ "text-align: center"] $ do
        let headers = catMaybes [ Just (toHtml (eventsHeader :: DockingEventsHeader 'Undocking))
                                , Just (toHtml (eventsHeader :: DockingEventsHeader 'Docking))
                                , Just (toHtml (ChargingEventsHeader (_systemStatusVisPageChargings params)))
                                ]
        mconcat $ map (`with` [class_ ("pure-u-md-1-" <> showt (length headers))]) headers

      br_ []

      -- Selection form
      form_ [class_ "pure-form pure-form-stacked", style_ "text-align: center"] $ fieldset_ $ do
        legend_ $ h3_ "Query Parameters"
        div_ [class_ "pure-g"] $ do -- Grid layout for form
          div_ [class_ "pure-u-1 pure-u-md-1-4"] (startTimeInput earliest)
          div_ [class_ "pure-u-1 pure-u-md-1-4"] (endTimeInput latest)
          div_ [class_ "pure-u-1 pure-u-md-1-4"] submitInput

      -- with div_ [class_ "graph"] (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal (vegaEmbedCfg HideActions) vegaChart))

    where
      dateHeader :: T.Text
      dateHeader = format "{} ➜ {}"
                   (prettyTime (earliestTime times'))
                   (prettyTime (latestTime   times'))

      eventsHeader :: DockingEventsHeader a
      eventsHeader = DockingEventsHeader (_systemStatusVisPageDockingEvents params)

      times' = times (_systemStatusVisPageTimeZone params) (_systemStatusVisPageCurrentUtc params) (_systemStatusVisPageTimeRange params)
      earliest = earliestTime times'
      latest   = latestTime   times'

