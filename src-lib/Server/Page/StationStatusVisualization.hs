{-# LANGUAGE DataKinds #-}
-- | This module defines the data types used to render the station status visualization page.

module Server.Page.StationStatusVisualization
     ( StationStatusVisualizationPage (..)
     ) where


import           Control.Monad                                ( when )

import           Data.Maybe                                   ( catMaybes, isJust )
import qualified Data.Text                                    as T
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.Tables.StationInformation

import           Graphics.Vega.VegaLite.Extra

import           Lucid

import           Servant

import           Server.Classes
import           Server.ComponentsAPI
import           Server.Page.StatusVisualization
import           Server.Page.Utils

import           TextShow

data StationStatusVisualizationPage where
  StationStatusVisualizationPage :: { _statusVisPageStationInfo    :: StationInformation
                                    , _statusVisPageStationId      :: Int
                                    , _statusVisPageTimeRange      :: TimePair (Maybe LocalTime)
                                    , _statusVisPageTimeZone       :: TimeZone
                                    , _statusVisPageCurrentUtc     :: UTCTime
                                    , _statusVisPageDataLink       :: Link
                                    , _statusVisPageStaticLink     :: Link
                                    } -> StationStatusVisualizationPage

instance ToHtmlComponents StationStatusVisualizationPage where
  toMenuHeading _ = menuHeading "#visualization" "Available Bikes"

instance ToHtml StationStatusVisualizationPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      h1_ [] (toHtml (pageTitle (_statusVisPageStationId params) (_infoName inf)))
      h2_ [] ((toHtml . dateHeader) times')
    br_ []
    div_ [class_ "content"] $ do
      -- Informational headers
      h2_ [style_ "text-align: center"] "Station Information & Statistics"
      br_ []
      div_ [class_ "pure-g", style_ "text-align: center"] $ do
        let headers = catMaybes [ Just capacityHeader
                                , Just $ hxSpinner_ staticLink (fieldLink dockingEventsHeader  (Just (_statusVisPageStationId params)) (earliestTime (_statusVisPageTimeRange params)) (latestTime (_statusVisPageTimeRange params)))
                                , if _infoIsChargingStation inf
                                  then Just $ hxSpinner_ staticLink (fieldLink chargingEventsHeader (Just (_statusVisPageStationId params)) (earliestTime (_statusVisPageTimeRange params)) (latestTime (_statusVisPageTimeRange params)))
                                  else Nothing
                                , Just $ hxSpinner_ staticLink (fieldLink performanceHeader    (Just (_statusVisPageStationId params)) (earliestTime (_statusVisPageTimeRange params)) (latestTime (_statusVisPageTimeRange params)))
                                , valetHeader
                                , virtualHeader
                                ]
        mconcat $ map (`with` [class_ ("pure-u-md-1-" <> showt (length headers))]) headers

      br_ []

      -- Selection form
      form_ [class_ "pure-form pure-form-stacked", style_ "text-align: center"] $ fieldset_ $ do
        legend_ $ h3_ "Query Parameters"
        div_ [class_ "pure-g"] $ do -- Grid layout for form
          div_ [class_ "pure-u-1 pure-u-md-1-4"] (stationIdInput params)
          div_ [class_ "pure-u-1 pure-u-md-1-4"] (startTimeInput earliest)
          div_ [class_ "pure-u-1 pure-u-md-1-4"] (endTimeInput latest)
          div_ [class_ "pure-u-1 pure-u-md-1-4"] submitInput

      with div_ [class_ "graph"] (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal (vegaEmbedCfg ShowActions) (vegaChart (map T.pack) (_statusVisPageDataLink params))))

    where
      inf = _statusVisPageStationInfo params

      staticLink = _statusVisPageStaticLink params

      pageTitle :: Int -> T.Text -> T.Text
      pageTitle a b = "Station #" <> (T.pack . show) a <> ": "<>b

      capacityHeader :: Monad m => HtmlT m ()
      capacityHeader = div_ $ do
        label_ [for_ "capacity"] (h3_ "Capacity")
        div_ [id_ "capacity"] (showth (_infoCapacity inf) <> " docks")

        label_ [for_ "station-type"] (h3_ "Station Type")
        div_ [id_ "station-type"] (toHtml ((physicalConfigurationLabel . _infoPhysicalConfiguration) inf) <> chargingLabel)
        where chargingLabel = if _infoIsChargingStation inf
                              then " (charging)"
                              else mempty

      virtualHeader :: Monad m => Maybe (HtmlT m ())
      virtualHeader = maybeHeader (_infoIsVirtualStation inf) $
        div_ $ do
          label_ [for_ "virtual"] (h3_ "Virtual Station")
          div_ [id_ "virtual"] (toHtml (boolToText (_infoIsVirtualStation inf)))

      valetHeader :: Monad m => Maybe (HtmlT m ())
      valetHeader = maybeHeader (_infoIsValetStation inf) $
        div_ $ do
          label_ [for_ "valet"] (h3_ "Valet Station")
          div_ [id_ "valet"] (toHtml (boolToText (_infoIsValetStation inf)))

      times' = times (_statusVisPageTimeZone params) (_statusVisPageCurrentUtc params) (_statusVisPageTimeRange params)
      earliest = earliestTime times'
      latest   = latestTime   times'

stationIdInput :: Monad m => StationStatusVisualizationPage -> HtmlT m ()
stationIdInput = makeInputField "Station ID" "number" "station-id" . showt . _statusVisPageStationId
