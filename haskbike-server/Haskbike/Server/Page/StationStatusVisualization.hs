{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
-- | This module defines the data types used to render the station status visualization page.

module Haskbike.Server.Page.StationStatusVisualization
     ( StationStatusVisualizationPage (..)
     ) where


import           Data.Maybe                                  ( catMaybes )
import qualified Data.Text                                   as T
import qualified Data.Text.Lazy                              as TL
import           Data.Time
import           Data.Time.Extras

import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Graphics.Vega.VegaLite.Extra
import           Haskbike.Server.Classes
import           Haskbike.Server.ComponentsAPI
import           Haskbike.Server.Page.SelectionForm
import           Haskbike.Server.Page.StatusVisualization
import           Haskbike.Server.Page.Utils

import           Lucid

import           Servant

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

  toHead _ = do
    script_ [src_ . TL.toStrict . vegaUrl      $ vegaSourceUrlsLocal] ("" :: String)
    script_ [src_ . TL.toStrict . vegaLiteUrl  $ vegaSourceUrlsLocal] ("" :: String)
    script_ [src_ . TL.toStrict . vegaEmbedUrl $ vegaSourceUrlsLocal] ("" :: String)

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
      div_ [class_ "pure-g full-width", style_ "text-align: center"] $ do
        mconcat $ map (`with` [class_ ("pure-u-md-1-" <> (showt . length) headers)]) headers
      br_ []

      -- Selection form
      toHtml (SelectionForm "Query Parameters"
              [ StationIdInput ((Just . _statusVisPageStationId) params)
              , TimeInput TimeInputStart (Just earliest)
              , TimeInput TimeInputEnd   (Just latest)
              , SubmitInput "Or hit enter"
              ])

      with div_ [class_ "graph"] (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal (vegaEmbedCfg ShowActions) (vegaChart (map T.pack) (_statusVisPageDataLink params))))

    where
      headers = catMaybes [ Just capacityHeader
                          , Just (mkHeader params _statusVisPageStaticLink _statusVisPageTimeRange (Just (_statusVisPageStationId params)) dockingEventsHeader)
                          , if _infoIsChargingStation inf then Just chargingHeader else Nothing
                          , Just (mkHeader params _statusVisPageStaticLink _statusVisPageTimeRange (Just (_statusVisPageStationId params)) performanceHeader)
                          , valetHeader
                          , virtualHeader
                          ]
      chargingHeader = mkHeader params _statusVisPageStaticLink _statusVisPageTimeRange (Just (_statusVisPageStationId params)) chargingEventsHeader

      inf = _statusVisPageStationInfo params

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
