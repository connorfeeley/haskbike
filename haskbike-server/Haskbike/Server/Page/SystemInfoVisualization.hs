{-# LANGUAGE DataKinds #-}

-- | This module defines the data types used to render the system information visualization page.

module Haskbike.Server.Page.SystemInfoVisualization
     ( SystemInfoVisualizationPage (..)
     ) where


import qualified Data.Text.Lazy                           as TL
import           Data.Time
import           Data.Time.Extras

import           Graphics.Vega.VegaLite                   hiding ( Number, toHtml )

import           Haskbike.Graphics.Vega.VegaLite.Extra
import           Haskbike.Server.Classes
import           Haskbike.Visualization.SystemInformation

import           Lucid

import           Servant

data SystemInfoVisualizationPage where
  SystemInfoVisualizationPage :: { _sysInfoVisPageTimeRange     :: TimePair (Maybe LocalTime)
                                 , _sysInfoVisPageTimeZone      :: TimeZone
                                 , _sysInfoVisPageCurrentUtc    :: UTCTime
                                 , _sysInfoVisPageDataLink      :: Link
                                 , _sysInfoVisPageStaticLink    :: Link
                                 , _sysInfoVisPageSysStatusLink :: Link
                                 } -> SystemInfoVisualizationPage
  deriving (Show)

instance ToHtmlComponents SystemInfoVisualizationPage where
  toMenuHeading _ = menuHeading "#system-information" "System Info"

  toHead _ = do
    script_ [src_ . TL.toStrict . vegaUrl      $ vegaSourceUrlsLocal, defer_ mempty] ("" :: String)
    script_ [src_ . TL.toStrict . vegaLiteUrl  $ vegaSourceUrlsLocal, defer_ mempty] ("" :: String)
    script_ [src_ . TL.toStrict . vegaEmbedUrl $ vegaSourceUrlsLocal, defer_ mempty] ("" :: String)

instance ToHtml SystemInfoVisualizationPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      h1_ [] "System Information"
    br_ []
    div_ [class_ "content"] $ do
      -- Informational headers
      div_ [] $ do
        p_ "The Bike Share Toronto API reports a few interesting (nonstandard) fields:"
        ul_ [style_ "margin-left: 3em;"] $ do
          li_ "Station count;"
          li_ "Mechanical (bike) count; and"
          li_ "E-bike count"
        p_ "It is not clear what these are meant to indicate. The station count appears accurate, yet the mechanical and e-bike counts don't make sense."
        p_ ("For a theory of what these values might be, see "  <> a_ [href_ "https://www.cfeeley.org/posts/city-stuff/freedom-of-information/discrepancies-in-the-api.html"] "this post" <> " on my blog.")

      with div_ [class_ "graph"] $ toHtmlRaw $ toHtmlWithUrls vegaSourceUrlsLocal (vegaEmbedCfg ShowActions) $
        toVegaLite $
        systemInformationProps
                "System Information"
                ("/" <> toUrlPiece (_sysInfoVisPageDataLink params))
                "Reported"
