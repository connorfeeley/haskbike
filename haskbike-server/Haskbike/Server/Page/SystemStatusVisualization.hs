{-# LANGUAGE DataKinds #-}

-- | This module defines the data types used to render the station status visualization page.

module Haskbike.Server.Page.SystemStatusVisualization
     ( SystemStatusVisualizationInfo (..)
     , SystemStatusVisualizationPage (..)
     ) where

import           Data.Default.Class
import qualified Data.Text                                as T
import qualified Data.Text.Lazy                           as TL
import           Data.Time
import           Data.Time.Extras

import           Haskbike.Graphics.Vega.VegaLite.Extra
import           Haskbike.Server.API.Components
import           Haskbike.Server.Classes
import           Haskbike.Server.ExternalAssets           ( ExternalAssetLocation )
import           Haskbike.Server.Page.SelectionForm
import           Haskbike.Server.Page.StatusVisualization
import           Haskbike.Server.Page.Utils
import           Haskbike.Server.StatusDataParams

import           Lucid

import           Servant

import           TextShow

data SystemStatusVisualizationInfo where
  SystemStatusVisualizationInfo :: { sysStatVisInfNumStations   :: Int
                                   , sysStatVisInfNumDocksAvail :: Int
                                   , sysStatVisInfNumDocksDisab :: Int
                                   , sysStatVisInfNumBikesAvail :: Int
                                   , sysStatVisInfNumBikesDisab :: Int
                                   , sysStatVisInfNumIconic     :: Int
                                   , sysStatVisInfNumEfit       :: Int
                                   , sysStatVisInfNumEfitG5     :: Int
                                   } -> SystemStatusVisualizationInfo
  deriving (Show, Eq)

instance Default SystemStatusVisualizationInfo where
  def = SystemStatusVisualizationInfo
    { sysStatVisInfNumStations   = 0
    , sysStatVisInfNumDocksAvail = 0
    , sysStatVisInfNumDocksDisab = 0
    , sysStatVisInfNumBikesAvail = 0
    , sysStatVisInfNumBikesDisab = 0
    , sysStatVisInfNumIconic     = 0
    , sysStatVisInfNumEfit       = 0
    , sysStatVisInfNumEfitG5     = 0
    }

instance ToHtml SystemStatusVisualizationInfo where
  toHtmlRaw = toHtml
  toHtml params = do
    div_ $ do
      label_ [for_ "capacity"] (h3_ "System Capacity")
      div_ [id_ "capacity"] $ do
        div_ [class_ "capacity"] $ do
          div_ $ do -- Tooltip content
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bikes available: " <> span_ [class_ "pure-u-1-2"] (showth (sysStatVisInfNumBikesAvail params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bikes disabled: "  <> span_ [class_ "pure-u-1-2"] (showth (sysStatVisInfNumBikesDisab params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Docks available: " <> span_ [class_ "pure-u-1-2"] (showth (sysStatVisInfNumDocksAvail params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Docks disabled: "  <> span_ [class_ "pure-u-1-2"] (showth (sysStatVisInfNumDocksDisab params))

            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Iconic: "   <> span_ [class_ "pure-u-1-2"] (showth (sysStatVisInfNumIconic params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (sysStatVisInfNumEfit params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (sysStatVisInfNumEfitG5 params))

data SystemStatusVisualizationPage where
  SystemStatusVisualizationPage :: { _systemStatusVisPageTimeRange      :: TimePair (Maybe LocalTime)
                                   , _systemStatusVisPageTimeZone       :: TimeZone
                                   , _systemStatusVisPageCurrentUtc     :: UTCTime
                                   , _systemStatusVisPageInfo           :: SystemStatusVisualizationInfo
                                   , _systemStatusVisPageDataLink       :: Link
                                   , _systemStatusVisPageStaticLink     :: Link
                                   , _systemStatusVisPageExternalAssets :: ExternalAssetLocation
                                   } -> SystemStatusVisualizationPage
  deriving (Show)

instance ToHtmlComponents SystemStatusVisualizationPage where
  pageAnchor _ = "#system-status"
  pageName   _ = "System Status"
  toHead _assts _ = do
    script_ [src_ . TL.toStrict . vegaUrl      $ vegaSourceUrlsLocal, defer_ mempty] ("" :: String)
    script_ [src_ . TL.toStrict . vegaLiteUrl  $ vegaSourceUrlsLocal, defer_ mempty] ("" :: String)
    script_ [src_ . TL.toStrict . vegaEmbedUrl $ vegaSourceUrlsLocal, defer_ mempty] ("" :: String)

instance ToHtml SystemStatusVisualizationPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      h1_ [] "System Status"
      h2_ [] ((toHtml . dateHeader) times')
    br_ []
    div_ [class_ "content"] $ do
      -- Informational headers
      h2_ [style_ "text-align: center"] "System Status & Statistics"
      br_ []
      div_ [class_ "pure-g full-width", style_ "text-align: center"] $ do
        let headers = [ toHtml (_systemStatusVisPageInfo params)
                      , mkHeader params _systemStatusVisPageTimeRange Nothing dockingEventsHeader
                      , mkHeader params _systemStatusVisPageTimeRange Nothing chargingEventsHeader
                      , hxSpinner_ ((fieldLink chargingInfrastructureHeader . latestTime . _systemStatusVisPageTimeRange) params)
                      ]
        mconcat $ map (`with` [class_ ("pure-u-md-1-" <> showt (length headers))]) headers

      -- Selection form
      toHtml (SelectionForm (Just "Query Parameters")
              [ TimeInput TimeInputStart (Just earliest)
              , TimeInput TimeInputEnd   (Just latest)
              , SubmitInput "Or hit Enter"
              ])

      with div_ [class_ "graph"] (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal (vegaEmbedCfg ShowActions) (vegaChart (map T.pack . drop 2) (_systemStatusVisPageDataLink params))))
      div_ $ i_ "Note: Iconic (mechanical) bikes are not displayed on the chart above since e-bike quantities are more interesting."

    where
      times' = enforceTimeRangeBounds (StatusDataParams (tz $ _systemStatusVisPageTimeRange params)
                                                        (currentUtcTime $ _systemStatusVisPageTimeRange params)
                                                        (_systemStatusVisPageTimeRange params)
                                      )
      earliest = earliestTime times'
      latest   = latestTime   times'

