{-# LANGUAGE DataKinds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.SystemStatusVisualization
     ( SystemStatusVisualizationInfo (..)
     , SystemStatusVisualizationPage (..)
     ) where


import           Data.Default.Class
import qualified Data.Text                       as T
import           Data.Time
import           Data.Time.Extras

import           Graphics.Vega.VegaLite.Extra

import           Lucid

import           Servant

import           Server.Classes
import           Server.ComponentsAPI
import           Server.Page.StatusVisualization
import           Server.Page.Utils
import           Server.StatusDataParams

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
  SystemStatusVisualizationPage :: { _systemStatusVisPageTimeRange     :: TimePair (Maybe LocalTime)
                                   , _systemStatusVisPageTimeZone      :: TimeZone
                                   , _systemStatusVisPageCurrentUtc    :: UTCTime
                                   , _systemStatusVisPageInfo          :: SystemStatusVisualizationInfo
                                   , _systemStatusVisPageDataLink      :: Link
                                   , _systemStatusVisPageStaticLink    :: Link
                                   } -> SystemStatusVisualizationPage
  deriving (Show)

instance ToHtmlComponents SystemStatusVisualizationPage where
  toMenuHeading _ = menuHeading "#visualization" "System Status"

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
                      , hxSpinner_ staticLink (fieldLink dockingEventsHeader  Nothing (earliestTime $ _systemStatusVisPageTimeRange params) (latestTime $ _systemStatusVisPageTimeRange params))
                      , hxSpinner_ staticLink (fieldLink chargingEventsHeader Nothing (earliestTime $ _systemStatusVisPageTimeRange params) (latestTime $ _systemStatusVisPageTimeRange params))
                      , hxSpinner_ staticLink (fieldLink chargingInfrastructureHeader (latestTime   $ _systemStatusVisPageTimeRange params))
                      ]
        mconcat $ map (`with` [class_ ("pure-u-md-1-" <> showt (length headers))]) headers

      -- Selection form
      form_ [class_ "pure-form pure-form-stacked", style_ "text-align: center"] $ fieldset_ $ do
        legend_ $ h3_ "Query Parameters"
        div_ [class_ "pure-g full-width"] $ do -- Grid layout for form
          div_ [class_ "pure-u-1 pure-u-md-1-3"] (startTimeInput earliest)
          div_ [class_ "pure-u-1 pure-u-md-1-3"] (endTimeInput latest)
          div_ [class_ "pure-u-1 pure-u-md-1-3"] submitInput

      with div_ [class_ "graph"] (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal (vegaEmbedCfg ShowActions) (vegaChart (map T.pack . drop 2) (_systemStatusVisPageDataLink params))))
      div_ $ i_ "Note: Iconic (mechanical) bikes are not displayed on the chart above since e-bike quantities are more interesting."

    where
      staticLink = _systemStatusVisPageStaticLink params

      times' = enforceTimeRangeBounds (StatusDataParams (tz $ _systemStatusVisPageTimeRange params)
                                                        (currentUtcTime $ _systemStatusVisPageTimeRange params)
                                                        (_systemStatusVisPageTimeRange params)
                                      )
      earliest = earliestTime times'
      latest   = latestTime   times'

