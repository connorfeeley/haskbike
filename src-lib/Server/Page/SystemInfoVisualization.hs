{-# LANGUAGE DataKinds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.SystemInfoVisualization
     ( SystemInfoVisualizationInfo (..)
     , SystemInfoVisualizationPage (..)
     , vegaSourceUrlsCdn
     , vegaSourceUrlsLocal
     ) where


import           Data.Default.Class
import qualified Data.Text                       as T
import           Data.Time
import           Data.Time.Extras

import           Fmt

import           Graphics.Vega.VegaLite.Extra

import           Lucid

import           Servant

import           Server.Classes
import           Server.ComponentsAPI
import           Server.Page.StatusVisualization
import           Server.Page.Utils
import           Server.StatusDataParams

import           TextShow

import           Visualization.StationOccupancy

data SystemInfoVisualizationInfo where
  SystemInfoVisualizationInfo :: { sysInfoVisInfNumStations   :: Int
                                 , sysInfoVisInfNumDocksAvail :: Int
                                 , sysInfoVisInfNumDocksDisab :: Int
                                 , sysInfoVisInfNumBikesAvail :: Int
                                 , sysInfoVisInfNumBikesDisab :: Int
                                 , sysInfoVisInfNumIconic     :: Int
                                 , sysInfoVisInfNumEfit       :: Int
                                 , sysInfoVisInfNumEfitG5     :: Int
                                 } -> SystemInfoVisualizationInfo
  deriving (Show, Eq)

instance Default SystemInfoVisualizationInfo where
  def = SystemInfoVisualizationInfo
    { sysInfoVisInfNumStations   = 0
    , sysInfoVisInfNumDocksAvail = 0
    , sysInfoVisInfNumDocksDisab = 0
    , sysInfoVisInfNumBikesAvail = 0
    , sysInfoVisInfNumBikesDisab = 0
    , sysInfoVisInfNumIconic     = 0
    , sysInfoVisInfNumEfit       = 0
    , sysInfoVisInfNumEfitG5     = 0
    }

instance ToHtml SystemInfoVisualizationInfo where
  toHtmlRaw = toHtml
  toHtml params = do
    div_ $ do
      label_ [for_ "capacity"] (h3_ "System Capacity")
      div_ [id_ "capacity"] $ do
        div_ [class_ "capacity"] $ do
          div_ $ do -- Tooltip content
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bikes available: " <> span_ [class_ "pure-u-1-2"] (showth (sysInfoVisInfNumBikesAvail params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Bikes disabled: "  <> span_ [class_ "pure-u-1-2"] (showth (sysInfoVisInfNumBikesDisab params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Docks available: " <> span_ [class_ "pure-u-1-2"] (showth (sysInfoVisInfNumDocksAvail params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Docks disabled: "  <> span_ [class_ "pure-u-1-2"] (showth (sysInfoVisInfNumDocksDisab params))

            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "Iconic: "   <> span_ [class_ "pure-u-1-2"] (showth (sysInfoVisInfNumIconic params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (sysInfoVisInfNumEfit params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (sysInfoVisInfNumEfitG5 params))

data SystemInfoVisualizationPage where
  SystemInfoVisualizationPage :: { _sysInfoVisPageTimeRange     :: TimePair (Maybe LocalTime)
                                 , _sysInfoVisPageTimeZone      :: TimeZone
                                 , _sysInfoVisPageCurrentUtc    :: UTCTime
                                 , _sysInfoVisPageInfo          :: SystemInfoVisualizationInfo
                                 , _sysInfoVisPageDataLink      :: Link
                                 , _sysInfoVisPageStaticLink    :: Link
                                 } -> SystemInfoVisualizationPage
  deriving (Show)

instance ToHtmlComponents SystemInfoVisualizationPage where
  toMenuHeading _ = menuHeading "#visualization" "Available Bikes"

instance ToHtml SystemInfoVisualizationPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      h1_ [] "System Information"
      h2_ [] (toHtml dateHeader)
    br_ []
    div_ [class_ "content"] $ do
      -- Informational headers
      h2_ [style_ "text-align: center"] "System Information"
      br_ []
      div_ [class_ "pure-g", style_ "text-align: center"] $ do
        let headers = [ toHtml (_sysInfoVisPageInfo params)
                      , hxSpinner_ staticLink (fieldLink dockingEventsHeader  Nothing (earliestTime $ _sysInfoVisPageTimeRange params) (latestTime $ _sysInfoVisPageTimeRange params))
                      , hxSpinner_ staticLink (fieldLink chargingEventsHeader Nothing (earliestTime $ _sysInfoVisPageTimeRange params) (latestTime $ _sysInfoVisPageTimeRange params))
                      ]
        mconcat $ map (`with` [class_ ("pure-u-md-1-" <> showt (length headers))]) headers

      -- Selection form
      form_ [class_ "pure-form pure-form-stacked", style_ "text-align: center"] $ fieldset_ $ do
        legend_ $ h3_ "Query Parameters"
        div_ [class_ "pure-g"] $ do -- Grid layout for form
          div_ [class_ "pure-u-1 pure-u-md-1-3"] (startTimeInput earliest)
          div_ [class_ "pure-u-1 pure-u-md-1-3"] (endTimeInput latest)
          div_ [class_ "pure-u-1 pure-u-md-1-3"] submitInput

      with div_ [class_ "graph"] (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal (vegaEmbedCfg ShowActions) (vegaChart (map T.pack . drop 2) (_sysInfoVisPageDataLink params))))
      div_ $ i_ "Note: Iconic (mechanical) bikes are not displayed on the chart above since e-bike quantities are more interesting."

    where
      staticLink = _sysInfoVisPageStaticLink params

      dateHeader :: T.Text
      dateHeader = format "{} ➜ {}"
                   (prettyTime (earliestTime times'))
                   (prettyTime (latestTime   times'))

      times' = enforceTimeRangeBounds (StatusDataParams (tz $ _sysInfoVisPageTimeRange params)
                                                        (currentUtcTime $ _sysInfoVisPageTimeRange params)
                                                        (_sysInfoVisPageTimeRange params)
                                      )
      earliest = earliestTime times'
      latest   = latestTime   times'

