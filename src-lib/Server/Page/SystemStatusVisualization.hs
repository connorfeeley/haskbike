-- | This module defines the data types used to render the station status visualization page.

module Server.Page.SystemStatusVisualization
     ( SystemStatusVisualizationPage (..)
     , vegaSourceUrlsCdn
     , vegaSourceUrlsLocal
     ) where


import           Data.Maybe                             ( catMaybes )
import qualified Data.Text                              as T
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare                     ( StationStatus )
import           Database.BikeShare.Operations

import           Fmt

import           Graphics.Vega.VegaLite.Extra

import           Lucid

import           Servant

import           Server.Classes
import           Server.Data.StationStatusVisualization
import           Server.Page.Utils

import           TextShow

import           Visualization.StationOccupancy

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
        let headers = catMaybes [Just undockingsHeader, Just dockingsHeader, Just chargingsHeader]
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
                   (prettyTime (earliestTime times))
                   (prettyTime (latestTime times))

      undockingsHeader :: Monad m => HtmlT m ()
      undockingsHeader =
        (\events' -> div_ $ do
            div_ [class_ "tooltip"] $ do
              label_ [ for_ "undockings"
                     , class_ "tooltip"
                     ] (h3_ "Undockings")
              div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Iconic: "   <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (iconicEvents events'))))
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (efitEvents   events'))))
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (efitG5Events events'))))
            div_ [id_ "undockings"] (showth (sumEvents Undocking (allBikeEvents events')))
        ) (_systemStatusVisPageDockingEvents params)

      dockingsHeader :: Monad m => HtmlT m ()
      dockingsHeader =
        (\events' -> div_ $ do
            div_ [class_ "tooltip"] $ do
              label_ [ for_ "undockings"
                     , class_ "tooltip"
                     ] (h3_ "Undockings")
              div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Iconic: "   <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (iconicEvents events'))))
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (efitEvents   events'))))
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (efitG5Events events'))))
            div_ [id_ "undockings"] (showth (sumEvents Docking (allBikeEvents events')))
        ) (_systemStatusVisPageDockingEvents params)

      chargingsHeader :: Monad m => HtmlT m ()
      chargingsHeader =
        div_ (do
          div_ [class_ "tooltip"] $ do
            label_ [for_ "charging-count"] (h3_ "Bikes Charged")
            div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
              p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEfitCharging   (_systemStatusVisPageChargings params)))
              p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (sumEfitG5Charging (_systemStatusVisPageChargings params)))
          div_ [id_ "charging-count"] (showth (sumAllCharging (_systemStatusVisPageChargings params))))

      times = enforceTimeRangeBounds (StatusDataParams (_systemStatusVisPageTimeZone params) (_systemStatusVisPageCurrentUtc params) (_systemStatusVisPageTimeRange params))
      earliest = earliestTime times
      latest   = latestTime times

      prettyTime :: LocalTime -> String
      prettyTime = formatTime defaultTimeLocale "%A, %b %e, %T"

      -- vegaChart :: VL.VegaLite
      -- vegaChart = availBikesOverTimeVL ("/" <> toUrlPiece (_systemStatusVisPageDataLink params))

-- This helper creates an input field with the provided 'id' and 'type' attributes.
makeInputField :: Monad m => HtmlT m () -> T.Text -> T.Text -> T.Text -> HtmlT m ()
makeInputField f t id' val = label_ [for_ id', style_ "width: fit-content"] $ f <> input_ [type_ t, id_ (id' <> T.pack "-input"), name_ id', class_ "pure-input-rounded", value_ val, style_ "width: 95%"]

formatTimeHtml :: LocalTime -> T.Text
formatTimeHtml = T.pack . formatTime defaultTimeLocale htmlTimeFormat

-- * Input helpers
startTimeInput, endTimeInput :: Monad m => LocalTime -> HtmlT m ()
startTimeInput  = makeInputField "Start Time"        "datetime-local" "start-time" . formatTimeHtml
endTimeInput    = makeInputField "End Time"          "datetime-local" "end-time"   . formatTimeHtml

submitInput :: Monad m => HtmlT m ()
submitInput    = makeInputField (i_ "Or hit Enter")  "submit"         "submit-form" "Submit"
