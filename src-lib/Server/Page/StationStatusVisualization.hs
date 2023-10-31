-- | This module defines the data types used to render the station status visualization page.

module Server.Page.StationStatusVisualization
     ( StationStatusVisualizationPage (..)
     , vegaSourceUrlsCdn
     , vegaSourceUrlsLocal
     ) where


import           Data.Maybe                             ( catMaybes )
import qualified Data.Text                              as T
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare                     ( StationStatus )
import           Database.BikeShare.Operations
import           Database.BikeShare.StationInformation

import           Fmt

import qualified Graphics.Vega.VegaLite                 as VL
import           Graphics.Vega.VegaLite.Extra

import           Lucid

import           Servant

import           Server.Classes
import           Server.Data.StationStatusVisualization
import           Server.Page.Utils

import           TextShow

import           Visualization.StationOccupancy


data StationStatusVisualizationPage where
  StationStatusVisualizationPage :: { _statusVisPageStationInfo   :: StationInformation
                                    , _statusVisPageStationId     :: Int
                                    , _statusVisPageTimeRange     :: TimePair (Maybe LocalTime)
                                    , _statusVisPageTimeZone      :: TimeZone
                                    , _statusVisPageCurrentUtc    :: UTCTime
                                    , _statusVisPageDockingEvents :: [DockingEventsCount]
                                    , _statusVisPageChargings     :: [(StationStatus, [ChargingEvent])]
                                    , _statusVisPageDataLink      :: Link
                                    , _statusVisPageStaticLink    :: Link
                                    } -> StationStatusVisualizationPage

instance ToHtmlComponents StationStatusVisualizationPage where
  toMenuHeading _ = menuHeading "#visualization" "Available Bikes"

instance ToHtml StationStatusVisualizationPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      h1_ [] (toHtml (pageTitle (_statusVisPageStationId params) (_infoName inf)))
      h2_ [] (toHtml dateHeader)
    br_ []
    div_ [class_ "content"] $ do
      -- Informational headers
      h2_ [style_ "text-align: center"] "Station Information & Statistics"
      br_ []
      div_ [class_ "pure-g", style_ "text-align: center"] $ do
        let headers = catMaybes [Just capacityHeader, Just undockingsHeader, Just dockingsHeader, Just chargingHeader, Just chargingsHeader, valetHeader, virtualHeader]
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

      with div_ [class_ "graph"] (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal (vegaEmbedCfg ShowActions) vegaChart))

    where
      inf = _statusVisPageStationInfo params

      pageTitle :: Int -> T.Text -> T.Text
      pageTitle = format "Station #{}: {}"

      dateHeader :: T.Text
      dateHeader = format "{} ➜ {}"
                   (prettyTime (earliestTime times))
                   (prettyTime (latestTime times))
      capacityHeader :: Monad m => HtmlT m ()
      capacityHeader = div_ $ do
        label_ [for_ "capacity"] (h3_ "Capacity")
        div_ [id_ "capacity"] (showth (_infoCapacity inf) <> " docks")


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
        ) (_statusVisPageDockingEvents params)

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
        ) (_statusVisPageDockingEvents params)

      chargingsHeader :: Monad m => HtmlT m ()
      chargingsHeader =
        div_ (do
          div_ [class_ "tooltip"] $ do
            label_ [for_ "charging-count"] (h3_ "Bikes Charged")
            div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
              p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEfitCharging   (_statusVisPageChargings params)))
              p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (sumEfitG5Charging (_statusVisPageChargings params)))
          div_ [id_ "charging-count"] (showth (sumAllCharging (_statusVisPageChargings params))))

      chargingHeader :: Monad m => HtmlT m ()
      chargingHeader = div_ $ do
        label_ [for_ "charging"] (h3_ "Charging Station")
        div_ [id_ "charging"] (toHtml (boolToText (_infoIsChargingStation inf)))

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

      maybeHeader :: Monad m => Bool -> HtmlT m () -> Maybe (HtmlT m ())
      maybeHeader cond expr =
        if cond
        then Just expr
        else Nothing

      times = enforceTimeRangeBounds (StatusDataParams (_statusVisPageTimeZone params) (_statusVisPageCurrentUtc params) (_statusVisPageTimeRange params))
      earliest = earliestTime times
      latest   = latestTime times

      prettyTime :: LocalTime -> String
      prettyTime = formatTime defaultTimeLocale "%A, %b %e, %T"

      vegaChart :: VL.VegaLite
      vegaChart = availBikesOverTimeVL ("/" <> toUrlPiece (_statusVisPageDataLink params))

      boolToText :: Bool -> T.Text
      boolToText True  = "Yes"
      boolToText False = "No"

-- This helper creates an input field with the provided 'id' and 'type' attributes.
makeInputField :: Monad m => HtmlT m () -> T.Text -> T.Text -> T.Text -> HtmlT m ()
makeInputField f t id' val = label_ [for_ id', style_ "width: fit-content"] $ f <> input_ [type_ t, id_ (id' <> T.pack "-input"), name_ id', class_ "pure-input-rounded", value_ val, style_ "width: 95%"]

formatTimeHtml :: LocalTime -> T.Text
formatTimeHtml = T.pack . formatTime defaultTimeLocale htmlTimeFormat

-- * Input helpers
stationIdInput :: Monad m => StationStatusVisualizationPage -> HtmlT m ()
stationIdInput = makeInputField "Station ID"        "number"          "station-id" . showt . _statusVisPageStationId

startTimeInput, endTimeInput :: Monad m => LocalTime -> HtmlT m ()
startTimeInput  = makeInputField "Start Time"        "datetime-local" "start-time" . formatTimeHtml
endTimeInput    = makeInputField "End Time"          "datetime-local" "end-time"   . formatTimeHtml

submitInput :: Monad m => HtmlT m ()
submitInput    = makeInputField (i_ "Or hit Enter")  "submit"         "submit-form" "Submit"
