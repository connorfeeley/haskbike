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
import           Data.Text                              hiding ( intersperse, length, map )
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
import           Server.Page.Utils

import           TextShow

import           Visualization.StationOccupancy


data StationStatusVisualizationPage where
  StationStatusVisualizationPage :: { _statusVisPageStationInfo   :: StationInformation
                                    , _statusVisPageStationId     :: Int
                                    , _statusVisPageTimeRange     :: TimePair (Maybe LocalTime)
                                    , _statusVisPageTimeZone      :: TimeZone
                                    , _statusVisPageCurrentUtc    :: UTCTime
                                    , _statusVisPageDockingEvents :: Maybe DockingEventsCount
                                    , _statusVisPageChargings     :: Maybe (Int, Int, Int)
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
    br_ []
    div_ [class_ "content"] $ do
      -- Informational headers
      h2_ [style_ "text-align: center"] "Station Information & Statistics"
      br_ []
      div_ [class_ "pure-g", style_ "text-align: center"] $ do
        mconcat $
          let headers = [capacityHeader, undockingsHeader, dockingsHeader, chargingHeader, chargingsHeader]
          in map (`with` [class_ ("pure-u-md-1-" <> showt (length headers))]) headers

      br_ []

      -- Selection form
      form_ [class_ "pure-form pure-form-stacked", style_ "text-align: center"] $ fieldset_ $ do
        legend_ $ h3_ "Query Parameters"
        div_ [class_ "pure-g"] $ do

          div_ [class_ "pure-u-1 pure-u-md-1-4"] (makeInputField "Station ID" "number" "station-id" (showt $ _statusVisPageStationId params))
          div_ [class_ "pure-u-1 pure-u-md-1-4"] (makeInputField "Start Time" "datetime-local" "start-time" (pack (formatTimeHtml earliest)))

          div_ [class_ "pure-u-1 pure-u-md-1-4"] (makeInputField "End Time" "datetime-local" "end-time" (pack (formatTimeHtml latest)))

          div_ [class_ "pure-u-1 pure-u-md-1-4"] (makeInputField (i_ "Or hit Enter") "submit" "submit-form" "Submit")

      with div_ [class_ "graph"] (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal vegaEmbedCfg vegaChart))

    where
      _dataSourceSegments :: [Text] = [ "/data", "station-status", showt 7001]

      inf = _statusVisPageStationInfo params

      pageTitle :: Int -> Text -> Text
      pageTitle = format "Station #{}: {}"

      dateHeader :: Text
      dateHeader = format "{} ➜ {}"
                   (prettyTime (earliestTime times))
                   (prettyTime (latestTime times))
      capacityHeader :: Monad m => HtmlT m ()
      capacityHeader = div_ $ do
        label_ [for_ "capacity"] (h3_ "Capacity")
        div_ [id_ "capacity"] (toHtml (showt (_infoCapacity inf) <> " docks"))


      undock = abs . _eventsCountUndockings
      dock = abs . _eventsCountDockings
      sumUndockings events' = abs (_eventsCountUndockings (_eventsIconicCount events') + _eventsCountUndockings (_eventsEfitCount events') + _eventsCountUndockings (_eventsEfitG5Count events'))
      sumDockings   events' = abs (_eventsCountDockings   (_eventsIconicCount events')   + _eventsCountDockings (_eventsEfitCount events')   + _eventsCountDockings (_eventsEfitG5Count events'))

      undockingsHeader :: Monad m => HtmlT m ()
      undockingsHeader = maybe mempty
        (\events' -> div_ $ do
            div_ [class_ "tooltip"] $ do
              label_ [ for_ "undockings"
                     , class_ "tooltip"
                     ] (h3_ "Undockings")
              div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
                p_ (b_ "Iconic: "   <> toHtml (showt (undock (_eventsIconicCount events'))))
                p_ (b_ "E-Fit: "    <> toHtml (showt (undock (_eventsEfitCount   events'))))
                p_ (b_ "E-Fit G5: " <> toHtml (showt (undock (_eventsEfitG5Count events'))))
            div_ [id_ "undockings"] (toHtml (showt (sumUndockings events')))
        ) (_statusVisPageDockingEvents params)

      dockingsHeader :: Monad m => HtmlT m ()
      dockingsHeader = maybe mempty
        (\events' -> div_ $ do
            div_ [class_ "tooltip"] $ do
              label_ [ for_ "dockings"
                     , class_ "tooltip"
                     ] (h3_ "Dockings")
              div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
                p_ (b_ "Iconic: "   <> toHtml (showt (dock (_eventsIconicCount events'))))
                p_ (b_ "E-Fit: "    <> toHtml (showt (dock (_eventsEfitCount   events'))))
                p_ (b_ "E-Fit G5: " <> toHtml (showt (dock (_eventsEfitG5Count events'))))
            div_ [id_ "dockings"] (toHtml (showt (sumDockings events')))
        ) (_statusVisPageDockingEvents params)

      chargingHeader :: Monad m => HtmlT m ()
      chargingHeader = div_ $ do
        label_ [for_ "charging"] (h3_"Charging station")
        div_ [id_ "charging"] (toHtml (boolToText (_infoIsChargingStation inf)))

      chargingsHeader :: Monad m => HtmlT m ()
      chargingsHeader = maybe mempty
        (\chargings -> div_ $ do
            label_ [for_ "charging-count"] (h3_"Bikes charged")
            div_ [id_ "charging-count"] (toHtml (showt (abs (chargings ^. _1)))))
        (_statusVisPageChargings params)

      times = enforceTimeRangeBounds (StatusDataParams (_statusVisPageTimeZone params) (_statusVisPageCurrentUtc params) (_statusVisPageTimeRange params))
      earliest = earliestTime times
      latest   = latestTime times

      prettyTime :: LocalTime -> String
      prettyTime = formatTime defaultTimeLocale "%A, %b %e, %T"
      formatTimeHtml = formatTime defaultTimeLocale htmlTimeFormat

      vegaEmbedCfg :: Maybe Value
      vegaEmbedCfg =  Just (toJSON $ object [ ("logLevel", "4")
                                            , ("$schema", "/static/js/vega/schema/vega-lite/v4.json")
                                            , ("actions", Bool False)
                                            ])

      vegaChart :: VL.VegaLite
      vegaChart = availBikesOverTimeVL ("/" <> toUrlPiece (_statusVisPageDataLink params))

      staticPath :: Text
      staticPath = "/" <> toUrlPiece (_statusVisPageStaticLink params)

      boolToText :: Bool -> Text
      boolToText True  = "Yes"
      boolToText False = "No"

instance ToHtmlComponents StationStatusVisualizationPage where
  toMenuHeading _ = menuHeading "#visualization" "Available Bikes"

-- This helper creates an input field with the provided 'id' and 'type' attributes.
makeInputField :: Monad m => HtmlT m () -> Text -> Text -> Text -> HtmlT m ()
makeInputField f t id' val = label_ [for_ id', style_ "width: fit-content"] $ f <> input_ [type_ t, id_ (id' <> pack "-input"), name_ id', class_ "pure-input-rounded", value_ val, style_ "width: 95%"]
