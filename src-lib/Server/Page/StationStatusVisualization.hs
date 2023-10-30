-- | This module defines the data types used to render the station status visualization page.

module Server.Page.StationStatusVisualization
     ( StationStatusVisualizationPage (..)
     , vegaSourceUrlsCdn
     , vegaSourceUrlsLocal
     ) where


import           API.Types                              ( TorontoVehicleType (..) )

import           Control.Monad                          ( when )

import           Data.Aeson
import           Data.Text                              hiding ( concat, filter, intersperse, length, map )
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare                     ( StationStatus )
import           Database.BikeShare.Operations          ( ChargingEvent (..), DockingEventsCount (..),
                                                          EventsCountResult (..) )
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
        mconcat $
          let headers = [capacityHeader, undockingsHeader, dockingsHeader, chargingHeader, chargingsHeader]
          in map (`with` [class_ ("pure-u-md-1-" <> showt (length headers))]) headers

      br_ []

      -- Selection form
      form_ [class_ "pure-form pure-form-stacked", style_ "text-align: center"] $ fieldset_ $ do
        legend_ $ h3_ "Query Parameters"
        div_ [class_ "pure-g"] $ do -- Grid layout for form
          div_ [class_ "pure-u-1 pure-u-md-1-4"] (stationIdInput params)
          div_ [class_ "pure-u-1 pure-u-md-1-4"] (startTimeInput earliest)
          div_ [class_ "pure-u-1 pure-u-md-1-4"] (endTimeInput latest)
          div_ [class_ "pure-u-1 pure-u-md-1-4"] submitInput

      with div_ [class_ "graph"] (toHtmlRaw (toHtmlWithUrls vegaSourceUrlsLocal (vegaEmbedCfg HideActions) vegaChart))

    where
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
        div_ [id_ "capacity"] (showth (_infoCapacity inf) <> " docks")


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
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Iconic: "   <> span_ [class_ "pure-u-1-2"] (showth (undock (_eventsIconicCount events'))))
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (undock (_eventsEfitCount   events'))))
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (undock (_eventsEfitG5Count events'))))
            div_ [id_ "undockings"] (showth (sumUndockings events'))
        ) (_statusVisPageDockingEvents params)

      dockingsHeader :: Monad m => HtmlT m ()
      dockingsHeader = maybe mempty
        (\events' -> div_ $ do
            div_ [class_ "tooltip"] $ do
              label_ [ for_ "dockings"
                     , class_ "tooltip"
                     ] (h3_ "Dockings")
              div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Iconic: "   <> span_ [class_ "pure-u-1-2"] (showth (dock (_eventsIconicCount events'))))
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (dock (_eventsEfitCount   events'))))
                p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (dock (_eventsEfitG5Count events'))))
            div_ [id_ "dockings"] (showth (sumDockings events'))
        ) (_statusVisPageDockingEvents params)

      chargingHeader :: Monad m => HtmlT m ()
      chargingHeader = div_ $ do
        label_ [for_ "charging"] (h3_ "Charging Station")
        div_ [id_ "charging"] (toHtml (boolToText (_infoIsChargingStation inf)))

      chargingsHeader :: Monad m => HtmlT m ()
      chargingsHeader = when (_infoIsChargingStation inf) $ div_ $ do
        div_ [class_ "tooltip"] $ do
          label_ [for_ "charging-count"] (h3_ "Bikes Charged")
          div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEfit   params))
            p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (sumEfitG5 params))
        div_ [id_ "charging-count"] (showth (sumAll params))

      times = enforceTimeRangeBounds (StatusDataParams (_statusVisPageTimeZone params) (_statusVisPageCurrentUtc params) (_statusVisPageTimeRange params))
      earliest = earliestTime times
      latest   = latestTime times

      prettyTime :: LocalTime -> String
      prettyTime = formatTime defaultTimeLocale "%A, %b %e, %T"

      vegaChart :: VL.VegaLite
      vegaChart = availBikesOverTimeVL ("/" <> toUrlPiece (_statusVisPageDataLink params))

      boolToText :: Bool -> Text
      boolToText True  = "Yes"
      boolToText False = "No"

-- This helper creates an input field with the provided 'id' and 'type' attributes.
makeInputField :: Monad m => HtmlT m () -> Text -> Text -> Text -> HtmlT m ()
makeInputField f t id' val = label_ [for_ id', style_ "width: fit-content"] $ f <> input_ [type_ t, id_ (id' <> pack "-input"), name_ id', class_ "pure-input-rounded", value_ val, style_ "width: 95%"]

sumAll, sumEfit, sumEfitG5 :: StationStatusVisualizationPage -> Int
sumAll    params = sumChargings (const True)                         (map snd (_statusVisPageChargings params))
sumEfit   params = sumChargings (\c -> _chargedBikeType c == EFit)   (map snd (_statusVisPageChargings params))
sumEfitG5 params = sumChargings (\c -> _chargedBikeType c == EFitG5) (map snd (_statusVisPageChargings params))

-- | Sum number of chargings (for a given filter condition).
sumChargings :: (ChargingEvent -> Bool) -> [[ChargingEvent]] -> Int
sumChargings cond chargings = sumBikes (filter cond (concat chargings))
  where
    sumBikes = sum . map _chargedBikeNumber

data ShowVegaActions = ShowActions | HideActions

vegaEmbedCfg :: ShowVegaActions -> Maybe Value
vegaEmbedCfg showActions =
  Just (toJSON (object [ ("logLevel", "4")
                        , ("$schema", "/static/js/vega/schema/vega-lite/v4.json")
                        , ("actions", actionToBool showActions)
                        ]))
  where
    actionToBool action = case action of ShowActions -> Bool True
                                         HideActions -> Bool False

formatTimeHtml :: LocalTime -> Text
formatTimeHtml = pack . formatTime defaultTimeLocale htmlTimeFormat

-- * Input helpers
stationIdInput :: Monad m => StationStatusVisualizationPage -> HtmlT m ()
stationIdInput = makeInputField "Station ID"        "number"          "station-id" . showt . _statusVisPageStationId

startTimeInput, endTimeInput :: Monad m => LocalTime -> HtmlT m ()
startTimeInput  = makeInputField "Start Time"        "datetime-local" "start-time" . formatTimeHtml
endTimeInput    = makeInputField "End Time"          "datetime-local" "end-time"   . formatTimeHtml

submitInput :: Monad m => HtmlT m ()
submitInput    = makeInputField (i_ "Or hit Enter")  "submit"         "submit-form" "Submit"
