-- | This module defines the data types used to render the station status visualization page.

module Server.Page.SystemStatusVisualization
     ( SystemStatusVisualizationPage (..)
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


data SystemStatusVisualizationPage where
  SystemStatusVisualizationPage :: { _systemStatusVisPageTimeRange     :: TimePair (Maybe LocalTime)
                                   , _systemStatusVisPageTimeZone      :: TimeZone
                                   , _systemStatusVisPageCurrentUtc    :: UTCTime
                                   , _systemStatusVisPageDockingEvents :: Maybe DockingEventsCount
                                   , _systemStatusVisPageChargings     :: [(StationStatus, [ChargingEvent])]
                                   -- , _systemStatusVisPageDataLink      :: Link
                                   , _systemStatusVisPageStaticLink    :: Link
                                   } -> SystemStatusVisualizationPage

instance ToHtmlComponents SystemStatusVisualizationPage where
  toMenuHeading _ = menuHeading "#visualization" "Available Bikes"

instance ToHtml SystemStatusVisualizationPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      -- h1_ [] (toHtml (pageTitle (_systemStatusVisPageStationId params) (_infoName inf)))
      h2_ [] (toHtml dateHeader)
    br_ []
    div_ [class_ "content"] $ do
      -- Informational headers
      h2_ [style_ "text-align: center"] "Station Information & Statistics"
      br_ []
      div_ [class_ "pure-g", style_ "text-align: center"] $ do
        mconcat $
          let headers = [undockingsHeader, dockingsHeader]
          in map (`with` [class_ ("pure-u-md-1-" <> showt (length headers))]) headers

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
      pageTitle :: Int -> Text -> Text
      pageTitle = format "Station #{}: {}"

      dateHeader :: Text
      dateHeader = format "{} ➜ {}"
                   (prettyTime (earliestTime times))
                   (prettyTime (latestTime times))

      undock = abs . _eventsCountUndockings
      dock = abs . _eventsCountDockings
      sumUndockings events' = abs (_eventsCountUndockings (_eventsIconicCount events') + _eventsCountUndockings (_eventsEfitCount events') + _eventsCountUndockings (_eventsEfitG5Count events'))
      sumDockings   events' = abs (_eventsCountDockings   (_eventsIconicCount events') + _eventsCountDockings (_eventsEfitCount events')   + _eventsCountDockings   (_eventsEfitG5Count events'))

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
        ) (_systemStatusVisPageDockingEvents params)

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
        ) (_systemStatusVisPageDockingEvents params)
      times = enforceTimeRangeBounds (StatusDataParams (_systemStatusVisPageTimeZone params) (_systemStatusVisPageCurrentUtc params) (_systemStatusVisPageTimeRange params))
      earliest = earliestTime times
      latest   = latestTime times

      prettyTime :: LocalTime -> String
      prettyTime = formatTime defaultTimeLocale "%A, %b %e, %T"

      -- vegaChart :: VL.VegaLite
      -- vegaChart = availBikesOverTimeVL ("/" <> toUrlPiece (_systemStatusVisPageDataLink params))

      boolToText :: Bool -> Text
      boolToText True  = "Yes"
      boolToText False = "No"

-- This helper creates an input field with the provided 'id' and 'type' attributes.
makeInputField :: Monad m => HtmlT m () -> Text -> Text -> Text -> HtmlT m ()
makeInputField f t id' val = label_ [for_ id', style_ "width: fit-content"] $ f <> input_ [type_ t, id_ (id' <> pack "-input"), name_ id', class_ "pure-input-rounded", value_ val, style_ "width: 95%"]

sumAll, sumEfit, sumEfitG5 :: SystemStatusVisualizationPage -> Int
sumAll    params = sumChargings (const True)                         (map snd (_systemStatusVisPageChargings params))
sumEfit   params = sumChargings (\c -> _chargedBikeType c == EFit)   (map snd (_systemStatusVisPageChargings params))
sumEfitG5 params = sumChargings (\c -> _chargedBikeType c == EFitG5) (map snd (_systemStatusVisPageChargings params))

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
startTimeInput, endTimeInput :: Monad m => LocalTime -> HtmlT m ()
startTimeInput  = makeInputField "Start Time"        "datetime-local" "start-time" . formatTimeHtml
endTimeInput    = makeInputField "End Time"          "datetime-local" "end-time"   . formatTimeHtml

submitInput :: Monad m => HtmlT m ()
submitInput    = makeInputField (i_ "Or hit Enter")  "submit"         "submit-form" "Submit"
