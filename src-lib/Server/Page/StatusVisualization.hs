{-# LANGUAGE DataKinds #-}
-- |

module Server.Page.StatusVisualization
     ( ChargingEventsHeader (..)
     , DockingEventsHeader (..)
     , StatusDataParams (..)
     , boolToText
     , endTimeInput
     , enforceTimeRangeBounds
     , formatTimeHtml
     , makeInputField
     , maybeHeader
     , prettyTime
     , startTimeInput
     , submitInput
     , times
     , vegaChart
     ) where

import           Control.Lens

import           Data.Int                              ( Int32 )
import           Data.Maybe                            ( fromMaybe )
import qualified Data.Text                             as T
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.Operations
import           Database.BikeShare.StationInformation ( StationInformation )

import           GHC.Generics

import qualified Graphics.Vega.VegaLite                as VL

import           Lucid

import           Servant

import           Server.Page.Utils

import           Visualization.StationOccupancy

data DockingEventsHeader a where
  DockingEventsHeader :: { unEvents :: [DockingEventsCount] } -> DockingEventsHeader a

data ChargingEventsHeader where
  ChargingEventsHeader :: { unChargingEvents :: [(StationInformation, Int32, Int32, Int32)] } -> ChargingEventsHeader

instance ToHtml (DockingEventsHeader 'Docking) where
  toHtmlRaw = toHtml
  toHtml params =
    (\events' -> div_ $ do
        div_ [class_ "tooltip"] $ do
          label_ [ for_ "dockings"
                 , class_ "tooltip"
                 ] (h3_ "Dockings")
          div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Iconic: "   <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (iconicEvents events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (efitEvents   events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (efitG5Events events'))))
          div_ [id_ "dockings"] (showth (sumEvents Docking (allBikeEvents events')))
        ) (unEvents params)

instance ToHtml (DockingEventsHeader 'Undocking) where
  toHtmlRaw = toHtml
  toHtml params =
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
    ) (unEvents params)

instance ToHtml ChargingEventsHeader where
  toHtmlRaw = toHtml
  toHtml params =
    div_ (do
      div_ [class_ "tooltip"] $ do
        label_ [for_ "charging-count"] (h3_ "Bikes Charged")
        div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
          p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (params' ^. _2))
          p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (params' ^. _3))
      div_ [id_ "charging-count"] (showth (abs (params' ^. _1))))
    where params' = sumTuples (unChargingEvents params)

sumTuples :: Num a => [(b, a, a, a)] -> (a, a, a)
sumTuples = foldr (\(_, a1, b1, c1) (a2, b2, c2) -> (a1 + a2, b1 + b2, c1 + c2)) (0, 0, 0)

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
submitInput    = makeInputField (i_ "Or hit Enter")  "submit"         "" "Submit"

prettyTime :: LocalTime -> String
prettyTime = formatTime defaultTimeLocale "%A, %b %e, %T"

vegaChart :: ([String] -> [T.Text]) -> Link -> VL.VegaLite
vegaChart filterFn dataLink = availBikesOverTimeVL filterFn ("/" <> toUrlPiece dataLink)

boolToText :: Bool -> T.Text
boolToText True  = "Yes"
boolToText False = "No"

maybeHeader :: Monad m => Bool -> HtmlT m () -> Maybe (HtmlT m ())
maybeHeader cond expr =
  if cond
  then Just expr
  else Nothing

times :: TimeZone -> UTCTime -> TimePair (Maybe LocalTime) -> TimePair LocalTime
times tz currentUtc range  = enforceTimeRangeBounds (StatusDataParams tz currentUtc range)

data StatusDataParams a where
  StatusDataParams :: { visTimeZone :: TimeZone
                      , visCurrentUtc :: UTCTime
                      , visTimeRange :: TimePair a
                      } -> StatusDataParams a
  deriving (Show, Generic, Eq, Ord)

enforceTimeRangeBounds :: StatusDataParams (Maybe LocalTime) -> TimePair LocalTime
enforceTimeRangeBounds params = TimePair start end
  where
    tz = visTimeZone params
    currentUtc = visCurrentUtc params
    yesterday = addUTCTime (-24 * 3600) currentUtc
    earliest = earliestTime (visTimeRange params)
    latest   = latestTime   (visTimeRange params)

    -- Default to 24 hours ago -> now.
    start = fromMaybe (utcToLocalTime tz yesterday)  earliest
    end   = fromMaybe (utcToLocalTime tz currentUtc) latest
