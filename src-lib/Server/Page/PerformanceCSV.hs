-- |

module Server.Page.PerformanceCSV where

import qualified Data.Text                       as T
import           Data.Time
import           Data.Time.Extras

import           Lucid

import           Servant

import           Server.Classes
import           Server.Page.StatusVisualization
import           Server.StatusDataParams


data PerformanceCSV where
  PerformanceCSV :: { performanceCsvPageTimeRange     :: TimePair (Maybe LocalTime)
                    , performanceCsvPageTimeZone      :: TimeZone
                    , performanceCsvPageCurrentUtc    :: UTCTime
                    , performanceCsvPageDataLink      :: Link
                    , performanceCsvPageStaticLink    :: Link
                    } -> PerformanceCSV
  deriving (Show)

instance ToHtmlComponents PerformanceCSV where
  toMenuHeading _ = menuHeading "#performance-csv" "Performance Data (CSV)"

instance ToHtml PerformanceCSV where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      h1_ [] "System Performance Indicators"
      h2_ [] (toHtml dateHeader)
    br_ []
    div_ [class_ "content"] $ do
      -- Informational headers
      p_ [style_ "text-align: center"] "Download station-level performance indicators for the entire system as a CSV."
      br_ []
      div_ [class_ "pure-g", style_ "text-align: center"] mempty
      -- Selection form
      form_ [ class_ "pure-form pure-form-stacked"
            , style_ "text-align: center"
            , action_ ("/" <> (T.pack . show . linkURI) (performanceCsvPageDataLink params))
            ] $ fieldset_ $ do
        legend_ $ h3_ "Time Range Selection"
        div_ [class_ "pure-g"] $ do -- Grid layout for form
          div_ [class_ "pure-u-1 pure-u-md-1-3"] (startTimeInput earliest)
          div_ [class_ "pure-u-1 pure-u-md-1-3"] (endTimeInput latest)
          div_ [class_ "pure-u-1 pure-u-md-1-3"] (makeInputField (i_ "Download CSV") "submit" "download" "Download")

    where
      dateHeader :: T.Text
      dateHeader = T.pack $
                   (prettyTime (earliestTime times')) <> " ➜ " <>
                   (prettyTime (latestTime   times'))

      times' = enforceTimeRangeBounds (StatusDataParams (tz $ performanceCsvPageTimeRange params)
                                                        (currentUtcTime $ performanceCsvPageTimeRange params)
                                                        (performanceCsvPageTimeRange params)
                                      )
      earliest = earliestTime times'
      latest   = latestTime   times'
