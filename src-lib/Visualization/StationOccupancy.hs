-- | This module contains functions for visualizing the occupancy of a station using Vega-Lite.

module Visualization.StationOccupancy
     ( availBikesOverTimeVL
     , selectionProps
     ) where

import           Data.Aeson.Encode.Pretty   ( encodePretty )
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text                  as T
import           Data.Time

import           Graphics.Vega.VegaLite

import           Prelude                    hiding ( filter, lookup, repeat )

import           TextShow


-- Implement Vega-Lite specification using hvega
availBikesOverTimeVL :: Int -> LocalTime -> LocalTime -> VegaLite
availBikesOverTimeVL stationId startTime endTime =
  let
    selLabel = "picked"
    sel = selection
          . select selLabel Single []

  in
    toVegaLite (sel [] : selectionProps selLabel "Available Vehicle Types Over Time" stationId startTime endTime)


selectionProps :: SelectionLabel -> T.Text -> Int -> LocalTime -> LocalTime -> [PropertySpec]
selectionProps selName label stationId startTime endTime =
  let
    -- Implement the `fold` transform
    dataTransforms =
      transform
        . foldAs [ "Available Docks", "Available E-Fit", "Available E-Fit G5", "Available Mechanical", "Disabled Bikes", "Disabled Docks" ] "Type" "Count"
    -- Setup encoding common to both 'area' and 'point' marks
    areaEncoding =
      encoding
        . position X [ PTitle "Time",  PName "Last Reported", PmType Temporal,     PTimeUnit (TU YearMonthDateHoursMinutesSeconds), PAxis [AxLabelAngle (-50)]]
        . position Y [ PTitle "Count", PName "Count",         PmType Quantitative, PStack StZero ]
        . color [ MName "Type"
                , MmType Nominal -- Data are also categories, but ones which have some natural order.
                , MScale [ SDomain (DStrings [ "Available Docks", "Available E-Fit", "Available E-Fit G5", "Available Mechanical", "Disabled Bikes", "Disabled Docks" ])
                         , SRange (RStrings [ white     -- Available dock: white
                                            , lightBlue -- E-Fit: light blue
                                            , skyBlue   -- E-Fit G5: sky blue
                                            , green     -- Iconic: Cal Poly Pomona green
                                            , salmon    -- Disabled bike: salmon
                                            , black     -- Disabled dock: black
                                            ]) ]
                -- , MLegend [ LLabelExpr "'<' + datum.label + '>'" ]
                -- , MSelectionCondition (SelectionName selName) [ MName "Vehicle Type", MmType Nominal ] [ MString "grey" ]
                ]
        -- . opacity [ MSelectionCondition (SelectionName selName)
        --             [ MNumber 1.0 ]
        --             [ MNumber 0.3 ]
        --           ]
        . toolTip
        where
          white     = "#FFFFFF"
          lightBlue = "#009ACD"
          skyBlue   = "#00688B"
          green     = "#1E4D2B"
          salmon    = "#FA8072"
          black     = "#000000"

    -- Define tooltip for 'area' mark
    toolTip =
      tooltips [ [TTitle "Time", TName "Last Reported", TmType Temporal, TTimeUnit (TU YearMonthDateHoursMinutesSeconds)]
               , [TName "Type", TmType Nominal]
               , [TName "Count", TmType Quantitative]
               ]

    config =
      configure
        . configuration (Axis [ DomainWidth 1 ])
        . configuration (ViewStyle [ ViewStroke "transparent" ])
        . configuration (SelectionStyle [ ( Single, [ On "dblclick" ] ) ])
        -- . configuration (BackgroundStyle "rgba(0, 0, 0, 0.1)")


    -- Setup the `area` mark type with its encoding and interactive features
    areaLayer = asSpec [ mark Area [ ], areaEncoding [ ]]
    -- Setup the `point` mark type with its encoding only
    pointLayer = asSpec [mark Point [], areaEncoding [] ]

  in
    [ title label [ TOrient SBottom ]
    , liveDataSource stationId startTime endTime
    , dataTransforms []
    , layer [ areaLayer
            , pointLayer
            ]
    , widthOfContainer
    , height 800
    , autosize [ AFit, APadding, AResize ]
    , config []
    ]


-- Prepare the Vega-Lite data source
liveDataSource :: Int -> LocalTime -> LocalTime -> Data
liveDataSource stationId startTime endTime =
  -- data array is under "station_status" key
  dataFromUrl (T.intercalate "/" segments <> "?start-time=" <> T.pack (show startTime) <> "&end-time=" <> T.pack (show endTime)) [ ]
  where segments :: [T.Text] = [ "/data", "station-status", showt stationId ]


printVegaLiteSchema :: VegaLite -> IO ()
printVegaLiteSchema schema = Char8.putStrLn (encodePretty (fromVL schema))
