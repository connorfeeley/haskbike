-- | This module contains functions for visualizing the occupancy of a station using Vega-Lite.

module Visualization.StationOccupancy where

import           Data.Aeson.Encode.Pretty   ( encodePretty )
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text                  as T

import           Graphics.Vega.VegaLite

import           Prelude                    hiding ( filter, lookup, repeat )

-- Prepare the Vega-Lite data source
dataSource :: Data
dataSource =
  dataFromUrl "https://gist.github.com/connorfeeley/53396497d4e84b3d45f557f0452eb31e/raw/af9bc760ebbc531ad80df8f90a82f2e6e988a4c8/station_status_7001_10-15-16_202310170129.csv" []

-- Implement Vega-Lite specification using hvega
toVegaLite' :: VegaLite
toVegaLite' =
  let
    selLabel = "picked"
    sel = selection
          . select selLabel Single []

  in
    toVegaLite (sel [] : selectionProps selLabel "Available Vehicle Types Over Time")

selectionProps :: SelectionLabel -> T.Text -> [PropertySpec]
selectionProps selName label =
  let
    -- Implement the `fold` transform
    dataTransforms =
      transform
        . foldAs [ "vehicle_types_available_iconic", "vehicle_types_available_efit", "vehicle_types_available_efit_g5", "num_bikes_disabled", "num_docks_available" ] "Vehicle Type" "Count"
    -- Setup encoding common to both 'area' and 'point' marks
    areaEncoding =
      encoding
        . position X [ PTitle "Last Reported", PName "last_reported", PmType Temporal]
        . position Y [ PTitle "Count", PName "Count", PmType Quantitative, PStack StZero ]
        . color [ MName "Vehicle Type"
                , MmType Nominal
                , MLegend [ LLabelExpr "'<' + datum.label + '>'" ]
                -- , MSelectionCondition (SelectionName selName) [ MName "Vehicle Type", MmType Nominal ] [ MString "grey" ]
                ]
        -- . opacity [ MSelectionCondition (SelectionName selName)
        --             [ MNumber 1.0 ]
        --             [ MNumber 0.3 ]
        --           ]
        . toolTip

    -- Define tooltip for 'area' mark
    toolTip =
      tooltips [ [TTitle "Last Reported", TName "last_reported", TmType Temporal, TTimeUnit (TU YearMonthDateHoursMinutesSeconds)]
               , [TName "Vehicle Type", TmType Nominal]
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
    , dataSource
    , dataTransforms []
    , layer [ areaLayer
            , pointLayer
            ]
    , width 1200
    , height 800
    , config []
    ]


main :: IO ()
main =
  toHtmlFile "example.html" toVegaLite'

printVegaLiteSchema :: VegaLite -> IO ()
printVegaLiteSchema schema = Char8.putStrLn (encodePretty (fromVL schema))
