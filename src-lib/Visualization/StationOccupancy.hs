-- | This module contains functions for visualizing the occupancy of a station using Vega-Lite.

module Visualization.StationOccupancy
     ( availBikesOverTimeVL
     , selectionProps
     ) where

import           Data.Aeson.Encode.Pretty      ( encodePretty )
import qualified Data.ByteString.Lazy.Char8    as Char8
import qualified Data.Text                     as T
import           Data.Time

import           Database.BikeShare.Operations

import           Graphics.Vega.VegaLite

import           Prelude                       hiding ( filter, lookup, repeat )

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
        . foldAs [ "available_iconic", "available_efit", "available_efit_g5", "bikes_disabled", "docks_available", "docks_disabled" ] "Vehicle Type" "Count"
    -- Setup encoding common to both 'area' and 'point' marks
    areaEncoding =
      encoding
        . position X [ PTitle "Last Reported", PName "last_reported", PmType Temporal, PTimeUnit (TU YearMonthDateHoursMinutesSeconds), PAxis [AxLabelAngle (-50)]]
        . position Y [ PTitle "Count", PName "Count", PmType Quantitative, PStack StZero ]
        . color [ MName "Vehicle Type"
                , MmType Nominal
                -- FIXME: these colours don't seem to be applied in order - I think VegaLite is sorting the data by the colour scale
                , MScale [ SRange (RStrings [ "#FA8072" -- Available dock: grey
                                            , "#928F8F" -- Disabled bike: salmon
                                            , "#009ACD" -- E-Fit: light blue
                                            , "#00688B" -- E-Fit G5: sky blue
                                            , "#FFC300" -- Iconic: yellow
                                            , "black"   -- Disabled dock: black
                                            ]) ]
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
