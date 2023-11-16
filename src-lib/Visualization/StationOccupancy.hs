-- | This module contains functions for visualizing the occupancy of a station using Vega-Lite.

module Visualization.StationOccupancy
     ( ShowVegaActions (..)
     , availBikesOverTimeVL
     , printVegaLiteSchema
     , selectionProps
     , vegaEmbedCfg
     ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text                  as T

import           Graphics.Vega.VegaLite     hiding ( Number )

import           Prelude                    hiding ( filter, lookup, repeat )


-- Implement Vega-Lite specification using hvega
availBikesOverTimeVL :: T.Text -> VegaLite
availBikesOverTimeVL dataUrl=
  let
    selLabel = "picked"
    sel = selection
          . select selLabel Single []

  in
    toVegaLite (sel [] : selectionProps selLabel "Available Vehicle Types Over Time" dataUrl)


selectionProps :: SelectionLabel -> T.Text -> T.Text -> [PropertySpec]
selectionProps _selName label dataUrl =
  let
    -- Implement the `fold` transform
    dataTransforms =
      transform
        . foldAs [ "Available Docks", "Available Mechanical", "Available E-Fit", "Available E-Fit G5", "Disabled Bikes", "Disabled Docks" ] "Type" "Count"
    -- Setup encoding common to both 'area' and 'point' marks
    areaEncoding =
      encoding
        . position X [ PTitle "Time",  PName "Last Reported", PmType Temporal,     PTimeUnit (TU YearMonthDateHoursMinutesSeconds), PAxis [AxLabelAngle (-90)]]
        . position Y [ PTitle "Count", PName "Count",         PmType Quantitative, PStack StZero ]
        . color [ MName "Type"
                , MmType Nominal -- Data are also categories, but ones which have some natural order.
                , MScale [ SDomain (DStrings [ "Available Docks", "Available Mechanical", "Available E-Fit", "Available E-Fit G5", "Disabled Bikes", "Disabled Docks" ])
                         , SRange (RStrings [ lemon       -- Available dock: lemon chiffron
                                            , green       -- Iconic: Cal Poly Pomona green
                                            , lightBlue   -- E-Fit: light blue
                                            , skyBlue     -- E-Fit G5: sky blue
                                            , salmon      -- Disabled bike: salmon
                                            , black       -- Disabled dock: black
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
          _transparent = "rgb(0,0,0,0)" -- transparent
          lemon        = "#fffacd"
          lightBlue    = "#009ACD"
          skyBlue      = "#00688B"
          green        = "#1E4D2B"
          salmon       = "#FA8072"
          black        = "#000000"

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
        . configuration (SelectionStyle [(Single, [On "dblclick"])])
        . configuration (BackgroundStyle "rgba(0, 0, 0, 0.5)")
        . configuration (LegendStyle [LeOrient LOBottom])


    -- Setup the `area` mark type with its encoding and interactive features
    areaLayer  = asSpec [mark Area  [MInterpolate StepAfter], areaEncoding []]
    -- Setup the `point` mark type with its encoding only
    pointLayer = asSpec [mark Point [MInterpolate StepAfter], areaEncoding []]
  in
    [ title label [ TOrient SBottom ]
    , dataFromUrl dataUrl [ ]
    , dataTransforms []
    , layer [ areaLayer
            , pointLayer
            ]
    , widthOfContainer
    , heightOfContainer
    , autosize [ AFit, APadding, AResize ]
    , config []
    ]


printVegaLiteSchema :: VegaLite -> IO ()
printVegaLiteSchema schema = Char8.putStrLn (encodePretty (fromVL schema))

data ShowVegaActions = ShowActions | HideActions

-- | JSON configuration passed to Vega-Embed.
vegaEmbedCfg :: ShowVegaActions -> Maybe Value
vegaEmbedCfg showActions =
  Just (toJSON (object [ ("logLevel", "4")
                        , ("$schema", "/static/js/vega/schema/vega-lite/v4.json")
                        , ("actions", actionToBool showActions)
                        , ("scaleFactor", Number 10)
                        ]))
  where
    actionToBool action = case action of ShowActions -> Bool True
                                         HideActions -> Bool False
