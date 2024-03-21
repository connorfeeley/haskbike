{-# LANGUAGE PartialTypeSignatures #-}
-- | This module contains functions for visualizing the occupancy of a station using Vega-Lite.

module Haskbike.Visualization.StationOccupancy
     ( availBikesOverTimeVL
     , selectionProps
     ) where

import qualified Data.Text                             as T

import           Graphics.Vega.VegaLite                hiding ( Number )

import           Haskbike.Graphics.Vega.VegaLite.Extra

import           Prelude                               hiding ( filter, lookup, repeat )


-- Implement Vega-Lite specification using hvega
availBikesOverTimeVL :: ([String] -> [T.Text]) -> T.Text -> VegaLite
availBikesOverTimeVL filterFn dataUrl =
  let
    selLabel = "picked"
    sel = selection
          . select selLabel Single []

  in
    toVegaLite (sel [] : selectionProps selLabel filterFn "Available Vehicle Types Over Time" dataUrl)

-- | For Station Status and System Status
selectionProps :: SelectionLabel -> ([String] -> [T.Text]) -> T.Text -> T.Text -> [PropertySpec]
selectionProps _selName filterFn label dataUrl =
  let
    -- Implement the `fold` transform
    dataTransforms =
      transform
        . foldAs (filterFn [ "Available Docks", "Available Mechanical", "Available E-Fit", "Available E-Fit G5", "Disabled Bikes", "Disabled Docks" ]) "Type" "Count"
    -- Setup encoding common to both 'area' and 'point' marks
    areaEncoding =
      encoding
        . position X [ PTitle "Time",  PName "Last Reported", PmType Temporal,     PTimeUnit (TU YearMonthDateHoursMinutesSeconds), PAxis [AxLabelAngle (-90)]]
        . position Y [ PTitle "Count", PName "Count",         PmType Quantitative, PStack StZero ]
        . color [ MName "Type"
                , MmType Nominal -- Data are also categories, but ones which have some natural order.
                , MScale [ SDomain (DStrings (filterFn [ "Available Docks", "Available Mechanical", "Available E-Fit", "Available E-Fit G5", "Disabled Bikes", "Disabled Docks" ]))
                         , SRange (RStrings (filterFn [ lemon       -- Available dock: lemon chiffron
                                                      , green       -- Iconic: Cal Poly Pomona green
                                                      , lightBlue   -- E-Fit: light blue
                                                      , skyBlue     -- E-Fit G5: sky blue
                                                      , salmon      -- Disabled bike: salmon
                                                      , black       -- Disabled dock: black
                                                      ])) ]
                -- , MLegend [ LLabelExpr "'<' + datum.label + '>'" ]
                -- , MSelectionCondition (SelectionName selName) [ MName "Vehicle Type", MmType Nominal ] [ MString "grey" ]
                ]
        -- . opacity [ MSelectionCondition (SelectionName selName)
        --             [ MNumber 1.0 ]
        --             [ MNumber 0.3 ]
        --           ]
        . toolTip

    -- Define tooltip for 'area' mark
    toolTip =
      tooltips [ [TTitle "Time", TName "Last Reported", TmType Temporal, TTimeUnit (TU YearMonthDateHoursMinutesSeconds)]
               , [TName "Type",  TmType Nominal]
               , [TName "Count", TmType Quantitative]
               ]

    -- Setup the `area` mark type with its encoding and interactive features
    areaLayer  = asSpec [mark Area  [MInterpolate StepAfter], areaEncoding []]
    -- Setup the `point` mark type with its encoding only
    _pointLayer = asSpec [mark Point [MInterpolate StepAfter], areaEncoding []]
  in
    makeProps label dataUrl dataTransforms [ areaLayer
                                           -- , pointLayer
                                           ]
