{-# LANGUAGE PartialTypeSignatures #-}
-- | This module contains functions for visualizing the system information API using Vega-Lite.

module Haskbike.Visualization.SystemInformation
     ( systemInformationProps
     ) where

import qualified Data.Text                             as T

import           Graphics.Vega.VegaLite                hiding ( Number )

import           Haskbike.Graphics.Vega.VegaLite.Extra

import           Prelude                               hiding ( filter, lookup, repeat )


-- | For System Information
systemInformationProps :: T.Text -> T.Text -> T.Text -> [PropertySpec]
systemInformationProps label dataUrl timeKey =
  let
    -- Setup the `area` mark type with its encoding and interactive features
    areaLayer  = asSpec [ mark Area  [MInterpolate StepAfter]
                        , (encoding
                           . position X [ PTitle "Time",  PName timeKey,  PmType Temporal,      PTimeUnit (TU MonthDateHoursMinutes), PAxis [AxLabelAngle (-90)]]
                           . position Y [ PTitle "Count", PName "Count", PmType Quantitative, PStack StZero ]
                           . color [ MName "Type"
                                   , MmType Nominal -- Data are also categories, but ones which have some natural order.
                                   , MScale [ SDomain (DStrings [ "Station Count", "Mechanical Count", "E-Bike Count" ])
                                            , SRange  (RStrings (map T.pack ["orange", green, skyBlue]))
                                            ]
                                   , MDataCondition [(Expr "datum ['Type'] === 'Station Count'", [MNullValue])] [MNullValue]
                                   ]
                            . tooltips [ [TTitle "Time", TName timeKey, TmType Temporal, TTimeUnit (TU YearMonthDate)]
                                       , [TName  "Type", TmType Nominal]
                                       , [TName "Count", TmType Quantitative]
                                       ]
                          ) []
                        , (transform . foldAs [ "Mechanical Count", "E-Bike Count" ] "Type" "Count") []
                        ]
    lineLayer  = asSpec [ mark Line  [MInterpolate StepAfter, MStrokeWidth 5]
                        , ( encoding
                            . position X [ PTitle "Time",  PName timeKey,  PmType Temporal,     PTimeUnit (TU MonthDateHoursMinutes), PAxis [AxLabelAngle (-90)]]
                            . position Y [ PTitle "Station Count", PName "Station Count", PmType Quantitative, PStack StZero ]
                            . color [ MString "orange"
                                    ]
                            . tooltips [ [TTitle "Time",  TName timeKey, TmType Temporal, TTimeUnit (TU YearMonthDate)]
                                       , [TName "Station Count", TmType Quantitative]
                                       , [TName "Mechanical Count", TmType Quantitative]
                                       , [TName "E-Bike Count", TmType Quantitative]
                                       ]
                          ) []
                        ]
  in
    makeProps label dataUrl transform
        [ areaLayer
        , lineLayer
        ]
