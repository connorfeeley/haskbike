-- |

module Server.Components.ChargingHeader
     ( ChargingHeader (..)
     , sumTuples
     ) where

import           Control.Lens

import           Data.Int                                     ( Int32 )

import           Database.BikeShare.Tables.StationInformation ( StationInformation )

import           Lucid

import           Server.Page.Utils



data ChargingHeader where
  ChargingHeader :: { unChargingEvents :: [(StationInformation, Int32, Int32, Int32)] } -> ChargingHeader

instance ToHtml ChargingHeader where
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
