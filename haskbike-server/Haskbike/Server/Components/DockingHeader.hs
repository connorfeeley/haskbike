-- |

module Haskbike.Server.Components.DockingHeader where

import           Haskbike.Database.EventCounts
import           Haskbike.Server.Page.Utils

import           Lucid


data DockingHeader where
  DockingHeader :: { dockingEvents   :: [DockingEventsCount]
                   } -> DockingHeader
instance ToHtml DockingHeader where
  toHtmlRaw = toHtml
  toHtml params = do
    (\events' -> div_ $ do
        div_ [class_ "tooltip"] $ do
          label_ [ for_ "undockings"
                 , class_ "tooltip"
                 ] (h3_ "Trips Started")
          div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Iconic: "   <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (iconicEvents events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Boost: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (boostEvents  events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (efitEvents   events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (efitG5Events events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "CHLOE: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (chloeEvents  events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Cosmo: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (cosmoEvents  events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Astro: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (astroEvents  events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Metro: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Undocking (metroEvents  events'))))
        div_ [id_ "undockings"] (showth (sumEvents Undocking (allBikeEvents events')))
      ) (dockingEvents params)
    (\events' -> div_ $ do
        div_ [class_ "tooltip"] $ do
          label_ [ for_ "dockings"
                 , class_ "tooltip"
                 ] (h3_ "Trips Ended")
          div_ [class_ "tooltip-bottom"] $ do -- Tooltip content
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Iconic: "   <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (iconicEvents events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Boost: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (boostEvents  events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (efitEvents   events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "E-Fit G5: " <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (efitG5Events events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "CHLOE: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (chloeEvents  events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Cosmo: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (cosmoEvents  events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Astro: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (astroEvents  events'))))
            p_ [class_ "pure-g"] (b_ [class_ "pure-u-1-2"] "Metro: "    <> span_ [class_ "pure-u-1-2"] (showth (sumEvents Docking (metroEvents  events'))))
          div_ [id_ "dockings"] (showth (sumEvents Docking (allBikeEvents events')))
        ) (dockingEvents params)
