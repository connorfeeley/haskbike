-- |

module Server.Components.DockingHeader where

import           Database.BikeShare.EventCounts

import           Lucid

import           Server.Page.Utils


data DockingHeader where
  DockingHeader :: { dockingEvents   :: [DockingEventsCount]
                   } -> DockingHeader
instance ToHtml DockingHeader where
  toHtmlRaw = toHtml
  toHtml params = do
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
        ) (dockingEvents params)
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
      ) (dockingEvents params)
