{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Server.Page.SideMenu where

import           Lucid
import           Lucid.Base        ( makeAttribute )

import           Servant           ( Link, toUrlPiece )

import           Server.Classes
import           Server.Page.Utils
import           Server.PureCSS

data PureSideMenu a where
  PureSideMenu :: (ToHtml a, ToHtmlComponents a) =>
    { visPageParams :: a
    , staticLink :: Link
    } -> PureSideMenu a

instance (ToHtml a, ToHtmlComponents a) => ToHtml (PureSideMenu a) where
  toHtmlRaw = toHtml
  toHtml params = do
    head_ $ do
      makeHeadElements ("/" <> toUrlPiece (staticLink params))
      stylesheet_ ("/" <> toUrlPiece (staticLink params) <> "/css/pure/side-menu.css")
      script_ [src_ ("/" <> toUrlPiece (staticLink params) <> "/js/pure/ui.js"), async_ mempty] ""
    div_ [id_ "layout"] $ do
      a_ [href_ "#menu", id_ "menuLink", class_ "menu-link", makeAttribute "aria-label" "Toggle sidebar"] $
        span_ mempty
      div_ [id_ "menu"] $ do
        div_ [class_ "pure-menu"] $ do
          toMenuHeading (visPageParams params)
          ul_ [class_ "pure-menu-list"] $ do
            navLink "/" "Home"
            -- navLink "/visualization" "Visualization"
            navLink "/visualization/station-list" "Station List"
            navLink "/visualization/system-status" "System Status"
            navLink "/data/system-status/performance/csv" "Performance Data (CSV)"
            -- navLink "/#about" "About"
            -- navLink "/#contact" "Contact"
      div_ [id_ "main"] $ do
        -- Render parameterized type
        toHtml (visPageParams params)
