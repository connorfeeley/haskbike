{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Server.Page.SideMenu where

import           Lucid

import           Servant           ( Link, toUrlPiece )

import           Server.Page.Utils
import           Server.PureCSS

data PureSideMenu a where
  PureSideMenu :: (ToHtml a) => { visPageParams :: a
                                , staticLink :: Link
                                } -> PureSideMenu a

instance (ToHtml a) => ToHtml (PureSideMenu a) where
  toHtmlRaw = toHtml
  toHtml params = do
    head_ $ do
      makeHeadElements ("/" <> toUrlPiece (staticLink params))
      stylesheet_ ("/" <> toUrlPiece (staticLink params) <> "/css/pure/side-menu.css")
      script_ [src_ ("/" <> toUrlPiece (staticLink params) <> "/js/pure/ui.js"), async_ mempty] ""
    div_ [id_ "layout"] $ do
      a_ [href_ "#menu", id_ "menuLink", class_ "menu-link"] $
        span_ mempty
      div_ [id_ "menu"] $ do
        div_ [class_ "pure-menu"] $ do
          a_ [class_ "pure-menu-heading", href_ "#company"] (toHtml "Availability")
          ul_ [class_ "pure-menu-list"] $ do
            navLink "#home" "Home"
            navLink "#about" "About"
            navLinkDivided
            navLink "#contact" "Contact"
      div_ [id_ "main"] $ do
        toHtml (visPageParams params)
        -- div_ [class_ "header"] $ do
        --   h1_ [] (toHtml "Title")
        --   h2_ [] (toHtml "Subtitle")
        -- div_ [class_ "content"] $ do
          -- contentSubhead "Content subhead"
          -- contentParagraph "Content paragraph"