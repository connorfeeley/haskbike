{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Server.Page.SideMenu where

import qualified Data.Text         as T

import           Lucid
import           Lucid.Base        ( makeAttribute )

import           Servant           ( Link, toUrlPiece )

import           Server.Classes
import           Server.Page.Utils
import           Server.PureCSS

data PureSideMenu a where
  PureSideMenu :: (ToHtml a, ToHtmlComponents a) =>
    { visPageParams :: a
    , staticLink    :: Link
    , versionText   :: String
    } -> PureSideMenu a

instance (ToHtml a, ToHtmlComponents a) => ToHtml (PureSideMenu a) where
  toHtmlRaw = toHtml
  toHtml params = do
    head_ $ do
      makeHeadElements ("/" <> toUrlPiece (staticLink params)) "//stats.bikes.cfeeley.org/count.js"
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
            navLink "/visualization/station-list" "Station List"
            navLink "/visualization/system-status" "System Status"
            navLink "/visualization/system-status/performance/csv" "Performance Data (CSV)"
        div_ [id_ "menu-footer"] ("Version: " <> versionLink (versionText params))
      div_ [id_ "main"] $ do
        -- Render parameterized type
        toHtml (visPageParams params)

versionLink :: Monad m => String -> HtmlT m ()
versionLink version = a_ [href_ (urlForVersion version)] (toHtml shortVersion)
  where
    shortVersion = T.pack (take 7 version)
    baseUrl = "https://github.com/connorfeeley/haskbike/tree/"
    urlForVersion object = baseUrl <> T.pack object
