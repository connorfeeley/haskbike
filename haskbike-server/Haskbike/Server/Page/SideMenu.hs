{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Haskbike.Server.Page.SideMenu
     ( PureSideMenu (..)
     , renderMain
     , renderMenu
     , versionLink
     ) where

import qualified Data.Text                  as T

import           Haskbike.LatestQueries
import           Haskbike.Server.Classes
import           Haskbike.Server.Page.Utils
import           Haskbike.Server.PureCSS

import           Lucid
import           Lucid.Base                 ( makeAttribute )

import           Servant                    ( Link, toUrlPiece )

data PureSideMenu a where
  PureSideMenu :: (ToHtml a, ToHtmlComponents a) =>
    { visPageParams    :: a
    , staticLink       :: Link
    , cabalVersionText :: String
    , gitVersionText   :: String
    , latestQueries    :: LatestQueries
    } -> PureSideMenu a

instance (ToHtml a, ToHtml LatestQueries, ToHtmlComponents a) => ToHtml (PureSideMenu a) where
  toHtmlRaw = toHtml
  toHtml component = do
    doctype_ -- Disable HTML quirks mode.
    headElement component
    layoutElement component
    where
      headElement c = head_ $ do
        makeHeadElements ("/" <> toUrlPiece (staticLink c)) "//stats.bikes.cfeeley.org/count.js"
        toHead (visPageParams c)

        stylesheet_ ("/" <> toUrlPiece (staticLink c) <> "/css/pure/side-menu.css") [defer_ mempty]
        script_ [src_ ("/" <> toUrlPiece (staticLink c) <> "/js/pure/ui.js"), defer_ mempty] ""
      menuLink = a_ [href_ "#menu", id_ "menuLink", class_ "menu-link", makeAttribute "aria-label" "Toggle sidebar"] $ span_ mempty
      layoutElement c = div_ [id_ "layout"] $ do
        menuLink
        renderMenu c -- Render menu and menu footer.
        renderMain c -- Render main content.


-- | Render the main content.
renderMain :: (Monad m, ToHtml a, ToHtmlComponents a) => PureSideMenu a -> HtmlT m ()
renderMain = mainContainer . mainContent -- Render parameterized type
  where
    mainContainer = div_ [id_ "main", class_ "main-container"]
    mainContent = toHtml . visPageParams

-- | Render the menu sidebar.
renderMenu :: (Monad m, ToHtml a, ToHtmlComponents a, ToHtml LatestQueries) => PureSideMenu a -> HtmlT m ()
renderMenu params =
  div_ [id_ "menu"] $ do
    div_ [class_ "pure-menu"] $ do
      toMenuHeading (visPageParams params)
      ul_ [class_ "pure-menu-list"] $ do
        navLink "/" "Home"
        navLink "/visualization/station-list" "Station List"
        navLink "/visualization/station-occupancy" "Station Occupancy"
        navLink "/visualization/system-status" "System Status"
        navLink "/visualization/system-information" "System Information"
        navLink "/visualization/system-status/performance/csv" "Performance Data (CSV)"

    div_ [id_ "menu-footer"] $ do
      div_ [class_ "menu-vertical-spacer"] mempty
      toHtml (latestQueries params)
      renderVersion params

-- | Render the version link.
renderVersion :: (Monad m, ToHtml a, ToHtmlComponents a) => PureSideMenu a -> HtmlT m ()
renderVersion params =
  versionContainer versionContent
  where
    versionContainer = div_ [class_ "menu-footer-element menu-footer-version"]
    versionContent   = "Version: " <> cabalVersion params <> gitVersion params
    wrapBrackets content = "(" <> content <> ")"
    gitVersion   = toHtml . versionLink . gitVersionText
    cabalVersion = toHtml . cabalVersionText

-- | Render the version link.
versionLink :: Monad m => String -> HtmlT m ()
versionLink version = linkElement shortVersion
  where
    linkElement = a_ [href_ (urlForVersion version)]
    shortVersion = (toHtml . T.pack . take 7) version
    baseUrl = "https://github.com/connorfeeley/haskbike/tree/"
    urlForVersion object = baseUrl <> T.pack object
