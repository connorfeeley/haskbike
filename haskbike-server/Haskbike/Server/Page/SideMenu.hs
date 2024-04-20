{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Haskbike.Server.Page.SideMenu
     ( PureSideMenu (..)
     , renderMain
     , renderMenu
     , sideMenu
     , versionLink
     ) where

import           Control.Monad.Catch
import           Control.Monad.Reader

import qualified Data.Text                              as T

import           Haskbike.AppEnv
import           Haskbike.Server.Classes
import           Haskbike.Server.ExternalAssets
import           Haskbike.Server.LatestQueries
import           Haskbike.Server.LatestQueriesComponent
import           Haskbike.Server.Page.Utils
import           Haskbike.Server.PureCSS
import           Haskbike.Server.StaticAPI
import           Haskbike.ServerEnv
import           Haskbike.Version

import           Lucid
import           Lucid.Base                             ( makeAttribute )

import           Servant                                ( Link, fieldLink, toUrlPiece )


data PureSideMenu a where
  PureSideMenu :: (ToHtml a, ToHtmlComponents a) =>
    { pageContent       :: a
    , assetsLocation    :: ExternalAssetLocation
    , staticLink        :: Link
    , cabalVersionText  :: String
    , gitVersionText    :: String
    } -> PureSideMenu a

instance (ToHtml a, ToHtml LatestQueries, ToHtmlComponents a) => ToHtml (PureSideMenu a) where
  toHtmlRaw = toHtml
  toHtml component = do
    doctype_ -- Disable HTML quirks mode.
    headElement component
    layoutElement component
    where
      asst = assetsLocation component
      headElement c = head_ $ do
        makeHeadElements (assetsLocation component) ("/" <> toUrlPiece (staticLink c))
        toHead asst (pageContent c)

        stylesheet_ ("/" <> toUrlPiece (staticLink c) <> "/css/pure/side-menu.css") [defer_ mempty]
        script_ [src_ ("/" <> toUrlPiece (staticLink c) <> "/js/pure/ui.js"), integrity_ "sha384-kSeBLTki8KqaxayzOX+J1V3vfRIjnWmdVFtdkzwN4mEjkDK8CMit4TbzxVHdOqPu", defer_ mempty] ""
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
    mainContent = toHtml . pageContent

-- | Render the menu sidebar.
renderMenu :: (Monad m, ToHtml a, ToHtmlComponents a, ToHtml LatestQueries) => PureSideMenu a -> HtmlT m ()
renderMenu params =
  div_ [id_ "menu"] $ do
    div_ [class_ "pure-menu"] $ do
      toMenuHeading (pageContent params)
      ul_ [class_ "pure-menu-list"] $ do
        navLink "/" "Home"
        navLink "/visualization/station-list" "Station List"
        navLink "/visualization/station-occupancy" "Station Occupancy"
        navLink "/visualization/system-status" "System Status"
        navLink "/visualization/system-information" "System Information"
        navLink "/visualization/system-status/performance/csv" "Performance Data (CSV)"

    div_ [id_ "menu-footer"] $ do
      div_ [class_ "menu-vertical-spacer"] mempty
      toHtml LatestQueriesComponent
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
versionLink ver = linkElement shortVersion
  where
    linkElement = a_ [href_ (urlForVersion ver)]
    shortVersion = (toHtml . T.pack . take 7) ver
    baseUrl = "https://github.com/connorfeeley/haskbike/tree/"
    urlForVersion object = baseUrl <> T.pack object

-- | 'SideMenu' smart constructor.
sideMenu :: (HasEnv env m, MonadIO m, ToHtml a, ToHtmlComponents a, MonadCatch m, HasServerEnv env m)
         => a -> m (PureSideMenu a)
sideMenu content = do
  assetsLocation <- getServerAssetsLocation
  pure $
    PureSideMenu
    { pageContent      = content
    , assetsLocation   = assetsLocation
    , staticLink       = fieldLink staticApi
    , cabalVersionText = getCabalVersion
    , gitVersionText   = getGitHash
    }
