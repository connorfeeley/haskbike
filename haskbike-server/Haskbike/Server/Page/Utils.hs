{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- |

module Haskbike.Server.Page.Utils
     ( hxSpinner_
     , hx_
     , makeFavicons
     , makeHeadElements
     , mkData_
     , showth
     , stylesheet_
     ) where

import qualified Data.Text  as T

import           Lucid
import           Lucid.Base ( makeAttribute )

import           Servant    ( Link, ToHttpApiData (toUrlPiece), linkURI )

import           TextShow


-- | Helper function to create a stylesheet link.
stylesheet_ :: Applicative m => T.Text -> [Attribute] -> HtmlT m ()
stylesheet_ url attrs = link_ ([rel_ "stylesheet", type_ "text/css", href_ url] ++ attrs)


-- | Make link elements for a list of pixel sizes, pointing to a given path.
makeFavicons :: (Monad m, Num a, Show a) => T.Text -> [a] -> HtmlT m [()]
makeFavicons staticPath = mapM (link_ . linkAttrs)
  where
    ssz :: (Num a, Show a) => a -> T.Text
    ssz sz = T.pack (show sz) <> "x" <> T.pack (show sz)
    hrefAttr  sz = href_ (staticPath <> "/images/favicon-" <> ssz sz <> ".png")
    linkAttrs sz = [rel_ "icon noopener noreferrer", type_ "image/png", sizes_ (ssz sz), target_ "_blank", hrefAttr sz]


-- | Create standard <head> elements.
makeHeadElements :: Monad m => T.Text -> T.Text -> HtmlT m ()
makeHeadElements staticPath statsPath = do
  -- Favicons
  makeFavicons staticPath ([16, 32, 48, 96, 180, 300, 512] :: [Int])

  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

  -- Pure.CSS
  link_ [rel_ "stylesheet", href_ (staticPath <> "/css/pure/pure-min@3.0.0.css"), integrity_ "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls", crossorigin_ "anonymous"]
  -- link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/pure-min.css", integrity_ "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls", crossorigin_ "anonymous"]
  stylesheet_ (staticPath <> "/css/pure/pure-grids-responsive-min@3.0.0.css") [defer_ mempty]

  -- HTMX
  script_ [src_ (staticPath <> "/js/htmx/htmx.min.js"), integrity_ "sha384-QFjmbokDn2DjBjq+fM+8LUIVrAgqcNW2s0PjAxHETgRn9l4fvX31ZxDxvwQnyMOX", crossorigin_ "anonymous", defer_ mempty] ("" :: T.Text)

  -- Project stylesheet
  stylesheet_ (staticPath <> "/css/haskbike.css") [defer_ mempty]
  stylesheet_ (staticPath <> "/css/tooltips.css") [defer_ mempty]

  -- TODO: get this from the server's environment.
  script_ [mkData_ "goatcounter" "https://stats.bikes.cfeeley.org/count", defer_ mempty, src_ statsPath] ("" :: T.Text)


-- | Make a "data-" attribute suffixed with the given 'Text'.
mkData_ :: T.Text -> T.Text -> Attribute
mkData_ suffix = makeAttribute ("data-" <> suffix)

showth :: (Monad m, TextShow a)
       => a -- ^ The (TextShow-able) value to be converted to HTML.
       -> HtmlT m ()
showth = toHtml . showt

-- | Helper function to create an HTMX attribute.
hx_ :: T.Text -> T.Text -> Attribute
hx_ attr = makeAttribute ("hx-" <> attr)


-- FIXME: manually prefixing with '/components' defeats the whole point of safe links.
hxSpinner_ :: Monad m => Link -> Link -> HtmlT m ()
hxSpinner_ staticPath link = div_ [ hx_ "trigger" "load"
                                  , hx_ "get" ("/components/" <> (T.pack . show . linkURI) link)
                                  ]
                             (img_ [class_ "htmx-indicator htmx-spinner", src_ ("/" <> toUrlPiece staticPath <> "/images/svg-loaders/circles.svg"), alt_ "Loading..."])
