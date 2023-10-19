-- |

module Server.Page.Utils
     ( makeFavicons
     , makeHeadElements
     , stylesheet_
     ) where

import           Data.Aeson
import           Data.Text
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.StationInformation

import           Fmt

import qualified Graphics.Vega.VegaLite                 as VL
import           Graphics.Vega.VegaLite.Extra

import           Lucid
import           Lucid.Base                             ( makeAttribute )
import           Lucid.Servant

import           Servant

import           Server.Data.StationStatusVisualization

import           TextShow

import           Visualization.StationOccupancy


-- | Helper function to create a stylesheet link.
stylesheet_ :: Applicative m => Text -> HtmlT m ()
stylesheet_ url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]


-- | Make link elements for a list of pixel sizes, pointing to a given path.
makeFavicons :: (Monad m, Num a, Show a) => Text -> [a] -> HtmlT m [()]
makeFavicons staticPath = mapM (link_ . linkAttrs)
  where
    ssz :: (Num a, Show a) => a -> Text
    ssz sz = pack (show sz) <> "x" <> pack (show sz)
    hrefAttr  sz = href_ (staticPath <> "/images/favicon-" <> ssz sz <> ".png")
    linkAttrs sz = [rel_ "icon noopener noreferrer", type_ "image/png", sizes_ (ssz sz), target_ "_blank", hrefAttr sz]

-- | Create standard <head> elements.
makeHeadElements :: Monad m => Text -> HtmlT m ()
makeHeadElements staticPath = do
  -- Favicons
  makeFavicons staticPath ([16, 32, 48, 96, 180, 300, 512] :: [Int])

  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

  -- Pure.CSS
  link_ [rel_ "stylesheet", href_ (staticPath <> "/css/pure/pure-min@3.0.0.css"), integrity_ "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls", crossorigin_ "anonymous"]
  -- link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/pure-min.css", integrity_ "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls", crossorigin_ "anonymous"]
  stylesheet_ (staticPath <> "/css/pure/pure-grids-responsive-min@3.0.0.css")

  -- Project stylesheet
  stylesheet_ (staticPath <> "/css/haskbike.css")
