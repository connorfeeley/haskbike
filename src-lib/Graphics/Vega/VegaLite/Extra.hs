-- |

-- | This module defines the data types used to render the station status visualization page.

module Graphics.Vega.VegaLite.Extra
     ( VegaSourceURLs (..)
     , toHtmlWithUrls
     , vegaSourceUrlsCdn
     , vegaSourceUrlsLocal
     ) where

import           Data.Aeson
import qualified Data.Aeson.Text        as A
import qualified Data.Text.Lazy         as TL

import qualified Graphics.Vega.VegaLite as VL

-- | Data type holding the 3 required Javascript source files for Vega-Embed.
data VegaSourceURLs where
  VegaSourceURLs :: { vegaUrl      :: TL.Text
                    , vegaLiteUrl  :: TL.Text
                    , vegaEmbedUrl :: TL.Text
                    } -> VegaSourceURLs
  deriving (Show, Eq)

-- | The CDN version of the Vega source files.
vegaSourceUrlsCdn :: VegaSourceURLs
vegaSourceUrlsCdn = VegaSourceURLs { vegaUrl      = "https://cdn.jsdelivr.net/npm/vega@5"
                                   , vegaLiteUrl  = "https://cdn.jsdelivr.net/npm/vega-lite@4"
                                   , vegaEmbedUrl = "https://cdn.jsdelivr.net/npm/vega-embed"
                                   }
-- | The local version of the Vega source files.
vegaSourceUrlsLocal :: VegaSourceURLs
vegaSourceUrlsLocal = VegaSourceURLs { vegaUrl      = "/static/js/vega/vega@5.25.0"
                                     , vegaLiteUrl  = "/static/js/vega/vega-lite@4.17.0"
                                     , vegaEmbedUrl = "/static/js/vega/vega-embed@6.22.2"
                                     }

{-|
Converts VegaLite to html Text. Uses Vega-Embed and is for when
some control is needed over the output: 'toHtml' is a simpler
form which just uses the default Vega-Embed options.

The render you use to view the output file must support Javascript,
since it is needed to create the visualization from the Vega-Lite
specification. The Vega and Vega-Lite Javascript versions are pegged
to 5 and 4, but no limit is applied to the Vega-Embed library.

@since 0.4.0.0
-}
toHtmlWithUrls ::
  -- ^ The base URLs for to add as <script> tags.
  VegaSourceURLs
  -> Maybe Value
  -- ^ The options to pass to the Vega-Embed @embed@ routine. See
  --   <https://github.com/vega/vega-embed#options> for the
  --   supported options.
  -> VL.VegaLite
  -- ^ The Vega-Lite specification to display.
  -> TL.Text
toHtmlWithUrls urls mopts vl =
  let spec = A.encodeToLazyText (VL.fromVL vl)
      opts = maybe "" (\o -> "," <> A.encodeToLazyText o) mopts

  in TL.unlines
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<head>"
      -- versions are fixed at vega 5, vega-lite 4
    , "  <script src=" <> vegaUrl urls <> "></script>"
    , "  <script src=" <> vegaLiteUrl urls <> "></script>"
    , "  <script src=" <> vegaEmbedUrl urls <> "></script>"
    , "</head>"
    , "<body>"
    , "<div id=\"vis\"></div>"
    , "<script type=\"text/javascript\">"
    , "  var spec = " <> spec <> ";"
    , "  vegaEmbed(\'#vis\', spec" <> opts <> ").then(function(result) {"
    , "  // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view"
    , "  }).catch(console.error);"
    , "</script>"
    , "</body>"
    , "</html>"
    ]
