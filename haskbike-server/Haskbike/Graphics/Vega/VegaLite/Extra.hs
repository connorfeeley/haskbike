-- |

-- | This module defines the data types used to render the station status visualization page.

module Haskbike.Graphics.Vega.VegaLite.Extra
     ( ShowVegaActions (..)
     , VegaSourceURLs (..)
     , makeProps
     , printVegaLiteSchema
     , toHtmlWithUrls
     , vegaEmbedCfg
     , vegaSourceUrlsCdn
     , vegaSourceUrlsLocal
       -- Colours
     , black
     , commonColourRange
     , green
     , lemon
     , lightBlue
     , salmon
     , skyBlue
     , transparent
     ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty   ( encodePretty )
import qualified Data.Aeson.Text            as A
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL

import           Graphics.Vega.VegaLite     hiding ( Number )
import qualified Graphics.Vega.VegaLite     as VL

import           Prelude                    hiding ( filter, lookup, repeat )


transparent, lemon, lightBlue, skyBlue, green, salmon, black :: String
transparent = "rgb(0,0,0,0)" -- transparent
lemon        = "#fffacd"
lightBlue    = "#009ACD"
skyBlue      = "#00688B"
green        = "#1E4D2B"
salmon       = "#FA8072"
black        = "#000000"

commonColourRange :: [T.Text]
commonColourRange = map T.pack [ lemon       -- Available dock: lemon chiffron
                               , green       -- Iconic: Cal Poly Pomona green
                               , lightBlue   -- E-Fit: light blue
                               , skyBlue     -- E-Fit G5: sky blue
                               , salmon      -- Disabled bike: salmon
                               , black       -- Disabled dock: black
                               ]

globalConfig :: [ConfigureSpec] -> PropertySpec
globalConfig =
  configure
    . configuration (Axis [ DomainWidth 1 ])
    . configuration (ViewStyle [ ViewStroke "transparent" ])
    . configuration (SelectionStyle [(Single, [On "dblclick"])])
    . configuration (BackgroundStyle "rgba(0, 0, 0, 0)")
    . configuration (LegendStyle [LeOrient LOBottom])


makeProps :: T.Text -> T.Text -> ([a] -> PropertySpec) -> [VLSpec] -> [PropertySpec]
makeProps label dataUrl dataTransforms layers =
  [ title label [ TOrient SBottom ]
  , dataFromUrl dataUrl [ ]
  , dataTransforms []
  , layer layers
  , widthOfContainer
  , heightOfContainer
  , autosize [ AFit, APadding, AResize ]
  , globalConfig []
  , resolve . resolution (RScale [(ChY, Independent)]) $ []
  ]


printVegaLiteSchema :: VegaLite -> IO ()
printVegaLiteSchema schema = Char8.putStrLn (encodePretty (fromVL schema))

data ShowVegaActions = ShowActions | HideActions

-- | JSON configuration passed to Vega-Embed.
vegaEmbedCfg :: ShowVegaActions -> Maybe Value
vegaEmbedCfg showActions =
  Just (toJSON (object [ ("logLevel", "4")
                        , ("$schema", "/static/js/vega/schema/vega-lite/v4.json")
                        , ("actions", actionToBool showActions)
                        , ("scaleFactor", Number 10)
                        ]))
  where
    actionToBool action = case action of ShowActions -> Bool True
                                         HideActions -> Bool False
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
toHtmlWithUrls _urls mopts vl =
  let spec = A.encodeToLazyText (VL.fromVL vl)
      opts = maybe "" (\o -> "," <> A.encodeToLazyText o) mopts

  -- FIXME: use Lucid for this.
  in TL.unlines
    [ "<div id=\"vis\"></div>"
    , "<script type=\"text/javascript\">"
    , "  window.addEventListener('DOMContentLoaded', function() {"
    , "  (function($) {"
    , "    var spec = " <> spec <> ";"
    , "    vegaEmbed(\'#vis\', spec" <> opts <> ").then(function(result) {"
    , "    // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view"
    , "    }).catch(console.error);"
    , "  })();"
    , "});"
    , "</script>"
    ]
