-- |

module Haskbike.Server.HtmlWithAssets
     ( HtmlWithAssets (..)
     ) where

import           Haskbike.Server.ExternalAssets

import           Lucid

data HtmlWithAssets a where
  HtmlWithAssets :: (ToHtml a) =>
    { siteAssets     :: SiteAssets
    , htmlWithAssets :: a
    } -> HtmlWithAssets a

-- instance (ToHtml a) => ToHtml (HtmlWithAssets a) where
--   toHtmlRaw = toHtml
--   toHtml = toHtml
