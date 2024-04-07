-- |

module Haskbike.Server.HtmlWithAssets
     ( HtmlWithAssets (..)
     , ServerAssets (..)
     ) where

import           Haskbike.Server.ExternalAssets

import           Lucid

-- | External asset location and manifest.
data ServerAssets where
  ServerAssets :: ExternalAssetLocation -> ServerAssets


data HtmlWithAssets a where
  HtmlWithAssets ::
    { siteAssets     :: ServerAssets
    , htmlWithAssets :: a
    } -> HtmlWithAssets a
