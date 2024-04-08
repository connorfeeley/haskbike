{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Types for external assets.

module Haskbike.Server.ExternalAssets
     ( ExternalAssetDetails (..)
     , ExternalAssetLocation (..)
     , GridJS (..)
     , HasAssetDetails (..)
     , MermaidCss (..)
     , PureCss (..)
     ) where

import qualified Data.Text as T

import           Servant


-- | Details about where to find an asset.
data ExternalAssetDetails where
  ExternalAssetDetails :: { assetLocation  :: ExternalAssetLocation
                          , assetUrl       :: T.Text
                          , assetIntegrity :: T.Text
                          } -> ExternalAssetDetails

-- | Typed representation of where external asset should be fetched from.
data ExternalAssetLocation where
  ExternalAssetVendored :: Link -> ExternalAssetLocation
  ExternalAssetCDN      :: ExternalAssetLocation
  deriving Show

-- | Typeclass for assets which have retrievable details, depending on asset location.
class HasAssetDetails a where
  getAssetDetails :: ExternalAssetLocation -> ExternalAssetDetails

  getAssetUrl :: ExternalAssetLocation -> T.Text
  getAssetUrl = assetUrl . getAssetDetails @a

  getAssetIntegrity :: ExternalAssetLocation -> T.Text
  getAssetIntegrity = assetIntegrity . getAssetDetails @a


data PureCss where
  PureCss :: PureCss

instance HasAssetDetails PureCss where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/css/pure/pure-min@3.0.0.css")
                                                   "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/pure-min.css"
                                                   "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls"


data GridJS where
  GridJS :: GridJS

instance HasAssetDetails GridJS where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/js/gridjs/gridjs.umd.js")
                                                   "sha384-y62I+ZvjNRolkugL/AMpUZykqrL6oqYxBruObmlDAhDpmapM0s+xgvVK+wGVpza0"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://cdn.jsdelivr.net/npm/gridjs/dist/gridjs.umd.js"
                                                   "sha384-y62I+ZvjNRolkugL/AMpUZykqrL6oqYxBruObmlDAhDpmapM0s+xgvVK+wGVpza0"

data MermaidCss where
  MermaidCSS :: MermaidCss

instance HasAssetDetails MermaidCss where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/css/mermaid/mermaid.min.css")
                                                   "sha384-i8iPOOXHyYKlqvjJjbORq7m/VrfUhgupTg3IZvtXz8M7c0CiTPUUhM5gdjiQiGbv"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://cdn.jsdelivr.net/npm/gridjs/dist/theme/mermaid.min.css"
                                                   "sha384-i8iPOOXHyYKlqvjJjbORq7m/VrfUhgupTg3IZvtXz8M7c0CiTPUUhM5gdjiQiGbv"
