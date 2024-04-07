{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Types for external assets.

module Haskbike.Server.ExternalAssets
     ( ExternalAssetDetails (..)
     , ExternalAssetLocation (..)
     , HasAssetDetails (..)
     , PureCSS (..)
     , SiteAssets (..)
     , allSiteAssets
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

-- | Typeclass for assets which have retrievable details, depending on asset location.
class HasAssetDetails a where
  getAssetDetails :: ExternalAssetLocation -> ExternalAssetDetails


data PureCSS where
  PureCSS :: PureCSS

instance HasAssetDetails PureCSS where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/css/pure/pure-min.css")
                                                   "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/pure-min.css"
                                                   "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls"

data SiteAssets where
  SiteAssets :: { pureCSS :: PureCSS
                } -> SiteAssets

allSiteAssets :: SiteAssets
allSiteAssets = SiteAssets PureCSS
