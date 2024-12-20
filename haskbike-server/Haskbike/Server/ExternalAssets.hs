{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Types for external assets.

module Haskbike.Server.ExternalAssets
     ( ExternalAssetDetails (..)
     , ExternalAssetLocation (..)
     , GoatCounterAnalytics (..)
     , GridJS (..)
     , HTMX (..)
     , HTMXExtClientSideTemplates (..)
     , HasAssetDetails (..)
     , MermaidCss (..)
     , Mustache (..)
     , PureCss (..)
     , PureCssGrids (..)
     , externalAssetCDN
     , externalAssetVendored
     ) where

import qualified Data.Attoparsec.Text       as A
import           Data.Either                ( fromRight )
import           Data.Functor               ( ($>) )
import qualified Data.Text                  as T

import           Haskbike.Server.API.Static ( staticApiLink )

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
  deriving (Show)


-- | Read instance for 'PopulateStatusChangesOpt'.
instance Read ExternalAssetLocation where
  readsPrec _ = fromRight [] . A.parseOnly parser . T.pack
    where
    parser :: A.Parser [(ExternalAssetLocation, String)]
    parser = A.choice
      [ A.asciiCI externalAssetVendored $> [(ExternalAssetVendored staticApiLink, "")]
      , A.asciiCI externalAssetCDN      $> [(ExternalAssetCDN,  "")]
      ]

externalAssetCDN, externalAssetVendored :: T.Text
externalAssetVendored = "vendored"
externalAssetCDN = "cdn"

-- | Typeclass for assets which have retrievable details, depending on asset location.
class HasAssetDetails a where
  getAssetDetails :: ExternalAssetLocation -> ExternalAssetDetails

  getAssetUrl :: ExternalAssetLocation -> T.Text
  getAssetUrl = assetUrl . getAssetDetails @a

  getAssetIntegrity :: ExternalAssetLocation -> T.Text
  getAssetIntegrity = assetIntegrity . getAssetDetails @a


-- | PureCSS asset details.
data PureCss where
  PureCss :: PureCss

instance HasAssetDetails PureCss where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/css/pure/pure-min@3.0.0.css")
                                                   "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/pure-min.css"
                                                   "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls"


-- | GridJS asset details.
data GridJS where
  GridJS :: GridJS

instance HasAssetDetails GridJS where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/js/gridjs/gridjs.umd.js")
                                                   "sha384-y62I+ZvjNRolkugL/AMpUZykqrL6oqYxBruObmlDAhDpmapM0s+xgvVK+wGVpza0"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://cdn.jsdelivr.net/npm/gridjs/dist/gridjs.umd.js"
                                                   "sha384-y62I+ZvjNRolkugL/AMpUZykqrL6oqYxBruObmlDAhDpmapM0s+xgvVK+wGVpza0"

-- | Mermaid CSS asset details.
data MermaidCss where
  MermaidCSS :: MermaidCss

instance HasAssetDetails MermaidCss where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/css/mermaid/mermaid.min.css")
                                                   "sha384-i8iPOOXHyYKlqvjJjbORq7m/VrfUhgupTg3IZvtXz8M7c0CiTPUUhM5gdjiQiGbv"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://cdn.jsdelivr.net/npm/gridjs/dist/theme/mermaid.min.css"
                                                   "sha384-i8iPOOXHyYKlqvjJjbORq7m/VrfUhgupTg3IZvtXz8M7c0CiTPUUhM5gdjiQiGbv"


-- | PureCSS Grids asset details.
data PureCssGrids where
  PureCssGrids :: PureCssGrids

instance HasAssetDetails PureCssGrids where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/css/pure/pure-grids-responsive-min@3.0.0.css")
                                                   "sha384-zSu8LMfA4z+XX+S+3seN6uTKIYy1Qcfk0dNE+/0SHT5UgOHCnFeHjR16aowIFpII"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/grids-responsive-min.css"
                                                   "sha384-zSu8LMfA4z+XX+S+3seN6uTKIYy1Qcfk0dNE+/0SHT5UgOHCnFeHjR16aowIFpII"


-- | HTMX asset details.
data HTMX where
  HTMX :: HTMX

instance HasAssetDetails HTMX where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/js/htmx/htmx.min.js")
                                                   "sha384-QFjmbokDn2DjBjq+fM+8LUIVrAgqcNW2s0PjAxHETgRn9l4fvX31ZxDxvwQnyMOX"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://unpkg.com/htmx.org@1.9.11"
                                                   "sha384-0gxUXCCR8yv9FM2b+U3FDbsKthCI66oH5IA9fHppQq9DDMHuMauqq1ZHBpJxQ0J0"


-- | HTMX client side templates asset details.
data HTMXExtClientSideTemplates where
  HTMXExtClientSideTemplates :: HTMXExtClientSideTemplates

instance HasAssetDetails HTMXExtClientSideTemplates where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/js/htmx/client-side-templates.js")
                                                   "sha384-QFjmbokDn2DjBjq+fM+8LUIVrAgqcNW2s0PjAxHETgRn9l4fvX31ZxDxvwQnyMOX"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://unpkg.com/htmx-ext-client-side-templates@2.0.0/client-side-templates.js"
                                                   "sha384-q/huQyJ6q5A6n6a7woJDV43/kg0OMiEM9B94VPjZD0VVN5DD2et0L8nAoX05TYVZ"


-- | Mustache template engine asset details.
data Mustache where
  Mustache :: Mustache

instance HasAssetDetails Mustache where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   ("/" <> toUrlPiece static <> "/js/mustache@4.2.0")
                                                   "sha384-w1w4OJZe53/hPslZL3huM7kr/RQ+IXfaVeO5Tx0boUDt0ZTA8dwJ5OjxjpWOtw14"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://unpkg.com/mustache@4.2.0"
                                                   "sha384-w1w4OJZe53/hPslZL3huM7kr/RQ+IXfaVeO5Tx0boUDt0ZTA8dwJ5OjxjpWOtw14"


-- | GoatCounter asset details.
data GoatCounterAnalytics where
  GoatCounterAnalytics :: GoatCounterAnalytics

instance HasAssetDetails GoatCounterAnalytics where
  getAssetDetails (ExternalAssetVendored static) = ExternalAssetDetails (ExternalAssetVendored static)
                                                   "/count.js"
                                                   "sha384-QGgNMMRFTi8ul5kHJ+vXysPe8gySvSA/Y3rpXZiRLzKPIw8CWY+a3ObKmQsyDr+a"
  getAssetDetails ExternalAssetCDN               = ExternalAssetDetails ExternalAssetCDN
                                                   "https://stats.bikes.cfeeley.org/count.js"
                                                   "sha384-QGgNMMRFTi8ul5kHJ+vXysPe8gySvSA/Y3rpXZiRLzKPIw8CWY+a3ObKmQsyDr+a"
