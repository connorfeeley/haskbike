-- |

module Haskbike.Server.Classes
     ( ToHtmlComponents (..)
     , menuHeading
     ) where

import           Data.Text

import           Haskbike.Server.ExternalAssets ( ExternalAssetLocation )

import           Lucid


-- | Can be converted to HTML.
class ToHtmlComponents a where
  toMenuHeading :: Monad m => a -> HtmlT m ()
  toMenuHeading p = menuHeading (pageAnchor p) (pageName p)

  toHead        :: Monad m => ExternalAssetLocation -> a -> HtmlT m ()
  toHead _      = mempty

  -- | Page name (used in sidebar)
  pageName      :: a -> Text
  -- | Page anchor link (used in sidebar)
  pageAnchor    :: a -> Text
  -- | Page title (used on page itself). Defaults to 'pageName'.
  pageTitle     :: a -> Text
  pageTitle     = pageName

-- -- | @since 2.9.8
-- instance (a ~ (), m ~ Identity) => ToHtmlComponents (HtmlT m a) where
--   toHtml = relaxHtmlT

-- instance ToHtmlComponents String where
--   toHtml    = build . Blaze.fromHtmlEscapedString

-- instance ToHtmlComponents Text where
--   toHtml    = build . Blaze.fromHtmlEscapedText

-- instance ToHtmlComponents LT.Text where
--   toHtml    = build . Blaze.fromHtmlEscapedLazyText
menuHeading :: Monad m => Text -> Text -> HtmlT m ()
menuHeading href title = a_ [class_ "pure-menu-heading", href_ href] (toHtml title)
