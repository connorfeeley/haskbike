-- |

module Server.Classes
     ( ToHtmlComponents (..)
     , menuHeading
     ) where

import           Data.Text

import           Lucid


-- | Can be converted to HTML.
class ToHtmlComponents a where
  toMenuHeading :: Monad m => a -> HtmlT m ()

  toHead        :: Monad m => a -> HtmlT m ()
  toHead _      = mempty

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
