-- |

module Server.PureCSS
     ( contentParagraph
     , contentSubhead
     , image
     , navLink
     , navLinkDivided
     ) where

import           Data.Text

import           Lucid

navLink :: Monad m => Text -> Text -> HtmlT m ()
navLink href text =
  li_ [class_ "pure-menu-item"] $
    a_ [href_ href, class_ "pure-menu-link"] (toHtml text)

navLinkDivided :: Monad m => Text -> Text -> HtmlT m ()
navLinkDivided href text =
  li_ [class_ "pure-menu-item menu-item-divided pure-menu-selected"] $
    a_ [class_ "pure-menu-link", href_ href] (toHtml text)

contentSubhead :: Monad m => Text -> HtmlT m ()
contentSubhead text =
  h2_ [class_ "content-subhead"] (toHtml text)

contentParagraph :: Monad m => Text -> HtmlT m ()
contentParagraph text =
  p_ [] (toHtml text)

image :: Monad m => Text -> Text -> HtmlT m ()
image src alt =
  div_ [class_ "pure-u-1-4"] $
    img_ [class_ "pure-img-responsive", src_ src, alt_ alt]
