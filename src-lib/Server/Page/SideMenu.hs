{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Server.Page.SideMenu where

import           Data.Text

import           Lucid

import           Server.Page.Utils

navLink :: Monad m => Text -> Text -> HtmlT m ()
navLink href text =
  li_ [class_ "pure-menu-item"] $
    a_ [href_ href, class_ "pure-menu-link"] (toHtml text)

navLinkDivided :: Monad m => HtmlT m ()
navLinkDivided =
  li_ [class_ "pure-menu-item menu-item-divided pure-menu-selected"] $
    a_ [class_ "pure-menu-link", href_ "#"] (toHtml "Services")

contentSubhead :: Monad m => String -> HtmlT m ()
contentSubhead text =
  h2_ [class_ "content-subhead"] (toHtml text)

contentParagraph :: Monad m => String -> HtmlT m ()
contentParagraph text =
  p_ [] (toHtml text)

image :: Monad m => Text -> Text -> HtmlT m ()
image src alt =
  div_ [class_ "pure-u-1-4"] $
    img_ [class_ "pure-img-responsive", src_ src, alt_ alt]

main :: IO ()
main = renderToFile "try.html" $ do
  head_ $ do
    makeHeadElements "/static"
    stylesheet_ "/static/css/pure/side-menu.css"
  div_ [id_ "layout"] $ do
    a_ [href_ "#menu", id_ "menuLink", class_ "menu-link"] $
      span_ mempty
    div_ [id_ "menu"] $ do
      div_ [class_ "pure-menu"] $ do
        a_ [class_ "pure-menu-heading", href_ "#company"](toHtml "Availability")
        ul_ [class_ "pure-menu-list"] $ do
          navLink "#home" "Home"
          navLink "#about" "About"
          navLinkDivided
          navLink "#contact" "Contact"
    div_ [id_ "main"] $ do
      div_ [class_ "header"] $ do
        h1_ [] (toHtml "Page Title")
        h2_ [] (toHtml "A subtitle for your page goes here")
      div_ [class_ "content"] $ do
        contentSubhead "How to use this layout"
        contentParagraph "To use this layout..."
        contentSubhead "Now Let's Speak Some Latin"
        contentParagraph "Lorem ipsum dolor sit amet..."
        div_ [class_ "pure-g"] $ do
          image "http://farm3.staticflickr.com/2875/9069037713_1752f5daeb.jpg" "Peyto Lake"
          image "http://farm3.staticflickr.com/2813/9069585985_80da8db54f.jpg" "Train"
          image "http://farm6.staticflickr.com/5456/9121446012_c1640e42d0.jpg" "T-Shirt Store"
          image "http://farm8.staticflickr.com/7357/9086701425_fda3024927.jpg" "Mountain"
        contentSubhead "Try Resizing your Browser"
        contentParagraph "Lorem ipsum dolor sit amet..."

data PureSideMenu a where
  PureSideMenu :: (ToHtml a) => { visPageParams :: a
                                } -> PureSideMenu a

instance (ToHtml a) => ToHtml (PureSideMenu a) where
  toHtmlRaw = toHtml
  toHtml params = do
    head_ $ do
      makeHeadElements "/static"
      stylesheet_ "/static/css/pure/side-menu.css"
    div_ [id_ "layout"] $ do
      a_ [href_ "#menu", id_ "menuLink", class_ "menu-link"] $
        span_ mempty
      div_ [id_ "menu"] $ do
        div_ [class_ "pure-menu"] $ do
          a_ [class_ "pure-menu-heading", href_ "#company"](toHtml "Company")
          ul_ [class_ "pure-menu-list"] $ do
            navLink "#home" "Home"
            navLink "#about" "About"
            navLinkDivided
            navLink "#contact" "Contact"
      div_ [id_ "main"] $ do
        div_ [class_ "header"] $ do
          h1_ [] (toHtml "Title")
          h2_ [] (toHtml "Subtitle")
        div_ [class_ "content"] $ do
          contentSubhead "Content subhead"
          contentParagraph "Content paragraph"
          toHtml (visPageParams params)
          -- contentSubhead "Now Let's Speak Some Latin"
          -- contentParagraph "Lorem ipsum dolor sit amet..."
          -- div_ [class_ "pure-g"] $ do
          --   image "http://farm3.staticflickr.com/2875/9069037713_1752f5daeb.jpg" "Peyto Lake"
          --   image "http://farm3.staticflickr.com/2813/9069585985_80da8db54f.jpg" "Train"
          --   image "http://farm6.staticflickr.com/5456/9121446012_c1640e42d0.jpg" "T-Shirt Store"
          --   image "http://farm8.staticflickr.com/7357/9086701425_fda3024927.jpg" "Mountain"
          -- contentSubhead "Try Resizing your Browser"
          -- contentParagraph "Lorem ipsum dolor sit amet..."
