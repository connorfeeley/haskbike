-- | This module provides the query history page.

module Haskbike.Server.Page.QueryHistory
     ( QueryHistoryComponent (..)
     , QueryHistoryPage (..)
     ) where

import           Haskbike.Server.Classes
import           Haskbike.Server.Components.QueryHistory
import           Haskbike.Server.Routes.Static

import           Lucid

import           Servant


data QueryHistoryPage where
    QueryHistoryPage :: QueryHistoryComponent -> QueryHistoryPage

instance ToHtml QueryHistoryPage where
  toHtmlRaw = toHtml
  toHtml (QueryHistoryPage component) = do
    div_ [class_ "header"] $ do
      h1_ "Query History"
    div_ [class_ "content"] $ do
      p_ "This page shows the history of API queries made to the Toronto Bike Share API."
      toHtml (toQueryHistoryTable component)

instance HasGridJs QueryHistoryPage where
  pageScript :: Monad m => a -> HtmlT m ()
  pageScript _page = do
    -- Station list JavaScript.
    script_ [src_ ("/" <> toUrlPiece (staticApi staticRoutesLinks) <> "/js/query-history-table.js"), defer_ mempty] ("" :: String)

instance ToHtmlComponents QueryHistoryPage where
  pageName _ = "Query History"
  pageAnchor _ = "query-history"
  toHead       = pageHead


-- | Table displaying query history information. Rendered using GridJS.
toQueryHistoryTable :: (ToHtmlComponents QueryHistoryPage, Monad m)
                   => QueryHistoryComponent -> HtmlT m ()
toQueryHistoryTable _ = do
  div_ [class_ "gridjs-table-too-small", style_ "display: none"]
    "Screen too small to display table. Rotate your device or resize your browser window."
  div_ [id_ "query-history-table", class_ "query-history-table"] mempty
