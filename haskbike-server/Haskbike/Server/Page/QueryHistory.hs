-- | This module provides the query history page.

module Haskbike.Server.Page.QueryHistory
     ( QueryHistoryComponent (..)
     , QueryHistoryPage (..)
     ) where

import qualified Data.Text                               as T

import           Haskbike.Server.Classes                 ( ToHtmlComponents (..) )
import           Haskbike.Server.Components.QueryHistory

import           Lucid


data QueryHistoryPage where
    QueryHistoryPage :: QueryHistoryComponent -> QueryHistoryPage

instance ToHtml QueryHistoryPage where
  toHtmlRaw = toHtml
  toHtml (QueryHistoryPage component) = do
    div_ $ do
      h1_ "Query History"
      p_ "This page shows the history of API queries made to the Toronto Bike Share API."
    toHtml component

instance ToHtmlComponents QueryHistoryPage where
  pageName :: QueryHistoryPage -> T.Text
  pageName _ = "Query History"
  pageAnchor :: QueryHistoryPage -> T.Text
  pageAnchor _ = "#query-history"
