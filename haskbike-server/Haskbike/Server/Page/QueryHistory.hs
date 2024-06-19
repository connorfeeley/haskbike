-- | This module provides the query history page.

module Haskbike.Server.Page.QueryHistory
     ( QueryHistoryPage (..)
     ) where

import qualified Data.Text                               as T

import           Haskbike.Server.Classes                 ( ToHtmlComponents (..) )
import           Haskbike.Server.Components.QueryHistory

import           Lucid


data QueryHistoryPage where
  QueryHistoryPage :: { } -> QueryHistoryPage

instance ToHtml QueryHistoryPage where
  toHtmlRaw = toHtml
  toHtml _ = do
    div_ $ do
      h1_ "Query History"
      p_ "This page shows the history of API queries made to the Toronto Bike Share API."
    toHtml QueryHistoryComponent

instance ToHtmlComponents QueryHistoryPage where
  pageName :: QueryHistoryPage -> T.Text
  pageName _ = "Query History"
  pageAnchor :: QueryHistoryPage -> T.Text
  pageAnchor _ = "#query-history"
