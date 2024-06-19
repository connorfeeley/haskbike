-- | This module provides the query history page.

module Haskbike.Server.Page.QueryHistory
     ( QueryHistoryComponent (..)
     ) where


import qualified Data.Text                        as T

import           Haskbike.Server.Classes          ( ToHtmlComponents (..) )
import           Haskbike.Server.Page.Utils
import           Haskbike.Server.Routes.QueryLogs

import           Lucid
import           Lucid.Base                       ( makeAttribute )

import           Servant                          ( fieldLink, linkURI )


data QueryHistoryComponent where
  QueryHistoryComponent :: { } -> QueryHistoryComponent

instance ToHtml QueryHistoryComponent where
  toHtmlRaw = toHtml
  toHtml _ = do
    div_ [ hx_ "ext" "client-side-templates" ] $
      div_ [ hx_ "trigger" "load"
           , hx_ "get" ("/debug/query-logs/history/" <> (T.pack . show . linkURI) (fieldLink allHistory Nothing Nothing))
           , hx_ "get" $ (T.pack . show . linkURI) (fieldLink allHistory Nothing Nothing)
           , makeAttribute "mustache-array-template"  "query-logs-template"
           ] mempty
    template_ [ id_ "query-logs-template"
              ] $ do
      toHtmlRaw $ T.pack "{{#data}}"
      div_ [] $ do
          h3_ [class_ "menu-heading"] "Endpoint: "
          toHtmlRaw $ T.pack "{{endpoint}}"
      toHtmlRaw $ T.pack "{{/data}}"

instance ToHtmlComponents QueryHistoryComponent where
  pageName :: QueryHistoryComponent -> T.Text
  pageName _ = "Test"
  pageAnchor :: QueryHistoryComponent -> T.Text
  pageAnchor _ = "Test"
