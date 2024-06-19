-- | This module provides the query history component.

module Haskbike.Server.Components.QueryHistory
     ( QueryHistoryComponent (..)
     ) where

import           Data.String                      ( IsString )
import qualified Data.Text                        as T

import           Haskbike.Server.ExternalAssets   ( ExternalAssetLocation (ExternalAssetCDN) )
import           Haskbike.Server.Page.Utils
import           Haskbike.Server.Routes.Debug
import           Haskbike.Server.Routes.QueryLogs

import           Lucid
import           Lucid.Base                       ( makeAttribute )

import           Servant                          ( linkURI )


data QueryHistoryComponent where
  QueryHistoryComponent :: { } -> QueryHistoryComponent

instance ToHtml QueryHistoryComponent where
  toHtmlRaw = toHtml
  toHtml _ = do
    doctype_ -- Disable HTML quirks mode.
    makeHeadElements ExternalAssetCDN "/static"

    div_ [ hx_ "ext" "client-side-templates" ] $
      div_ [ hx_ "trigger" "load"
           , hx_ "get" $ "/" <> (T.pack . show . linkURI) ((allHistory . history . queryApi) debugRoutesLinks Nothing Nothing)
           , makeAttribute "mustache-array-template"  "query-logs-template"
           ] mempty
    mustacheTemplate "query-logs-template" $ do
      mustache "#data"
      div_ $ do
        h3_ [class_ "menu-heading"] "Endpoint: "
        mustache "endpoint" <> " "
      mustache "/data"

mustache :: (ToHtml a, Monad m, Semigroup a, IsString a, a ~ T.Text) => a -> HtmlT m ()
mustache x = toHtmlRaw ("{{" <> x <> "}}")

mustacheTemplate :: Term [Attribute] (a -> b) => T.Text -> a -> b
mustacheTemplate name content = do
  template_ [ id_ name ] content
