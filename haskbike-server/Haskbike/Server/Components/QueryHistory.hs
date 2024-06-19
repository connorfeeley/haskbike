-- | This module provides the query history component.

module Haskbike.Server.Components.QueryHistory
     ( QueryHistoryComponent (..)
     ) where

import           Data.String                       ( IsString )
import qualified Data.Text                         as T
import           Data.Time                         ( UTCTime )

import           Haskbike.Database.EndpointQueried
import           Haskbike.Server.ExternalAssets    ( ExternalAssetLocation (ExternalAssetCDN) )
import           Haskbike.Server.Page.Utils
import           Haskbike.Server.Routes.Debug
import           Haskbike.Server.Routes.QueryLogs
import           Haskbike.Server.Routes.Static

import           Lucid
import           Lucid.Base                        ( makeAttribute )

import           Servant                           ( linkURI, toUrlPiece )


data QueryHistoryComponent where
  QueryHistoryComponent ::
    { queryHistEndpoint  :: Maybe EndpointQueried
    , queryHistStartTime :: Maybe UTCTime
    , queryHistEndTime   :: Maybe UTCTime
    } -> QueryHistoryComponent

instance ToHtml QueryHistoryComponent where
  toHtmlRaw = toHtml
  toHtml _ = do
    doctype_ -- Disable HTML quirks mode.
    makeHeadElements ExternalAssetCDN ("/" <> toUrlPiece (staticApi staticRoutesLinks))

    div_ [ hx_ "ext" "client-side-templates" ] $
      div_ [ hx_ "trigger" "load"
           , hx_ "get" $ "/" <> (T.pack . show . linkURI) ((allHistory . history . queryApi) debugRoutesLinks Nothing Nothing)
           , makeAttribute "mustache-array-template"  "query-logs-template"
           ] $ do
        br_ []
        br_ []
        img_ [ class_ "htmx-indicator htmx-spinner"
             , src_ ("/" <> toUrlPiece (staticApi staticRoutesLinks) <> "/images/svg-loaders/circles.svg")
             , alt_ "Loading..."
             ]
    mustacheTemplate "query-logs-template" $ do
      mustache "#data"

      div_ $ do
        h2_ [class_ "menu-heading"] (b_ "Endpoint: " >> mustache "endpoint")
        br_ []

        h3_ [class_ "menu-heading"] "Total Queries"
        p_ $ do
          p_ $ b_ "Total: "            >> mustache "total.num-queries"
          p_ $ b_ "Average interval: " >> mustache "total.avg-interval"
          p_ $ b_ "Latest query: "     >> mustache "total.latest-query"
        br_ []

        h3_ [class_ "menu-heading"] "Successful Queries"
        p_ $ do
          p_ $ b_ "Total: "            >> mustache "successful.num-queries"
          p_ $ b_ "Average interval: " >> mustache "successful.avg-interval"
          p_ $ b_ "Latest query: "     >> mustache "successful.latest-query"
        br_ []

        h3_ [class_ "menu-heading"] "Failed Queries"
        p_ $ do
          p_ $ b_ "Total: "            >> mustache "failed.num-queries"
          p_ $ b_ "Average interval: " >> mustache "failed.avg-interval"
          p_ $ b_ "Latest query: "     >> mustache "failed.latest-query"
        br_ []

        br_ []
        br_ []

      mustache "/data"

mustache :: (ToHtml a, Monad m, Semigroup a, IsString a, a ~ T.Text) => a -> HtmlT m ()
mustache x = toHtmlRaw ("{{" <> x <> "}}")

mustacheTemplate :: Term [Attribute] (a -> b) => T.Text -> a -> b
mustacheTemplate name content = do
  template_ [ id_ name ] content
