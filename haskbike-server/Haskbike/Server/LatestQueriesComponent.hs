-- |

module Haskbike.Server.LatestQueriesComponent
     ( LatestQueriesComponent (..)
     ) where

import           Control.Monad

import qualified Data.Text                         as T

import           Haskbike.Database.EndpointQueried
import           Haskbike.Server.API.Components
import           Haskbike.Server.LatestQueries
import           Haskbike.Server.Page.Utils

import           Lucid

import           Servant                           ( fieldLink, linkURI )


data LatestQueriesComponent where
  LatestQueriesComponent :: { } -> LatestQueriesComponent

instance ToHtml LatestQueriesComponent where
  toHtmlRaw = toHtml
  toHtml _ =
    div_ [ hx_ "trigger" "load"
         , hx_ "get" ("/components/" <> (T.pack . show . linkURI) (fieldLink latestQueries Nothing))
         ] $ -- (img_ [class_ "htmx-indicator htmx-spinner", src_ ("/" <> toUrlPiece (staticLink params) <> "/images/svg-loaders/circles.svg"), alt_ "Loading..."])
      div_ [class_ "menu-footer-element"] $
          h3_ [class_ "menu-heading latest-updated-header"] "Last Updated" >>
          forM_ [(StationInformationEP, Nothing), (StationStatusEP, Nothing), (SystemInformationEP, Nothing)] (uncurry endpointElementTemplate)
