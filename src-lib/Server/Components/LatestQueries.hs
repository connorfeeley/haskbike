-- |

module Server.Components.LatestQueries
     ( LatestQueries (..)
     ) where

import           Control.Monad                      ( forM_ )

import qualified Data.Map                           as Map
import qualified Data.Text                          as T
import           Data.Time

import           Database.BikeShare.EndpointQueried

import           Lucid


data LatestQueries where
  LatestQueries :: { unLatestQueries :: Map.Map EndpointQueried LocalTime
                   } -> LatestQueries

instance ToHtml LatestQueries where
  toHtmlRaw = toHtml
  toHtml params = forM_ (Map.toList (unLatestQueries params)) divForEndpoint

divForEndpoint :: Monad m => (EndpointQueried, LocalTime) -> HtmlT m ()
divForEndpoint (ep, _time) = div_ [class_ "menu-footer-element"] (foo ep)

foo :: Monad m => EndpointQueried -> HtmlT m ()
foo ep = p_ [class_ "pure-g"] $ b_ [class_ "pure-u-1-2"] ((toHtml . endpointName) ep) <> span_ [class_ "pure-u-1-2"] "Placeholder"

endpointName :: EndpointQueried -> T.Text
endpointName ep = case ep of
  StationInformationEP -> "Station information"
  StationStatusEP      -> "Station status"
  SystemInformationEP  -> "System information"
