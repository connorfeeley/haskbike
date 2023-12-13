-- | Component for displaying the latest queries.

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
  toHtml params = div_ [class_ "menu-footer-element"] $
    h3_ [class_ "menu-heading latest-updated-header"] "Latest queries" >>
    forM_ (Map.toList (unLatestQueries params)) (uncurry endpointElement)


endpointElement :: Monad m => EndpointQueried -> LocalTime -> HtmlT m ()
endpointElement ep time =
  p_ [class_ "pure-g latest-updated"] $ title <> content
  where style = style_ "padding-left: 1em;"
        title = b_ [class_ "pure-u-1-2"] ((toHtml . endpointName) ep)
        content = span_ [class_ "pure-u-1-2", style] ((toHtml . formatTimeHtml) time)


endpointName :: EndpointQueried -> T.Text
endpointName ep = case ep of
  StationInformationEP -> "Station information"
  StationStatusEP      -> "Station status"
  SystemInformationEP  -> "System  information"


formatTimeHtml :: LocalTime -> T.Text
formatTimeHtml = T.pack . formatTime defaultTimeLocale shortTimeFormat


-- Short month name, day, hours-minutes-seconds
shortTimeFormat :: String
shortTimeFormat = "%b %d %H:%M:%S"
