-- | Component for displaying the latest queries.

module Haskbike.Server.LatestQueries
     ( LatestQueries (..)
     , latestQueryLogsToMap
     ) where

import           Control.Monad                      ( forM_ )

import qualified Data.Map                           as Map
import qualified Data.Text                          as T
import           Data.Time

import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Tables.QueryLogs

import           Lucid


data LatestQueries where
  LatestQueries :: { unLatestQueries :: Map.Map EndpointQueried LocalTime
                   } -> LatestQueries


instance ToHtml LatestQueries where
  toHtmlRaw = toHtml
  toHtml params = div_ [class_ "menu-footer-element"] $
    h3_ [class_ "menu-heading latest-updated-header"] "Last Updated" >>
    forM_ (Map.toList (unLatestQueries params)) (uncurry endpointElement)


endpointElement :: Monad m => EndpointQueried -> LocalTime -> HtmlT m ()
endpointElement ep time =
  p_ [class_ "pure-g latest-updated"] $ title <> content
  where title = b_ [class_ "latest-updated-endpoint"] ((toHtml . endpointName) ep)
        content = span_ [class_ "latest-updated-time"] ((toHtml . formatTimeHtml) time)


endpointName :: EndpointQueried -> T.Text
endpointName ep = case ep of
  VersionsEP           -> "Versions"
  VehicleTypesEP       -> "Vehicle types"
  StationInformationEP -> "Station information"
  StationStatusEP      -> "Station status"
  SystemInformationEP  -> "System information"
  SystemRegionsEP      -> "System regions"
  SystemPricingPlansEP -> "System pricing plans"


formatTimeHtml :: LocalTime -> T.Text
formatTimeHtml = T.pack . formatTime defaultTimeLocale shortTimeFormat


-- Short month name, day, hours-minutes-seconds
shortTimeFormat :: String
shortTimeFormat = "%b %d %H:%M:%S"

latestQueryLogsToMap :: TimeZone -> [QueryLog] -> LatestQueries
latestQueryLogsToMap tz = LatestQueries . queryMap
  where
    queryMap = Map.fromList . map (\q -> (_queryLogEndpoint q, (utcToLocal . _queryLogTime) q))
    utcToLocal = utcToLocalTime tz
