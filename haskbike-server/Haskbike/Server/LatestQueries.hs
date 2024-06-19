-- | Component for displaying the latest queries.

module Haskbike.Server.LatestQueries
     ( LatestQueries (..)
     , endpointElement
     , endpointElementTemplate
     , getLatestQueries
     , latestQueryLogsToMap
     ) where

import           Control.Monad                      ( forM_ )
import           Control.Monad.Catch                ( MonadCatch )

import qualified Data.Map                           as Map
import qualified Data.Text                          as T
import           Data.Time
import           Data.Time.Extras

import           Database.Beam                      hiding ( div_ )

import           Haskbike.Database.EndpointQueried
import           Haskbike.Database.Expressions
import           Haskbike.Database.Tables.QueryLogs
import           Haskbike.ServerEnv

import           Lucid


data LatestQueries where
  LatestQueries :: { unLatestQueries :: Map.Map EndpointQueried LocalTime
                   } -> LatestQueries


instance ToHtml LatestQueries where
  toHtmlRaw = toHtml
  toHtml params = div_ [class_ "menu-footer-element"] $
    h3_ [class_ "menu-heading latest-updated-header"] "Last Updated" >>
    forM_ (Map.toList (unLatestQueries params)) (uncurry endpointElement)


-- | Templated LatestQueries element with concrete data.
endpointElement :: Monad m => EndpointQueried -> LocalTime -> HtmlT m ()
endpointElement ep t = endpointElementTemplate ep (Just t)


-- | Template for LatestQueries element. Used by HTMX component to pre-render the endpoint elements.
endpointElementTemplate :: Monad m => EndpointQueried -> Maybe LocalTime -> HtmlT m ()
endpointElementTemplate ep t =
  div_ [class_ "pure-g latest-updated"] $ title <> content
  where
    title = b_ [class_ "latest-updated-endpoint"] ((toHtml . endpointName) ep)
    content = span_ [class_ "latest-updated-time"] (maybe mempty (toHtml . formatTimeHtml) t)


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

latestQueryLogsToMap :: TimeZone -> [QueryLog] -> LatestQueries
latestQueryLogsToMap tz = LatestQueries . queryMap
  where
    queryMap = Map.fromList . map (\q -> (_queryLogEndpoint q, (utcToLocal . _queryLogTime) q))
    utcToLocal = utcToLocalTime tz

getLatestQueries :: (HasEnv env m, MonadIO m, MonadCatch m) => m LatestQueries
getLatestQueries = do
  tz <- getTz
  latest <- withPostgres $ runSelectReturningList $ selectWith queryLatestQueryLogs
  pure $ latestQueryLogsToMap tz latest
