-- |

module Haskbike.Server.Page.Debug.LandingPage
     ( DebugLandingPage (..)
     ) where

import           Control.Monad                          ( forM_ )

import           Haskbike.Database.EndpointQueried      ( EndpointQueried (..) )
import           Haskbike.Server.LatestQueries          ( endpointName )
import           Haskbike.Server.Routes.Debug.DebugAPI
import           Haskbike.Server.Routes.Debug.QueryLogs

import           Lucid

import           Servant                                ( ToHttpApiData (toUrlPiece) )


-- * Debug landing page.
data DebugLandingPage where
  DebugLandingPage :: {  } -> DebugLandingPage
  deriving (Show, Eq)

instance ToHtml DebugLandingPage where
  toHtmlRaw = toHtml
  toHtml _params = do
    h2_ $ "Debug landing page"

    h3_ "Miscellaneous"
    ul_ [class_ ""] $ do
      li_ [] $ a_ [ href_ (toUrlPiece (serverVersion debugRoutesLinks))
                  ] "Server version"

    h3_ "Query History"
    ul_ [class_ ""] $ do
      li_ [] $ a_ [ href_ (toUrlPiece ((allHistory . history . queryApi) debugRoutesLinks Nothing Nothing))
                  ] "All endpoint history"
      h4_ "Endpoint-specific history"
      ul_ [class_ ""] $ do
        -- Render link for each endpoint.
        forM_ [(minBound :: EndpointQueried) ..] renderEndpointHistoryLink

    h3_ "Error History"
    ul_ [class_ ""] $ do
      li_ [] $ a_ [ href_ (toUrlPiece ((latestErrors . errorsApi) debugRoutesLinks 1))
                  ] "Latest error"
      li_ [] $ a_ [ href_ (toUrlPiece ((latestErrors . errorsApi) debugRoutesLinks 10))
                  ] "Latest 10 errors"

    where
      renderEndpointHistoryLink :: Monad m => EndpointQueried -> HtmlT m ()
      renderEndpointHistoryLink ep =
        li_ [] $ a_ [ href_ (toUrlPiece ((historyForEndpoint . history . queryApi) debugRoutesLinks ep Nothing Nothing))
                    ] (toHtml (endpointName ep))
