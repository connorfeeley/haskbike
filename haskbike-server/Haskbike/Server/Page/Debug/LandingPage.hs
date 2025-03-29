-- |

module Haskbike.Server.Page.Debug.LandingPage
     ( DebugLandingPage (..)
     ) where

import           Lucid


-- * Debug landing page.
data DebugLandingPage where
  DebugLandingPage :: {  } -> DebugLandingPage
  deriving (Show, Eq)

instance ToHtml DebugLandingPage where
  toHtmlRaw = toHtml
  toHtml _params = do
    div_ "Debug landing page"
