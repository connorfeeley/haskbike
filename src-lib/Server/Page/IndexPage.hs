{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.IndexPage
     ( IndexPage (..)
     ) where

import           Lucid

import           Prelude        hiding ( null )

import           Servant

import           Server.Classes
import           Server.PureCSS


data IndexPage where
  IndexPage :: { _indexStaticLink :: Link } -> IndexPage

instance ToHtml IndexPage where
  toHtmlRaw = toHtml
  toHtml _params = do
    -- script_ [src_ ("/" <> toUrlPiece (_staticLink params) <> "/js/station-list.js"), async_ mempty] ""
    div_ [class_ "header"] $ do
      h1_ [] (toHtml "Home")
    div_ [class_ "content pure-g", style_ "justify-content: center;"] $ do
      div_ [class_ "pure-u-1 pure-u-md-2-3"] $ do
        contentSubhead "About"
        p_ "I started collecting station-level data from Toronto's Bike Share API so that I could get a historical view of the system's usage. I've been collecting data about every station in the network roughly every 30s, since September 24th, 2023." <> br_ []
        p_ "Fundamentally, the data is a time series of the number of bikes (broken down by the type of each bike) available at each station, as well as the number of docks available and disabled at each station." <> br_ []
        div_ [] $ do
          p_ $ "This project was borne out of my frustration for why " <> a_ [href_ "/visualization/station-status?station-id=7001&start-time=2023-09-24T00%3A00"] "Wellesley Station" <> " has had a complement of at least 9 - often more - disabled e-bikes ever since the station was converted to a charging station a few months ago."
          p_ [style_ "font-style: italic"] "Click the link to see the last 24 hours of data for Wellesley Station."
          p_ [style_ "font-style: italic"] "Play around with the date ranges to "
          p_ "http://localhost:8081/visualization/station-status?station-id=7001&start-time=2023-10-19T12%3A00&end-time=2023-10-20T18%3A00"


        contentSubhead "Source Code"
        p_ $
          a_ [href_ "https://github.com/connorfeeley/haskbike"] "GitHub: connorfeeley/haskbike"

        contentSubhead "Report an issue"
        p_ $
          a_ [href_ "https://github.com/connorfeeley/haskbike/issues"] "On GitHub"
        p_ $
          a_ [href_ "mailto:bikes@cfeeley.org"] "Over e-mail"

instance ToHtmlComponents IndexPage where
  toMenuHeading _ = menuHeading "/" "Home"
