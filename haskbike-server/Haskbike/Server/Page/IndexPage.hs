{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Haskbike.Server.Page.IndexPage
     ( IndexPage (..)
     ) where

import qualified Data.Text               as T
import           Data.Time

import           Haskbike.Server.Classes
import           Haskbike.Server.PureCSS

import           Lucid
import           Lucid.Servant           ( linkHref_ )

import           Prelude                 hiding ( null )

import           Servant


data IndexPage where
  IndexPage :: { _stationStatusLink :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> Link
               , _contactEmail :: String
               } -> IndexPage

instance ToHtml IndexPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- script_ [src_ ("/" <> toUrlPiece (_staticLink params) <> "/js/station-list.js"), defer_ mempty] ""
    div_ [class_ "header"] $ do
      h1_ [] (toHtml "Home")
    div_ [class_ "content"] $ do
      div_ [class_ "pure-u-1 pure-u-md-2-3"] $ do
        contentSubhead "Get Started"
        p_ $ "Pick a station from the " <> a_ [href_ "/visualization/station-list"] "station list" <> ", then select the time range you want to view."

        contentSubhead "About"
        p_ "I started collecting station-level data from Toronto's Bike Share API so that I could get a historical view of the system's usage. I've been collecting data about every station in the network roughly every 30s, since September 24th, 2023." <> br_ []
        br_ []
        p_ "The data available from Toronto's Bike Share API is, primarily:"
        ul_ [class_ "pure-menu-list"] $ do
          li_ [style_ "margin-left: 40px;"] "- the number of available bikes (broken down by the type of each bike) at each station"
          li_ [style_ "margin-left: 40px;"] $ "- the number of disabled bikes (" <> i_ "not" <> " broken down by the type of each bike) at each station"
          li_ [style_ "margin-left: 40px;"] "- the number of available docks at each station"
          li_ [style_ "margin-left: 40px;"] "- the number of disabled docks at each station"
        br_ []
        div_ [] $ do
          p_ $ "This project was borne out of my frustration for why " <> a_ [linkHref_ "/" (_stationStatusLink params (Just 7001) Nothing Nothing)] "Wellesley Station" <> " has had a complement of at least 9 - often more - disabled e-bikes ever since the station was converted to a charging station a few months ago."
          p_ [style_ "font-style: italic; margin-left: 40px"] "Click the link to see the last 24 hours of data for Wellesley Station."
        br_ []
        p_ [style_ "font-style: italic"] ("This is a work-in-progress; see " <> a_ [href_ "https://cfeeley.org/posts/city-stuff/freedom-of-information/"] "my blog" <> " for a series of articles on this.")

        br_ []
        p_ ("I'd be happy send a copy of the database to anyone who's interested - " <> a_ [href_ ("mailto:" <> (T.pack . _contactEmail) params)] "send me an email" <> " and we'll work out the best way to do that.")

        contentSubhead "Source Code"
        p_ $ "GitHub: " <> a_ [href_ "https://github.com/connorfeeley/haskbike"] "connorfeeley/haskbike"

        contentSubhead "Report an issue"
        p_ $
          a_ [href_ "https://github.com/connorfeeley/haskbike/issues"] "On GitHub"
        p_ $
          a_ [href_ ("mailto:" <> (T.pack . _contactEmail) params)] "Over e-mail"

instance ToHtmlComponents IndexPage where
  pageAnchor _ = "/"
  pageName   _ = "Home"
