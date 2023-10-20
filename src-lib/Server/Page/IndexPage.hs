{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.IndexPage
     ( IndexPage (..)
     ) where

import           Data.Maybe                            ( catMaybes, fromMaybe )
import           Data.Text
import           Data.Time

import           Database.BikeShare.StationInformation

import           Lucid

import           Prelude                               hiding ( null )

import           Servant

import           Server.Classes
import           Server.Page.Utils
import           Server.PureCSS

import           TextShow


data IndexPage where
  IndexPage :: { _indexStaticLink :: Link } -> IndexPage

instance ToHtml IndexPage where
  toHtmlRaw = toHtml
  toHtml params = do
    -- script_ [src_ ("/" <> toUrlPiece (_staticLink params) <> "/js/station-list.js"), async_ mempty] ""
    div_ [class_ "header"] $ do
      h1_ [] (toHtml "Home")
    div_ [class_ "content"] $ do
      contentSubhead "What "

instance ToHtmlComponents IndexPage where
  toMenuHeading _ = menuHeading "/" "Home"
