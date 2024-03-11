{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.List.StationEmptyFullList
     ( StationList (..)
     ) where

import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationStatus

import           Lucid

import           Servant

import           Server.Classes
import           Server.Data.EmptyFullData
import           Server.Page.List.Common
import           Server.Page.List.StationList
import           Server.Page.Utils


instance ToHtml (StationList [(StationInformation, StationStatus, EmptyFull)]) where
  toHtmlRaw = toHtml
  toHtml params = do
    div_ [class_ "header"] $ do
      h1_ [] (toHtml "Station Empty/Full List")
    div_ [class_ "content"] $ do
      toHtml (StationListForm (_stationListInputs params))
      toHtml (toStationEmptyFullTable params)

-- | Table displaying station information.
toStationEmptyFullTable :: Monad m => StationList [(StationInformation, StationStatus, EmptyFull)] -> HtmlT m ()
toStationEmptyFullTable _ = do
  div_ [id_ "station-list-table"] mempty

instance ToHtmlComponents (StationList [(StationInformation, StationStatus, EmptyFull)]) where
  toMenuHeading _ = menuHeading "#station-empty-full" "Station Empty/Full"
  toHead params = do

    -- GridJS
    script_ [src_ "https://cdn.jsdelivr.net/npm/gridjs/dist/gridjs.umd.js"] ""
    stylesheet_ "https://cdn.jsdelivr.net/npm/gridjs/dist/theme/mermaid.min.css"

    -- Station list JavaScript.
    script_ [src_ ("/" <> toUrlPiece (_staticLink params) <> "/js/station-list.js")] ""
