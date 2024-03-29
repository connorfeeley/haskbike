{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines the data types used to render the station status visualization page.

module Haskbike.Server.Page.List.StationList
     ( StationList (..)
     ) where

import           Data.Time

import           Haskbike.Database.Tables.StationInformation
import           Haskbike.Database.Tables.StationStatus
import           Haskbike.Server.Classes
import           Haskbike.Server.Data.EmptyFullData
import           Haskbike.Server.Page.List.Common
import           Haskbike.Server.Page.SelectionForm
import           Haskbike.Server.Page.Utils                  ( stylesheet_ )

import           Lucid

import           Prelude                                     hiding ( null )

import           Servant


data StationList a where
  StationList :: { _stationList           :: a
                 , _stationTimeRange      :: (Maybe LocalTime, Maybe LocalTime)
                 , _staticLink            :: Link
                 , _stationListInputs     :: [SelectionFormInput]
                 , _visualizationPageLink :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> Link
                 } -> StationList a


instance ToHtml (StationList [(StationInformation, StationStatus)]) where
  toHtmlRaw = toHtml
  toHtml params = do
    div_ [class_ "header"] $ do
      h1_ [] (toHtml "Station List")
    div_ [class_ "content"] $ do
      toHtml (StationListForm (_stationListInputs params))
      toHtml (toStationListTable params)

instance ToHtmlComponents (StationList [(StationInformation, StationStatus)]) where
  toMenuHeading _ = menuHeading "#station-list" "Station List"
  toHead params = do
    -- GridJS
    script_ [src_ "https://cdn.jsdelivr.net/npm/gridjs/dist/gridjs.umd.js", defer_ mempty] ""
    stylesheet_ "https://cdn.jsdelivr.net/npm/gridjs/dist/theme/mermaid.min.css" [defer_ mempty]

    -- Station list JavaScript.
    script_ [src_ ("/" <> toUrlPiece (_staticLink params) <> "/js/station-list-table.js"), defer_ mempty] ""
    script_ [src_ ("/" <> toUrlPiece (_staticLink params) <> "/js/station-list.js"), defer_ mempty] ""


-- | Table displaying station info, status, etc.
toStationListTable :: Monad m => StationList a -> HtmlT m ()
toStationListTable _ = do
  div_ [id_ "station-list-table-too-small", style_ "display: none"]
    "Screen too small to display table. Rotate your device or resize your browser window."
  div_ [id_ "station-list-table", class_ "station-list-table"] mempty


-- * Station empty/full list.

instance ToHtml (StationList [(StationInformation, StationStatus, EmptyFull)]) where
  toHtmlRaw = toHtml
  toHtml params = do
    div_ [class_ "header"] $ do
      h1_ [] (toHtml "Station Occupancy")
    div_ [class_ "content"] $ do
      toHtml (StationListForm (_stationListInputs params))
      toHtml (toStationListTable params)

instance ToHtmlComponents (StationList [(StationInformation, StationStatus, EmptyFull)]) where
  toMenuHeading _ = menuHeading "#station-occupancy" "Station Occupancy"
  toHead params = do

    -- GridJS
    script_ [src_ "https://cdn.jsdelivr.net/npm/gridjs/dist/gridjs.umd.js", defer_ mempty] ""
    stylesheet_ "https://cdn.jsdelivr.net/npm/gridjs/dist/theme/mermaid.min.css" [defer_ mempty]

    -- Station list JavaScript.
    script_ [src_ ("/" <> toUrlPiece (_staticLink params) <> "/js/station-list-table.js"), defer_ mempty] ""
    script_ [src_ ("/" <> toUrlPiece (_staticLink params) <> "/js/station-occupancy.js"), defer_ mempty] ""
