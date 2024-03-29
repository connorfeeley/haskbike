{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the data types used to render the station status visualization page.

module Haskbike.Server.Page.List.StationList
     ( HasStationListPage (..)
     , StationList (..)
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


-- - ---------------------------------------------------------------------------
-- * Common type, type class, and instances.

-- | Parametric station list type.
data StationList a where
  StationList :: { _stationList           :: a
                 , _stationTimeRange      :: (Maybe LocalTime, Maybe LocalTime)
                 , _staticLink            :: Link
                 , _stationListInputs     :: [SelectionFormInput]
                 , _visualizationPageLink :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> Link
                 } -> StationList a


-- | Type class for pages that display a list of stations.
class HasStationListPage a where
  pageScript :: Monad m => StationList a -> HtmlT m ()

  pageHead :: Monad m => StationList a -> HtmlT m ()
  pageHead page = do
    -- GridJS
    script_ [src_ "https://cdn.jsdelivr.net/npm/gridjs/dist/gridjs.umd.js", defer_ mempty] ""
    stylesheet_ "https://cdn.jsdelivr.net/npm/gridjs/dist/theme/mermaid.min.css" [defer_ mempty]

    -- Station list JavaScript.
    script_ [src_ ("/" <> toUrlPiece (_staticLink page) <> "/js/station-list-table.js"), defer_ mempty] ""
    pageScript page


-- | Define instance used by both regular station list and station occupancy list.
instance ToHtmlComponents (StationList a) => ToHtml (StationList a) where
  toHtmlRaw = toHtml
  toHtml params = do
    div_ [class_ "header"] $ do
      h1_ [] (toHtml (pageTitle params))
    div_ [class_ "content"] $ do
      toHtml (StationListForm Nothing (_stationListInputs params))
      toHtml (toStationListTable params)


-- | Table displaying station info, status, etc. Rendered using GridJS.
toStationListTable :: (ToHtmlComponents (StationList a), Monad m)
                   => StationList a -> HtmlT m ()
toStationListTable _ = do
  div_ [id_ "station-list-table-too-small", style_ "display: none"]
    "Screen too small to display table. Rotate your device or resize your browser window."
  div_ [id_ "station-list-table", class_ "station-list-table"] mempty


-- - ---------------------------------------------------------------------------
-- * Regular station list instances.

instance HasStationListPage [(StationInformation, StationStatus)] where
  pageScript page = script_ [src_ ("/" <> toUrlPiece (_staticLink page) <> "/js/station-list.js"), defer_ mempty] ""

instance HasStationListPage [(StationInformation, StationStatus)] =>
         ToHtmlComponents (StationList [(StationInformation, StationStatus)]) where
  pageAnchor _ = "#station-list"
  pageName   _ = "Station List"
  toHead       = pageHead


-- - ---------------------------------------------------------------------------
-- * Station occupancy list instances.

instance HasStationListPage [(StationInformation, StationStatus, EmptyFull)] where
  pageScript page = script_ [src_ ("/" <> toUrlPiece (_staticLink page) <> "/js/station-occupancy.js"), defer_ mempty] ""

instance HasStationListPage [(StationInformation, StationStatus, EmptyFull)] =>
         ToHtmlComponents (StationList [(StationInformation, StationStatus, EmptyFull)]) where
  pageAnchor _ = "#station-occupancy"
  pageName   _ = "Station Occupancy"
  pageTitle  _ = "Station List with Occupancy Time"
  toHead       = pageHead
