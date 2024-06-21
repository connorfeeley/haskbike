{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the data types used to render the station status visualization page.

module Haskbike.Server.Page.List.StationList
     ( StationList (..)
     ) where

import           Data.Time

import qualified Haskbike.Database.Tables.StationInformation as DB
import qualified Haskbike.Database.Tables.StationOccupancy   as DB
import qualified Haskbike.Database.Tables.StationStatus      as DB
import           Haskbike.Server.Classes
import           Haskbike.Server.Page.List.Common
import           Haskbike.Server.Page.SelectionForm
import           Haskbike.Server.Routes.Static

import           Lucid

import           Prelude                                     hiding ( null )

import           Servant


-- - ---------------------------------------------------------------------------
-- * Common type, type class, and instances.

-- | Parametric station list type.
data StationList a where
  StationList :: { _stationList           :: a
                 , _stationTimeRange      :: (Maybe LocalTime, Maybe LocalTime)
                 , _stationListInputs     :: [SelectionFormInput]
                 , _visualizationPageLink :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> Link
                 } -> StationList a


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
  div_ [class_ "gridjs-table-too-small", style_ "display: none"]
    "Screen too small to display table. Rotate your device or resize your browser window."
  div_ [id_ "station-list-table", class_ "station-list-table"] mempty


-- - ---------------------------------------------------------------------------
-- * Regular station list instances.

instance HasGridJs (StationList [(DB.StationInformation, DB.StationStatus)]) where
  pageScript _page = do
    -- Station list JavaScript.
    script_ [src_ ("/" <> toUrlPiece (staticApi staticRoutesLinks) <> "/js/station-list-table.js"), defer_ mempty] ""

    script_ [src_ ("/" <> toUrlPiece (staticApi staticRoutesLinks) <> "/js/station-list.js"), defer_ mempty] ""

instance HasGridJs (StationList [(DB.StationInformation, DB.StationStatus)]) =>
         ToHtmlComponents (StationList [(DB.StationInformation, DB.StationStatus)]) where
  pageAnchor _ = "#station-list"
  pageName   _ = "Station List"
  toHead       = pageHead


-- - ---------------------------------------------------------------------------
-- * Station occupancy list instances.

instance HasGridJs (StationList [(DB.StationInformation, DB.StationStatus, DB.EmptyFull)]) where
  pageScript _page = do
    -- Station list JavaScript.
    script_ [src_ ("/" <> toUrlPiece (staticApi staticRoutesLinks) <> "/js/station-list-table.js"), defer_ mempty] ""

    script_ [src_ ("/" <> toUrlPiece (staticApi staticRoutesLinks) <> "/js/station-occupancy.js"), defer_ mempty] ""

instance HasGridJs (StationList [(DB.StationInformation, DB.StationStatus, DB.EmptyFull)]) =>
         ToHtmlComponents (StationList [(DB.StationInformation, DB.StationStatus, DB.EmptyFull)]) where
  pageAnchor _ = "#station-occupancy"
  pageName   _ = "Station Occupancy"
  pageTitle  _ = "Station List with Occupancy Time"
  toHead       = pageHead
