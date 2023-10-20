{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.StationList
     ( StationList (..)
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


data StationList where
  StationList :: { _stationList :: [StationInformation]
                 , _staticLink :: Link
                 , _visualizationPageLink :: Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> Link
                 } -> StationList

instance ToHtml StationList where
  toHtmlRaw = toHtml
  toHtml params = do
    script_ [src_ ("/" <> toUrlPiece (_staticLink params) <> "/js/station-list.js"), async_ mempty] ""
    div_ [class_ "header"] $ do
      h1_ [] (toHtml "Station List")
    div_ [class_ "content"] $ do
      contentSubhead "Select station type"
      toHtml (StationListForm {})
      toHtml (toStationListTable params)

-- | Table displaying station information.
toStationListTable :: Monad m => StationList -> HtmlT m ()
toStationListTable params = do
  table_ [id_ "station-list-table", class_ "pure-table pure-table-horizontal pure-table-striped"] $ do
    thead_ [] $ tr_ $ do
      th_ [id_ "station-id-col"] "ID"
      th_ [id_ "station-name-col"] "Name"
      th_ [id_ "station-type-col", style_ "text-align: center"] "Type"
      th_ [id_ "station-capacity-col", style_ "text-align: center"] "Capacity"
      th_ [id_ "station-address-col"] "Address"
    tbody_ [] $ do
      mapM_ (\station -> tr_ $ do
              td_ [columnId_ "station-id-col"] (stationIdLink (_visualizationPageLink params) station)
              td_ [columnId_ "station-name-col"] (toHtml (_infoName station))
              td_ [columnId_ "station-type-col", style_ "text-align: center"] (stationTypeText station)
              td_ [columnId_ "station-capacity-col", style_ "text-align: center"] (toHtml (showt (_infoCapacity station)))
              td_ [columnId_ "station-address-col"] (toHtml (fromMaybe "" (_infoAddress station)))
            ) (_stationList params)

-- | Form use to select station information filter parameters.
data StationListForm where StationListForm :: { } -> StationListForm

instance ToHtml StationListForm where
  toHtmlRaw = toHtml
  toHtml _params = do
    -- Selection form
    form_ [class_ "pure-form pure-form-stacked"] $ fieldset_ $ do
      legend_ "Filter stations by type, name, ID, or address"
      div_ [class_ "pure-g"] $ do
        -- Station type radio inputs
        div_ [class_ "pure-u-1-2"] $ do
          label_ [for_ "station-type-radio-all", class_ "pure-radio"] $
            input_ [id_ "station-type-radio-all", type_ "radio", name_ "station-type-radio", value_ "all", mkData_ "station-type" "All", checked_] <> span_ "All"

          label_ [for_ "station-type-radio-regular", class_ "pure-radio"] $
            input_ [id_ "station-type-radio-regular", type_ "radio", name_ "station-type-radio", value_ "regular", mkData_ "station-type" "Regular"] <> span_ "Regular"

          label_ [for_ "station-type-radio-charging", class_ "pure-radio"] $
            input_ [id_ "station-type-radio-charging", type_ "radio", name_ "station-type-radio", value_ "charging", mkData_ "station-type" "Charging"] <> span_ "Charging"
        div_ [class_ "pure-u-1-2"] $ do
          label_ [for_ "station-filter-input"] "Filter"
          input_ [id_ "station-filter-input", class_ "pure-input-1-2", type_ "search", placeholder_ "Type a station name, ID, or address"]


columnId_ :: Text -> Attribute
columnId_ = mkData_ "column-id"

fromBool :: Text -> Bool -> Maybe Text
fromBool t b = if b then Just t else Nothing

stationTypeText :: Monad m => StationInformation -> HtmlT m ()
stationTypeText station = stationTypeSpan <> extraSpan
  where
    charging  = _infoIsChargingStation  station
    valet     = _infoIsValetStation     station
    virtual   = _infoIsVirtualStation   station
    extra     = extraText [fromBool "valet" valet, fromBool "virtual" virtual]
    extraSpan       = span_ (toHtml extra)
    chargingSpan    = span_ [style_ "font-weight: bold"] (toHtml "Charging")
    regularSpan     = span_ (toHtml "Regular")
    stationTypeSpan = if charging then chargingSpan else regularSpan

extraText :: [Maybe Text] -> Text
extraText optValues =
  if null intercalated
  then ""
  else "(" <> intercalated <> ")"
  where intercalated = intercalate ", " (catMaybes optValues)

stationIdLink :: Monad m => (Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> Link) -> StationInformation -> HtmlT m ()
stationIdLink baseLink params =
  a_ [href_ ("/" <> toUrlPiece (baseLink (Just (fromIntegral (_infoStationId params) :: Int)) Nothing Nothing))] (toHtml (showt (_infoStationId params)))

instance ToHtmlComponents StationList where
  toMenuHeading _ = menuHeading "#station-list" "Station List"
