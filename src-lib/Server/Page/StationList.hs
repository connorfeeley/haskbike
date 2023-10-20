{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.StationList
     ( StationList (..)
     ) where



import           Data.Maybe                            ( fromMaybe )
import           Data.Text
import           Data.Time

import           Database.BikeShare.StationInformation

import           Graphics.Vega.VegaLite                ( dataColumn )

import           Lucid
import           Lucid.Base                            ( makeAttribute )
import           Lucid.Servant

import           Servant

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
    -- Injected into 'SideMenu'
    div_ [class_ "header"] $ do
      h1_ [] (toHtml "Station List")
      -- h2_ [] (toHtml dateHeader)
    div_ [class_ "content"] $ do
      contentSubhead "Select station type"
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

columnId_ :: Text -> Attribute
columnId_ = mkData_ "column-id"

mkData_ :: Text -> Text -> Attribute
mkData_ suffix = makeAttribute ("data-" <> suffix)

-- stationTypeText :: StationInformation -> Text
stationTypeText :: Monad m => StationInformation -> HtmlT m ()
stationTypeText station =
  let valetText = if _infoIsValetStation station then Just "Valet" else Nothing
  in if _infoIsChargingStation station
  then span_ [style_ "font-weight: bold"] (toHtml "Charging")
  else span_ (toHtml "Regular")

stationIdLink :: Monad m => (Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> Link) -> StationInformation -> HtmlT m ()
stationIdLink baseLink params =
  a_ [href_ ("/" <> toUrlPiece (baseLink (Just (fromIntegral (_infoStationId params) :: Int)) Nothing Nothing))] (toHtml (showt (_infoStationId params)))
