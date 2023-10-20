{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.StationList
     ( StationList (..)
     ) where



import           Data.Maybe                            ( fromMaybe )
import           Data.Text

import           Database.BikeShare.StationInformation

import           Graphics.Vega.VegaLite                ( dataColumn )

import           Lucid
import           Lucid.Base                            ( makeAttribute )

import           Servant

import           Server.PureCSS

import           TextShow



data StationList where
  StationList :: { _stationList :: [StationInformation]
                 , _staticLink :: Link
                 , _visualizationPageLink :: Link
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
      form_ [class_ "pure-form pure-form-stacked", target_ (toUrlPiece (_visualizationPageLink params))] $ fieldset_ $ do
        legend_ "Select station"
        div_ [class_ "pure-g"] $ do
          datalist_ [id_ "station-list"] $ do
            mapM_ (\station -> option_ [value_ (showt (_infoStationId station)), label_ (_infoName station)] (toHtml (_infoName station))) (_stationList params)
          -- Station type radio inputs
          div_ [class_ "pure-u-1-3 pure-u-md-2-3"] $ do
            label_ [for_ "station-type-radio-all", class_ "pure-radio"] "All" <>
              input_ [id_ "station-type-radio-all", type_ "radio", name_ "station-type", value_ "all", checked_]
            label_ [for_ "station-type-radio-regular", class_ "pure-radio"] "Regular" <>
              input_ [id_ "station-type-radio-regular", type_ "radio", name_ "station-type", value_ "regular"]
            label_ [for_ "station-type-radio-charging", class_ "pure-radio"] "Charging" <>
              input_ [id_ "station-type-radio-charging", type_ "radio", name_ "station-type", value_ "charging"]
          div_ [class_ "pure-u-1-3 pure-u-md-2-3"] $ do
            label_ [for_ "station-filter-input"] "Station Name"
            input_ [id_ "station-filter-input", class_ "pure-input-1-2", type_ "search"]
      table_ [id_ "station-list-table", class_ "pure-table pure-table-horizontal pure-table-striped"] $ do
        thead_ [] $ tr_ $ do
          th_ [id_ "station-id-col"] "ID"
          th_ [id_ "station-name-col"] "Name"
          th_ [id_ "station-type-col", style_ "text-align: center"] "Type"
          th_ [id_ "station-capacity-col", style_ "text-align: center"] "Capacity"
          th_ [id_ "station-address-col"] "Address"
        tbody_ [] $ do
          mapM_ (\station -> tr_ $ do
                  td_ [columnId_ "station-id-col"] (toHtml (showt (_infoStationId station)))
                  td_ [columnId_ "station-name-col"] (toHtml (_infoName station))
                  td_ [columnId_ "station-type-col", style_ "text-align: center"] (stationTypeText station)
                  td_ [columnId_ "station-capacity-col", style_ "text-align: center"] (toHtml (showt (_infoCapacity station)))
                  td_ [columnId_ "station-address-col"] (toHtml (fromMaybe "" (_infoAddress station)))
                ) (_stationList params)

columnId_ :: Text -> Attribute
columnId_ = makeAttribute "data-column-id"

-- stationTypeText :: StationInformation -> Text
stationTypeText :: Monad m => StationInformation -> HtmlT m ()
stationTypeText station = case _infoIsChargingStation station of
  False -> span_ (toHtml "Regular")
  True  -> span_ [style_ "font-weight: bold"] (toHtml "Charging")
