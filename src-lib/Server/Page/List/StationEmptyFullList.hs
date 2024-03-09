{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.List.StationEmptyFullList
     ( EmptyFull (..)
     , StationList (..)
     ) where

import           Control.Lens

import           Data.Maybe                                   ( fromMaybe )
import           Data.String                                  ( IsString )
import qualified Data.Text                                    as T
import           Data.Time

import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationStatus

import           Lucid

import           Servant

import           Server.Classes
import           Server.Page.List.Common
import           Server.Page.List.StationList
import           Server.PureCSS

import           TextShow

data EmptyFull where
  EmptyFull :: { _emptyTime :: NominalDiffTime
               , _fullTime  :: NominalDiffTime
               } -> EmptyFull

instance ToHtml (StationList [(StationInformation, StationStatus, EmptyFull)]) where
  toHtmlRaw = toHtml
  toHtml params = do
    div_ [class_ "header"] $ do
      h1_ [] (toHtml "Station Empty/Full List")
    div_ [class_ "content"] $ do
      contentSubhead "Select station type"
      toHtml (StationListForm { _stationListFormSelection = _stationListSelection params })
      toHtml (toStationEmptyFullTable params)

-- | Table displaying station information.
toStationEmptyFullTable :: Monad m => StationList [(StationInformation, StationStatus, EmptyFull)] -> HtmlT m ()
toStationEmptyFullTable params = do
  table_ [id_ "station-list-table", class_ "pure-table pure-table-horizontal pure-table-striped"] $ do
    thead_ [] $ tr_ $ do
      th_ [id_ "station-id-col"] "ID"
      th_ [id_ "station-name-col"] "Name"
      th_ [id_ "station-type-col",         style_ "text-align: center"] "Type"
      th_ [id_ "station-capacity-col",     style_ "text-align: center"] "Capacity"
      th_ [id_ "mechanical-available-col", style_ "text-align: center"] "# Mechanical"
      th_ [id_ "efit-available-col",       style_ "text-align: center"] "# E-Fit"
      th_ [id_ "efit-g5-available-col",    style_ "text-align: center"] "# E-Fit G5"
      th_ [id_ "bikes-disabled-col",       style_ "text-align: center"] "# Bikes Disabled"
      th_ [id_ "docks-disabled-col",       style_ "text-align: center"] "# Docks Disabled"

      th_ [id_ "empty-seconds-col",        style_ "text-align: center"] "Time Empty"
      th_ [id_ "full-seconds-col",         style_ "text-align: center"] "Time Full"

      th_ [id_ "station-address-col"] "Address"
    tbody_ [] $ do
      mapM_ (\(info, status, emptyFull) -> tr_ $ do
              td_ [columnId_ "station-id-col"] (stationIdLink (_visualizationPageLink params) info start end)
              td_ [columnId_ "station-name-col"] (toHtml (_infoName info))
              td_ [columnId_ "station-type-col",         style_ "text-align: center"] (stationTypeText info)
              td_ [columnId_ "station-capacity-col",     style_ "text-align: center"] (toHtml (showt (_infoCapacity info)))
              td_ [columnId_ "mechanical-available-col", style_ "text-align: center"] (toHtml (showt (status ^. vehicleTypesAvailableIconic)))
              td_ [columnId_ "efit-available-col",       style_ "text-align: center"] (toHtml (showt (status ^. vehicleTypesAvailableEfit)))
              td_ [columnId_ "efit-g5-available-col",    style_ "text-align: center"] (toHtml (showt (status ^. vehicleTypesAvailableEfitG5)))
              td_ [columnId_ "bikes-disabled-col",       style_ "text-align: center"] (toHtml (showt (status ^. statusNumBikesDisabled)))
              td_ [columnId_ "docks-disabled-col",       style_ "text-align: center"] (toHtml (showt (status ^. statusNumDocksDisabled)))

              td_ [columnId_ "empty-seconds-col",       style_ "text-align: center"] ((toHtml . formatDiffTime . _emptyTime) emptyFull)
              td_ [columnId_ "full-seconds-col",        style_ "text-align: center"] ((toHtml . formatDiffTime . _fullTime)  emptyFull)

              td_ [columnId_ "station-address-col"] (toHtml (fromMaybe "" (_infoAddress info)))
            ) (_stationList params)
  where (start, end) = _stationTimeRange params

instance ToHtmlComponents (StationList [(StationInformation, StationStatus, EmptyFull)]) where
  toMenuHeading _ = menuHeading "#station-empty-full" "Station Empty/Full"
  toHead params = do

    -- Station list JavaScript.
    script_ [src_ ("/" <> toUrlPiece (_staticLink params) <> "/js/station-list.js"), type_ "module"] ""

-- | Format a 'NominalDiffTime' as 'Text' with a human-readable format.
formatDiffTime :: NominalDiffTime -> T.Text
formatDiffTime dt = (T.pack . formatTime defaultTimeLocale (shortestFormatString dt)) dt

shortestFormatString :: IsString a => NominalDiffTime -> a
shortestFormatString dt =
  case (days, hours, minutes, dt) of -- if dt >= nominalDay then "%dd %Hh %Mm %Ss" else "%Hh %Mm %Ss"
    (0, 0, 0, 0) -> ""                -- empty
    (0, 0, 0, _) -> "%Ss"             -- seconds
    (0, 0, _, _) -> "%Mm %Ss"         -- minutes and seconds
    (0, _, _, _) -> "%Hh %Mm %Ss"     -- hours, minutes, seconds
    (_, _, _, _) -> "%dd %Hh %Mm %Ss" -- days, hours, minutes, seconds
  where
    nominalHour   = secondsToNominalDiffTime (60 * 60)
    nominalMinute = secondsToNominalDiffTime 60
    days    = floor (dt / nominalDay)
    hours   = floor (dt / nominalHour)
    minutes = floor (dt / nominalMinute)
