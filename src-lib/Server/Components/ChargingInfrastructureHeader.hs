-- | Component for displaying the latest queries.

module Server.Components.ChargingInfrastructureHeader
     ( ChargingInfrastructureHeader (..)
     ) where

import           Lucid

import           Server.Page.Utils


data ChargingInfrastructureHeader where
  ChargingInfrastructureHeader ::
    { chargingStationCount :: Int
    , chargingDockCount    :: Int
    } -> ChargingInfrastructureHeader


instance ToHtml ChargingInfrastructureHeader where
  toHtmlRaw = toHtml

  toHtml params = div_ $ do
    label_ [for_ "charging-infrastructure"] (h3_ "Charging Infrastructure")
    div_ [id_ "charging-infrastructure", class_ "charging-infrastructure"] $ do
      p_ [class_ "pure-g"] $ b_ [class_ "pure-u-3-4"] "Charging Stations: " <>
        span_ [class_ "pure-u-1-4"] stationCount
      p_ [class_ "pure-g"] $ b_ [class_ "pure-u-3-4"] "Charging Docks: " <>
        span_ [class_ "pure-u-1-4"] dockCount

    where
      stationCount = showth . chargingStationCount $ params
      dockCount    = showth . chargingDockCount    $ params
