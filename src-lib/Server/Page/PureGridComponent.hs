{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | This module defines the data types used to render the station status visualization page.

module Server.Page.PureGridComponent
     ( PureGridComponent (..)
     ) where

import           Data.Aeson
import           Data.Text
import           Data.Time
import           Data.Time.Extras

import           Database.BikeShare.StationInformation

import           Fmt

import qualified Graphics.Vega.VegaLite                 as VL
import           Graphics.Vega.VegaLite.Extra

import           Lucid
import           Lucid.Base                             ( makeAttribute )
import           Lucid.Servant

import           Servant

import           Server.Data.StationStatusVisualization
import           Server.Page.StationStatusVisualization

import           TextShow

import           Visualization.StationOccupancy


data PureGridComponent where
  PureGridComponent :: { visPageParams :: StationStatusVisualizationPage
                       } -> PureGridComponent

instance ToHtml PureGridComponent where
  toHtmlRaw = toHtml
  toHtml params = do
    with div_ [class_ "pure-g"] $ do
      with div_ [class_ "pure-u-1-3"] "Thirds"
      with div_ [class_ "pure-u-1-3"] "Thirds"
      with div_ [class_ "pure-u-1-3"] "Thirds"
