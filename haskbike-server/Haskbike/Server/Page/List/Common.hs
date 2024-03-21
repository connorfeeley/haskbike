-- |

module Haskbike.Server.Page.List.Common
     ( StationListForm (..)
     , columnId_
     , extraText
     , fromBool
     , stationIdLink
     , stationTypeText
     ) where

import           Data.Maybe                                   ( catMaybes )
import           Data.Text
import qualified Data.Text                                    as T
import           Data.Time

import           Haskbike.Database.Tables.StationInformation

import           Lucid

import           Prelude                                      hiding ( null )

import           Servant

import           Haskbike.Server.Page.SelectionForm
import           Haskbike.Server.Page.Utils

import           TextShow


columnId_ :: T.Text -> Attribute
columnId_ = mkData_ "column-id"

fromBool :: T.Text -> Bool -> Maybe T.Text
fromBool t b = if b then Just t else Nothing

stationTypeText :: Monad m => StationInformation -> HtmlT m ()
stationTypeText station = stationTypeSpan <> extraSpan
  where
    charging  = _infoIsChargingStation  station
    valet     = _infoIsValetStation     station
    virtual   = _infoIsVirtualStation   station
    extra     = extraText [fromBool "valet" valet, fromBool "virtual" virtual]
    extraSpan       = span_ (toHtml extra)
    chargingSpan    = span_ [style_ "font-weight: bold"] (toHtml ("Charging" :: String))
    regularSpan     = span_ (toHtml ("Regular" :: String))
    stationTypeSpan = if charging then chargingSpan else regularSpan

extraText :: [Maybe Text] -> Text
extraText optValues =
  if null intercalated
  then ""
  else " (" <> intercalated <> ")"
  where intercalated = intercalate ", " (catMaybes optValues)

stationIdLink :: Monad m => (Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> Link) -> StationInformation -> Maybe LocalTime -> Maybe LocalTime -> HtmlT m ()
stationIdLink baseLink params start end = a_ [href] (toHtml (showt (_infoStationId params)))
  where link = baseLink (Just (fromIntegral (_infoStationId params) :: Int)) start end
        href = href_ ("/" <> toUrlPiece link)

-- | Form use to select station information filter parameters.
data StationListForm where
  StationListForm :: { _stationListFormInputs    :: [SelectionFormInput]
                     } -> StationListForm

instance ToHtml StationListForm where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Selection form
    toHtml (SelectionForm "Filter stations by type, name, ID, or address" (_stationListFormInputs params))
