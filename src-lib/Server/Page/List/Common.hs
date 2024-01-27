{-# LANGUAGE DerivingStrategies #-}

-- |

module Server.Page.List.Common
     ( StationListForm (..)
     , StationRadioInputSelection (..)
     , columnId_
     , extraText
     , fromBool
     , inputCheckedIf_
     , stationIdLink
     , stationTypeText
     ) where

import           Control.Lens

import           Data.Maybe                                   ( catMaybes, fromMaybe )
import           Data.Text
import qualified Data.Text                                    as T
import           Data.Time

import           Database.BikeShare.Tables.StationInformation
import           Database.BikeShare.Tables.StationStatus

import           Lucid

import           Prelude                                      hiding ( null )

import           Servant

import           Server.Classes
import           Server.Page.Utils
import           Server.PureCSS

import           TextShow


-- | Which radio button to pre-select.
data StationRadioInputSelection where
  SelectionAll      :: StationRadioInputSelection
  SelectionRegular  :: StationRadioInputSelection
  SelectionCharging :: StationRadioInputSelection
  deriving stock (Eq, Show)

inputCheckedIf_ :: Applicative m => Bool -> [Attribute] -> HtmlT m ()
inputCheckedIf_ cond attrs =
  if cond
  then input_ (checked_ : attrs)
  else input_ attrs


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
    chargingSpan    = span_ [style_ "font-weight: bold"] (toHtml "Charging")
    regularSpan     = span_ (toHtml "Regular")
    stationTypeSpan = if charging then chargingSpan else regularSpan

extraText :: [Maybe Text] -> Text
extraText optValues =
  if null intercalated
  then ""
  else " (" <> intercalated <> ")"
  where intercalated = intercalate ", " (catMaybes optValues)

stationIdLink :: Monad m => (Maybe Int -> Maybe LocalTime -> Maybe LocalTime -> Link) -> StationInformation -> HtmlT m ()
stationIdLink baseLink params =
  a_ [href_ ("/" <> toUrlPiece (baseLink (Just (fromIntegral (_infoStationId params) :: Int)) Nothing Nothing))] (toHtml (showt (_infoStationId params)))

-- | Form use to select station information filter parameters.
data StationListForm where StationListForm :: { _stationListFormSelection :: StationRadioInputSelection } -> StationListForm

instance ToHtml StationListForm where
  toHtmlRaw = toHtml
  toHtml params = do
    -- Selection form
    form_ [class_ "pure-form pure-form-stacked"] $ fieldset_ $ do
      legend_ "Filter stations by type, name, ID, or address"
      div_ [class_ "pure-g"] $ do
        -- Station type radio inputs
        div_ [class_ "pure-u-1-2"] $ do
          label_ [for_ "station-type-radio-all", class_ "pure-radio"] $
            inputCheckedIfSelection_ SelectionAll
              [id_ "station-type-radio-all", type_ "radio", name_ "station-type-radio", value_ "all", mkData_ "station-type" "All"] <> span_ "All"

          label_ [for_ "station-type-radio-regular", class_ "pure-radio"] $
            inputCheckedIfSelection_ SelectionRegular
              [id_ "station-type-radio-regular", type_ "radio", name_ "station-type-radio", value_ "regular", mkData_ "station-type" "Regular"] <> span_ "Regular"

          label_ [for_ "station-type-radio-charging", class_ "pure-radio"] $
            inputCheckedIfSelection_ SelectionCharging
              [id_ "station-type-radio-charging", type_ "radio", name_ "station-type-radio", value_ "charging", mkData_ "station-type" "Charging"] <> span_ "Charging"

        div_ [class_ "pure-u-1-2"] $ do
          label_ [for_ "station-filter-input"] "Filter"
          input_ [id_ "station-filter-input", class_ "pure-input-1-2", type_ "search", placeholder_ "Type a station name, ID, or address"]
    where
      inputCheckedIfSelection_ :: Applicative m => StationRadioInputSelection -> [Attribute] -> HtmlT m ()
      inputCheckedIfSelection_ selection =
        inputCheckedIf_ (_stationListFormSelection params == selection)
